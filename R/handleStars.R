#' Compute budget for one winter from a stars object
#'
#' This function should not be called by the user. It is called internally by
#' [compute_budget_stars()].
#'
#' @inheritParams arguments
#'
#' @return a stars object with energy budget information
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'   test_stars <- readRDS("../NC/stars/gfdl-esm4_SSP126.rds")
#'   test <- compute_budget_stars1year(year_start = 2020, test_stars,
#'                                     fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'  }
#'
compute_budget_stars1year <- function(year_start,
                                      stars_object,
                                      fit_state, fit_MR,
                                      roost_insulation_dTa = 5,
                                      huddling_factor = 0.5,
                                      temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                                      threshold_mortality = 27) {

  ## extract dates
  print(paste("computing budget for year", year_start))
  all_dates <- stars::st_get_dimension_values(stars_object, "time")
  focal_dates <- all_dates[format(all_dates, "%Y") %in% c(year_start, year_start + 1)]
  if (length(focal_dates) == 0) stop("Missing years in data")

  indices_dates_to_do <- which(all_dates %in% focal_dates)

  ## extract budget at all locations
  stars_obj <- stars::st_apply(stars_object[, , , indices_dates_to_do],
                               MARGIN = 1:2,
                               FUN = compute_budget_summarystats,
                               vec_Dates = focal_dates,
                               fit_state = fit_state, fit_MR = fit_MR,
                               roost_insulation_dTa = roost_insulation_dTa,
                               huddling_factor = huddling_factor,
                               temp_threshold = temp_threshold, split_summer = split_summer,
                               min_days_trigger_winter = min_days_trigger_winter,
                               threshold_mortality = threshold_mortality,
                               PROGRESS = TRUE)

  ## reformat output (see also https://github.com/r-spatial/stars/issues/635)
  data <- do.call("rbind", stars_obj$compute_budget_summarystats)
  stars_obj$compute_budget_summarystats <- NULL
  for (layer in colnames(data)) {
    stars_obj[[layer]] <-  data[, layer]
  }

  sf::st_crs(stars_obj) <- "+proj=longlat +datum=WGS84 +no_defs"

  stars_obj
}


#' Compute summary statistics for all winters from a stars object
#'
#' This function calls [compute_budget_summarystats()] on all winters and across all locations
#' contained in a stars object. It is an extremely computationally intensive step. Note that the
#' progress messages are more informative outside RStudio.
#'
#' @inheritParams arguments
#'
#' @return a stars object with energy budget information
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'  test_stars <- readRDS("../NC/stars/gfdl-esm4_SSP126.rds")
#'  test_stars_small <- dplyr::filter(test_stars, time < as.Date("2017-01-01"), x < -10, y > 70)
#'  test <- compute_budget_stars(test_stars_small,
#'                               fit_state = fit_normo_cauchit, fit_MR = fit_torpor,
#'                               nb_cores = 30)
#'  test
#'  }
#'
compute_budget_stars <- function(stars_object,
                                 fit_state, fit_MR,
                                 roost_insulation_dTa = 5,
                                 huddling_factor = 0.5,
                                 temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                                 threshold_mortality = 27,
                                 nb_cores = 1, lapply_pkg = "pbmcapply") {

  ## infer years to consider as year_start for the extraction of the winter info
  first_year  <- as.numeric(format(min(stars::st_get_dimension_values(stars_object, "time")), "%Y"))
  last_year   <- as.numeric(format(max(stars::st_get_dimension_values(stars_object, "time")), "%Y")) - 1
  years_to_do <- first_year:last_year
  nb_cores <- min(c(length(years_to_do), nb_cores))

  ## prepare parallel computing
  if (nb_cores > 1L && lapply_pkg == "base") message("using the 'base' package does not allow for parallel computing; only 1 CPU core will be used and that means it will take a lot of time (perhaps days) to run till completion...")

  if (lapply_pkg == "pbmcapply" && !requireNamespace("pbmcapply", quietly = TRUE)) {
    message("to run parallel computing using the package {pbmcapply} you need to install this package; since you did not, {parallel} will be used instead.")
    lapply_pkg <- "parallel"
  }

  lapply_fn <- switch(lapply_pkg,
                      parallel = function(...) parallel::mclapply(..., mc.cores = nb_cores, mc.preschedule = FALSE),
                      pbmcapply = function(...) pbmcapply::pbmclapply(..., mc.cores = nb_cores, mc.preschedule = FALSE, mc.style = "txt", mc.substyle = 3),
                      base = function(...) lapply(...)
                      )

  ## extract winter info for each year and locations
  stars_object_filled <- lapply_fn(years_to_do, function(year) {
    compute_budget_stars1year(year_start = year,
                              stars_object = stars_object,
                              fit_state = fit_state, fit_MR = fit_MR,
                              roost_insulation_dTa = roost_insulation_dTa,
                              huddling_factor = huddling_factor,
                              temp_threshold = temp_threshold, split_summer = split_summer, min_days_trigger_winter = min_days_trigger_winter,
                              threshold_mortality = threshold_mortality)})

  stars_object_filled_all_years <- do.call("c", c(stars_object_filled, along = 3))

  if (length(years_to_do) > 1) {
    stars_object_filled_all_years <- stars::st_set_dimensions(stars_object_filled_all_years, 3, values = as.character(years_to_do))
    stars_object_filled_all_years <- stars::st_set_dimensions(stars_object_filled_all_years, names = c("x", "y", "year"))
  }

  sf::st_crs(stars_object_filled_all_years) <- "+proj=longlat +datum=WGS84 +no_defs"

  stars_object_filled_all_years
}


#' Loads, combine and merge winter stars across forcing models
#'
#'
#' @inheritParams arguments
#'
#' @return a stars object
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'  reshape_stars_across.models(directory_stars = "../NC/stars_winter", SSP = "126")
#'  }
#'
reshape_stars_across.models <- function(directory_stars, SSP, varname = "Budget_winter", flatten = TRUE) {
  ##TODO, check that this function works
  all_rds_to_do <- list.files(path = directory_stars, full.names = TRUE, pattern = SSP)
  list_stars_winter <- lapply(all_rds_to_do, \(x) readRDS(x)[varname,])
  stars_winter <- do.call("c", list_stars_winter)
  stars_winter <- merge(stars_winter, name = "forcing_model")
  names(stars_winter) <- varname
  if (flatten) stars_winter <- stars::st_apply(stars_winter, MARGIN = 1:3, FUN = mean, .fname = varname)
  stars_winter
}



#' Extract latitude from a stars object
#'
#' This function should not be called by the user. It is called internally by
#' [summarise_info_winter.stars()].
#'
#' @inheritParams arguments
#'
#' @return the latitude
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'  test_stars <- readRDS("../NC/stars_winter/gfdl-esm4_SSP126_winter.rds")
#'  test_stars_small <- dplyr::filter(test_stars, year > 2080, x < -10, y > 70)
#'  extract_stars_latitude(test_stars_small)
#'  extract_stars_latitude(test_stars_small, fn = "max")
#'  }
#'
extract_stars_latitude <- function(stars_object, year = NULL, name_bool_var = "Survive", mask = NULL, fn = "median") {

  index_bool_var <- which(names(stars_object) == name_bool_var)

  ## check name_bool_var:
  if (!is.logical(as.vector(stars_object[[index_bool_var]]))) {
    stop("The variable set in name_bool_var must point to a boolean (i.e. it should only contain TRUE/FALSE/NA)")
  }

  possible_years <- as.numeric(stars::st_get_dimension_values(stars_object, "year"))

  ## if year not provided, consider all years:
  if (is.null(year)) {
    year <- possible_years
  }

  ## apply mask, if provided:
  if (!is.null(mask)) {
    stars_object <- stars_object[mask, crop = FALSE]
  }

  d <- as.data.frame(stars_object[index_bool_var, , , which(possible_years == year)])
  lat <- d[d[, name_bool_var] & !is.na(d[, name_bool_var]), "y"]

  ## apply the summary function:
  do.call(fn, list(lat))
}



#' Summarise winter information
#'
#' This function should not be called by the user. It is internally called by [summarise_info_winter.stars.all()].
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'  test_stars <- readRDS("../NC/stars_winter/gfdl-esm4_SSP126_winter.rds")
#'  #test_stars_small <- dplyr::filter(test_stars, year > 2080, x < -10, y > 70)
#'  data(lands_polygons, package = "winteR")
#'  test <- summarise_info_winter.stars(test_stars, mask = lands_polygons)
#'  }
#'
summarise_info_winter.stars <- function(stars_object, mask = NULL) {

  ## Apply mask (optional)
  if (!is.null(mask)) {
    stars_object_masked <-  stars_object[mask, crop = FALSE] ## mask defines here what to keep!
  } else {
    stars_object_masked <-  stars_object
  }

  suitability <- data.frame(Year = character(),
                            Suitable_cells = numeric(),
                            Suitable_area_km2 = numeric(),
                            Start_winter = numeric(),
                            Stop_winter = numeric(),
                            Duration_winter = numeric(),
                            Temp_winter_mean = numeric(),
                            Temp_winter_sd = numeric(),
                            Temp_winter_median = numeric(),
                            Temp_winter_min = numeric(),
                            Temp_winter_max = numeric(),
                            Temp_winter_autocorr = numeric(),
                            Budget_winter = numeric())

  ## Count proportion of suitable cells and other metrics (within the mask if applicable)
  for (i in c(1:(with(stars::st_dimensions(stars_object_masked)$year, to - from + 1)))) {

    ## Suitable cells are those for which budget is sufficient and hibernation occurs
    starts_object_suitable <- stars_object_masked["Survive"][, , , i] & stars_object_masked["Duration_winter"][, , , i] > 0

    ## Proportion of suitable cells is the number of suitable cells out of all cells not outside the mask
    cells_prop <- mean(starts_object_suitable[[1]], na.rm = TRUE)

    sf::st_as_sf(starts_object_suitable, merge = TRUE) |>
      dplyr::filter(.data$Survive == 1) |>
      dplyr::mutate(area = units::set_units(sf::st_area(.data$geometry), value = "km^2")) |>
      dplyr::pull(.data$area) |>
      sum() -> suitable_area

    year <- stars::st_get_dimension_values(stars_object_masked[, , , i], "year")
    suitability[i, "Year"]                 <- year
    suitability[i, "Suitable_cells"]       <- as.numeric(cells_prop)
    suitability[i, "Suitable_area_km2"]    <- suitable_area
    suitability[i, "Latitude_mean"]        <- extract_stars_latitude(starts_object_suitable, fn = "mean")
    suitability[i, "Latitude_median"]      <- extract_stars_latitude(starts_object_suitable, fn = "median")
    suitability[i, "Latitude_min"]         <- extract_stars_latitude(starts_object_suitable, fn = "min")
    suitability[i, "Latitude_max"]         <- extract_stars_latitude(starts_object_suitable, fn = "max")
    suitability[i, "Start_winter"]         <- round(mean(as.numeric(stars_object_masked[["Start_winter"]][, , i]), na.rm = TRUE))
    suitability[i, "Stop_winter"]          <- round(mean(as.numeric(stars_object_masked[["Stop_winter"]][, , i]), na.rm = TRUE))
    suitability[i, "Duration_winter"]      <- mean(as.numeric(stars_object_masked[["Duration_winter"]][, , i]), na.rm = TRUE)
    suitability[i, "No_winter"]            <- mean(as.numeric(stars_object_masked[["Duration_winter"]][, , i]) == 0, na.rm = TRUE)
    suitability[i, "Temp_winter_mean"]     <- mean(as.numeric(stars_object_masked[["Temp_winter_mean"]][, , i]), na.rm = TRUE)
    suitability[i, "Temp_winter_sd"]       <- mean(as.numeric(stars_object_masked[["Temp_winter_sd"]][, , i]), na.rm = TRUE)
    suitability[i, "Temp_winter_median"]   <- mean(as.numeric(stars_object_masked[["Temp_winter_median"]][, , i]), na.rm = TRUE)
    suitability[i, "Temp_winter_min"]      <- mean(as.numeric(stars_object_masked[["Temp_winter_min"]][, , i]), na.rm = TRUE)
    suitability[i, "Temp_winter_max"]      <- mean(as.numeric(stars_object_masked[["Temp_winter_max"]][, , i]), na.rm = TRUE)
    suitability[i, "Temp_winter_autocorr"] <- mean(as.numeric(stars_object_masked[["Temp_winter_autocorr"]][, , i]), na.rm = TRUE)
    suitability[i, "Budget_winter"]        <- mean(as.numeric(stars_object_masked[["Budget_winter"]][, , i]), na.rm = TRUE)
  }

  suitability[, "Start_winter"] <- as.Date(suitability[, "Start_winter"], origin = "1970-01-01")
  suitability[, "Stop_winter"]  <- as.Date(suitability[, "Stop_winter"], origin = "1970-01-01")

  suitability
}

#' Summarise winter information
#'
#' This function summarises the information contained in a stars object produced with [compute_budget_stars()].
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'   data(lands_polygons, package = "winteR")
#'   summarise_info_winter.stars.all("../NC/stars_winter", mask = lands_polygons)
#' }
#'
summarise_info_winter.stars.all <- function(directory_stars, mask = NULL) {

  list_winter_stars <- list.files(path = directory_stars, full.names = TRUE, pattern =  ".rds")

  list_info_all_winter_stars <- lapply(list_winter_stars, function(stars) {
    print(paste("Processing winter stars", stars))
    stars_focal <- readRDS(file = stars)
    scenario <- stringr::str_remove_all(stringr::str_extract(basename(stars), pattern = "(.*)(OBSCLIM|SSP.{3})\\1"), pattern = "\\_")
    forcing <- stringr::str_extract(basename(stars), pattern = ".*(?=\\_OBSCLIM|\\_SSP)")
    cbind(summarise_info_winter.stars(stars_object = stars_focal, mask = mask), Forcing = forcing, Scenario = scenario)
  })

  do.call("rbind", list_info_all_winter_stars)
}



#' Build a stars object with summary statistics aggregated across years
#'
#' @inheritParams arguments
#'
#' @return a stars object
#' @export
#'
#' @examples
#' data_OBSCLIM <- readRDS("../NC/stars_winter/gswp3-w5e5_OBSCLIM_winter.rds")
#' build_stars_winter.stats(data_OBSCLIM)
#'
build_stars_winter.stats <- function(stars_object) {

  ## keep only numerical variables (we don't need the others and it would prevent compute_winter_summarystats to work properly)
  stars_object |>
    dplyr::select(dplyr::where(is.numeric)) -> stars_object2

  ## extract summary stats for each metric
  stars_obj <- stars::st_apply(stars_object2,
                               MARGIN = c("x", "y"),
                               FUN = compute_winter_summarystats)

  ## reformat output (see also https://github.com/r-spatial/stars/issues/635)
  metrics <- stars::st_get_dimension_values(stars_obj, "compute_winter_summarystats")

  list_stars <- lapply(metrics, function(metric) {
      stars_metric <- stars_obj[, metric, , drop = TRUE]
      names(stars_metric) <- paste0(names(stars_metric), "_", metric)
      stars_metric
    })

  do.call("c", list_stars)

}

#' Extract summary statistics about winter characteristics
#'
#' @inheritParams arguments
#'
#' @return a data frame
#' @export
#'
#' @examples
#' data_OBSCLIM <- readRDS("../NC/stars_winter/gswp3-w5e5_OBSCLIM_winter.rds")
#' stars::st_apply(data_OBSCLIM[c("Duration_winter", "Temp_winter_mean")],
#'                 MARGIN = c("x", "y"),
#'                 FUN = compute_winter_summarystats)
#'
compute_winter_summarystats <- function(x) {
  c(mean = mean(x, na.rm = TRUE), median = stats::median(x, na.rm = TRUE))
}

