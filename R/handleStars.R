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
#'   test <- compute_budget_stars1year(year_start = 2020, test_stars, fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'  }
#'
compute_budget_stars1year <- function(year_start,
                                      stars_object,
                                      fit_state, fit_MR,
                                      roost_insulation_dTa = 5,
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
                               temp_threshold = temp_threshold, split_summer = split_summer,
                               min_days_trigger_winter = min_days_trigger_winter,
                               threshold_mortality = threshold_mortality,
                               PROGRESS = TRUE)

  ## reshape output
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
#' contained in a stars object. It is an extremelly computationaly intensive step. Note that the
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
#'  test <- compute_budget_stars(test_stars_small, fit_state = fit_normo_cauchit, fit_MR = fit_torpor, nb_cores = 30)
#'  test
#'  }
#'
compute_budget_stars <- function(stars_object,
                                 fit_state, fit_MR,
                                 roost_insulation_dTa = 5,
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

