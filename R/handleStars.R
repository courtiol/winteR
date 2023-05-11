#' Compute budget for one winter from a stars object
#'
#' @inheritParams arguments
#'
#' @return a stars object with energy budget information
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'   path_NC_dir <- "../NC/ISIMIP_sources/gswp3-w5e5/OBSCLIM/"
#'   obsclim <- load_NC_files(path_NC_dir, downsample = TRUE)
#'   test <- compute_budget_stars1year(year_start = 1901, obsclim, fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'  }
#'
compute_budget_stars1year <- function(year_start,
                                      stars_object,
                                      fit_state, fit_MR,
                                      roost_insulation_dTa = 5,
                                      temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                                      threshold_mortality = 27,
                                      .cluster = NULL) {

  ## extract dates
  print(paste("computing budget for year", year_start))
  all_dates <- as.Date(stars::st_get_dimension_values(stars_object, "time"))
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
                               PROGRESS = TRUE,
                               CLUSTER = .cluster)

  ## reshape output
  tibble::as_tibble(stars_obj) |>
    tidyr::unnest(cols = c("compute_budget_summarystats")) |>
    stars::st_as_stars() -> stars_obj

  sf::st_crs(stars_obj) <- "+proj=longlat +datum=WGS84 +no_defs"

  stars_obj
}



#' Compute budget for all winters from a stars object
#'
#' @inheritParams arguments
#'
#' @return a stars object with energy budget information
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'   path_NC_dir <- "../NC/ISIMIP_sources/gswp3-w5e5/OBSCLIM/"
#'   obsclim <- load_NC_files(path_NC_dir, downsample = FALSE, .downsampling_args = c(40, 40, 0))
#'   obsclim1901_1904 <- dplyr::filter(obsclim, time < as.Date("1905-01-01"))
#'   test <- compute_budget_stars(obsclim1901_1904, fit_state = fit_normo_cauchit, fit_MR = fit_torpor, nb_cores_year = 5)
#'  }
#'
compute_budget_stars <- function(stars_object,
                                 fit_state, fit_MR,
                                 roost_insulation_dTa = 5,
                                 temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                                 threshold_mortality = 27,
                                 nb_cores_location = 1,
                                 nb_cores_year = 1) {

  ## infer years to consider as year_start for the extraction of the winter info
  first_year  <- as.numeric(format(min(stars::st_get_dimension_values(stars_object, "time")), "%Y"))
  last_year   <- as.numeric(format(max(stars::st_get_dimension_values(stars_object, "time")), "%Y")) - 1
  years_to_do <- first_year:last_year

  ## declare cluster for parallel job
  cluster_location <- NULL
  if (nb_cores_location > 1) {
    cluster_location <- parallel::makeCluster(min(c(parallel::detectCores(), nb_cores_location))) ## set number of CPU to use
    parallel::clusterExport(cl = cluster_location, varlist = ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(f) is.function(get(f)))]) ## export functions to workers
  }

  ## extract winter info for each year and locations
  stars_object_filled <- parallel::mclapply(years_to_do, function(year) {
    compute_budget_stars1year(year_start = year,
                              stars_object = stars_object,
                              fit_state = fit_state, fit_MR = fit_MR,
                              roost_insulation_dTa = roost_insulation_dTa,
                              temp_threshold = temp_threshold, split_summer = split_summer, min_days_trigger_winter = min_days_trigger_winter,
                              threshold_mortality = threshold_mortality,
                              .cluster = cluster_location)}, mc.cores = nb_cores_year)

  ## close cluster as parallel job done
  if (nb_cores_location > 1) {
    parallel::stopCluster(cluster_location)
  }

  stars_object_filled_all_years <- do.call("c", c(stars_object_filled, along = 3))
  stars_object_filled_all_years <- stars::st_set_dimensions(stars_object_filled_all_years, 3, values = as.character(years_to_do))
  stars_object_filled_all_years <- stars::st_set_dimensions(stars_object_filled_all_years, names = c("x", "y", "year"))

  sf::st_crs(stars_object_filled_all_years) <- "+proj=longlat +datum=WGS84 +no_defs"

  stars_object_filled_all_years
}

