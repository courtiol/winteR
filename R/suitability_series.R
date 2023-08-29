#' Extract summary statistics about winter suitability
#'
#' This function returns how often an average bat experience a suitable winter in a given location.
#' It also computes the year of the establishment of bats, as well as the year of their
#' disappearance in a given location. We consider that the year of establishment corresponds to the
#' first year within a sequence of at least 5 years in a row  (or another number as defined via the
#' parameter `min_years_trigger_suitability`) for which an hibernation season exists and an average
#' bat is expected to survive each such season. We consider that the year of disappearance
#' corresponds to the year 5 (i.e. `min_years_trigger_suitability`) within a sequence of at least 5
#' years in a row (i.e. `min_years_trigger_suitability`) for which either an hibernation season does
#' not exist, or an average bat is not expected to survive each such years.
#'
#' @inheritParams arguments
#'
#' @return a data frame
#' @export
#'
#' @examples
#' suitable_winters_vec <- c(rep(FALSE, 15), rep(TRUE, 4), FALSE, rep(TRUE, 6),
#'                           rep(FALSE, 10), rep(TRUE, 5), rep(TRUE, 2))
#' #suitable_winters_vec <- rep(TRUE, 10)
#' #suitable_winters_vec <- rep(FALSE, 10)
#' years <- seq(2000, length.out = length(suitable_winters_vec))
#' data.frame(Year = years, Suitable_winter = suitable_winters_vec)
#' compute_suitability_summarystats(vec_Suitable_winter = suitable_winters_vec, vec_Year = years)
#'
compute_suitability_summarystats <- function(vec_Suitable_winter, vec_Year, min_years_trigger_suitability = 5) {

  data_suitability <- data.frame(Year = vec_Year, Suitable_winter = vec_Suitable_winter)

  ## figure out if there is establishment and disappearance happened
  rle_obj <- rle(data_suitability$Suitable_winter)
  establishment <- any(rle_obj$values[rle_obj$lengths >= min_years_trigger_suitability])
  disappearance <- any(!rle_obj$values[rle_obj$lengths >= min_years_trigger_suitability])

  ## detecting first year of establishment
  if (establishment) {
    index_begin <- min(which(rle_obj$lengths >= min_years_trigger_suitability & rle_obj$values)) - 1
    if (length(index_begin) > 0) {
      index_year_begin <- sum(rle_obj$lengths[seq_len(index_begin)]) + 1
      year_establishment <- ifelse(length(index_year_begin) > 0, data_suitability$Year[index_year_begin], data_suitability$Year[1])
    }
  } else {
    year_establishment <- NA
  }

  ## detecting first year of disappearance
  if (disappearance && establishment) {
    indices_begin <- which(rle_obj$lengths >= min_years_trigger_suitability & !rle_obj$values) - 1
    if (length(indices_begin) > 0) {
      indices_year_begin <- sapply(indices_begin, \(i) sum(rle_obj$lengths[seq_len(i)]) + min_years_trigger_suitability)
      possible_years <- data_suitability$Year[indices_year_begin]
      year_disappearance <- ifelse(any(possible_years > year_establishment), min(possible_years[possible_years > year_establishment]), NA)
    }
  } else {
    year_disappearance <- NA
  }

  ## return
  data.frame(freq_suitability = mean(data_suitability$Suitable_winter),
             year_establishment = year_establishment,
             year_disappearance = year_disappearance)
}



#' Estimate suitability across space and add it to a stars object
#'
#' @inheritParams arguments
#'
#' @return a stars object
#' @export
#'
#' @examples
#' data_OBSCLIM <- readRDS("../NC/stars_winter/gswp3-w5e5_OBSCLIM_winter.rds")
#' add_suitability_stars(data_OBSCLIM)
#'
add_suitability_stars <- function(stars_object, min_years_trigger_suitability = 10, add_winter.stats = TRUE) {

  years <- as.numeric(stars::st_get_dimension_values(stars_object, "year"))

  stars_object |>
    dplyr::mutate(Survive_winter = .data$Survive & .data$Duration_winter > 0, .keep = "none") -> stars_object2

  years <- as.numeric(stars::st_get_dimension_values(stars_object2, "year"))

  stars_obj <- stars::st_apply(stars_object2,
                               MARGIN = c("x", "y"),
                               FUN = compute_suitability_summarystats,
                               vec_Year = years,
                               min_years_trigger_suitability = min_years_trigger_suitability)

    ## reformat output (see also https://github.com/r-spatial/stars/issues/635)
    data <- do.call("rbind", stars_obj$compute_suitability_summarystats)
    stars_obj$compute_suitability_summarystats <- NULL

    for (layer in colnames(data)) {
      stars_obj[[layer]] <-  data[, layer]
    }

  sf::st_crs(stars_obj) <- "+proj=longlat +datum=WGS84 +no_defs"

  if (add_winter.stats) {
    stars_obj <- c(stars_obj, build_stars_winter.stats(stars_object))
  }

  stars_obj

}


#' Helper function to turn years into decades or other time periods
#'
#' @inheritParams arguments
#'
#' @return a vector
#' @export
#'
#' @examples
#' recode_year_decade(c(1909, 1910, 2022, 2099))
#' recode_year_decade(c(1909, 1910, 2022, 2099), years_to_combine = 20)
#'
recode_year_decade <- function(vec_Year, min_Year = 1901, max_Year = 2099, years_to_combine = 10) {
  decades <- years_to_combine*floor(vec_Year/years_to_combine)
  min_decade <- years_to_combine*floor(min_Year/years_to_combine)
  max_decade <- years_to_combine*floor(max_Year/years_to_combine)
  start_possible_decades <- as.character(seq(min_decade, max_decade, by = years_to_combine))
  end_possible_decades <- as.character(seq(min_decade + years_to_combine - 1, max_decade + years_to_combine - 1, by = years_to_combine))
  possible_decades <- paste0(start_possible_decades, "-", end_possible_decades)
  possible_decades[1] <- paste0("<", min_decade + years_to_combine)
  decades <- dplyr::case_when(decades == min_decade ~ paste0("<", decades + years_to_combine),
                              decades > min_decade ~ paste0(as.character(decades), "-", as.character(decades + years_to_combine - 1)))
  factor(decades, levels = rev(possible_decades))
}



#' Helper function to turn proportion into categories of percentages
#'
#' @inheritParams arguments
#'
#' @return a vector
#' @export
#'
#' @examples
#' recode_freq_pct(seq(0, 1, 0.01))
#'
recode_freq_pct <- function(vec_Proportion) {
  simple_seq <- paste0(as.character(seq(10, 90, 10)), "-", as.character(seq(10, 90, 10) + 9), "%")
  possible_pct <- c("0%", "<1%", "1-9%", simple_seq, ">99%")
  vec_Proportion2 <- dplyr::case_when(vec_Proportion == 0 ~ "0%",
                                      100*vec_Proportion < 1 ~ "<1%",
                                      100*vec_Proportion < 10 ~ "1-9%",
                                      100*vec_Proportion <= 99 ~ paste0(as.character(floor(10*vec_Proportion)*10), "-", as.character(floor(10*vec_Proportion)*10 + 9), "%"),
                                      100*vec_Proportion > 99 ~ ">99%")
  factor(vec_Proportion2, levels = rev(possible_pct))
}


#' Estimate suitability across space
#'
#' @inheritParams arguments
#'
#' @return a stars object
#' @export
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'   build_suitability_stars("../NC/stars_winter")
#' }
#'
build_suitability_stars <- function(directory_stars, min_years_trigger_suitability = 5) {

  ## Identify files to handle
  OBSCLIM_file <- list.files(path = directory_stars, full.names = TRUE, pattern =  "OBSCLIM")

  list_winter.stars_files <- list.files(path = directory_stars, full.names = TRUE, pattern =  ".rds")
  #list_winter.stars_files <- list_winter.stars_files[list_winter.stars_files != OBSCLIM_file]

  ## Handle OBSCLIM stars
  OBSCLIM_stars <- readRDS(file = OBSCLIM_file)
  OBSCLIM_stars_processed <- add_suitability_stars(OBSCLIM_stars, min_years_trigger_suitability = min_years_trigger_suitability)

  ## Extract the name of the variables produced by add_suitability_stars()
  varnames <- names(OBSCLIM_stars_processed)

  ## Handle future projections
  list_winter.stars_info <- lapply(list_winter.stars_files, function(stars) {
    stars_focal <- readRDS(file = stars)
    scenario <- stringr::str_remove_all(stringr::str_extract(basename(stars), pattern = "(.*)(OBSCLIM|SSP.{3})\\1"), pattern = "\\_")
    forcing <- stringr::str_extract(basename(stars), pattern = ".*(?=\\_OBSCLIM|\\_SSP)")
    data.frame(File = stars, Scenario = scenario, Forcing = forcing)
  })

  tbl_winter.stars_info <- tibble::as_tibble(do.call("rbind", list_winter.stars_info))

  ### Create table where each row should be one batch of files to be processed together and averaged
  tbl_winter.stars_info |>
    dplyr::summarise(Files = list(.data$File), .by = c("Scenario")) -> tbl.aggregated_winter.stars_info

  ### Loads winter stars
  tbl.aggregated_winter.stars_info |>
    dplyr::rowwise() |>
    dplyr::mutate(stars_objects = list(lapply(.data$Files, \(file) readRDS(file))),
                  stars_objects_with_OBSCLIM =  list(lapply(.data$Files, \(file) c(OBSCLIM_stars, readRDS(file), along = "year")))) |>
    dplyr::select(-.data$Files) -> tbl.aggregated_winter.stars

  ### Compute suitability
  print("processing stars... (it takes a few minutes, be patient)")
  tbl.aggregated_winter.stars |>
    dplyr::rowwise() |>
    dplyr::mutate(stars_suitability =  list(lapply(.data$stars_objects, \(stars_object) add_suitability_stars(stars_object, min_years_trigger_suitability = min_years_trigger_suitability))),
                  stars_with_OBSCLIM_suitability =  list(lapply(.data$stars_objects_with_OBSCLIM, \(stars_object) add_suitability_stars(stars_object, min_years_trigger_suitability = min_years_trigger_suitability)))) |>
    dplyr::select(-.data$stars_objects, -.data$stars_objects_with_OBSCLIM) -> tbl_suitability.stars

  ## Merging across forcing models
  tbl_suitability.stars |>
    dplyr::rowwise() |>
    dplyr::mutate(stars_suitability_merged = list(merge(do.call(c, c(.data$stars_suitability)), name = "metric")),
                  stars_with_OBSCLIM_suitability_merged = list(merge(do.call(c, c(.data$stars_with_OBSCLIM_suitability)), name = "metric"))) |>
    dplyr::select(-.data$stars_suitability, -.data$stars_with_OBSCLIM_suitability) -> tbl_suitability.stars.merged

  ## Averaging across merged stars
  tbl_suitability.stars.merged |>
    dplyr::ungroup() |>
    dplyr::mutate(metrics = list(varnames)) |>
    tidyr::unnest_longer(.data$metrics) |>
    dplyr::mutate(stars_1metric = purrr::map2(.x = .data$stars_suitability_merged, .y = .data$metrics, .f = ~ dplyr::filter(.x, grepl(.y, .data$metric))),
                  stars_with_OBSCLIM_1metric = purrr::map2(.x = .data$stars_with_OBSCLIM_suitability_merged, .y = .data$metrics, .f = ~ dplyr::filter(.x, grepl(.y, .data$metric)))) |>
    dplyr::rowwise() |>
    dplyr::mutate(stars_1metric_avg = list(stars::st_apply(.data$stars_1metric, MARGIN = c("x", "y"), FUN = mean, .fname =  .data$metrics)),
                  stars_with_OBSCLIM_1metric_avg = list(stars::st_apply(.data$stars_with_OBSCLIM_1metric, MARGIN = c("x", "y"), FUN = mean, .fname =  .data$metrics))) |>
    dplyr::ungroup() |>
    dplyr::select(.data$Scenario, .data$stars_1metric_avg, .data$stars_with_OBSCLIM_1metric_avg) |>
    dplyr::summarise(stars_avg = list(do.call(c, c(.data$stars_1metric_avg))), ## or stars_avg = purrr::list_flatten(.data$stars_1metric_avg)
                     stars_with_OBSCLIM_avg = list(do.call(c, c(.data$stars_with_OBSCLIM_1metric_avg))),
                     .by = "Scenario")

}


#' Plot the suitability niche
#'
#' This function combines all winter stars and count for each combination of mean winter temperature
#' and winter duration (discretised), for how many hibernation seasons an average bat is expected to
#' survive.
#'
#' @inheritParams arguments
#'
#' @return a ggplot2
#' @export
#'
#' @examples
#' ## See ?winteR
#'
plot_hibernation_niche <- function(stars_list) {

  utils::data("lands_polygons", package = "winteR")

  all_data <- lapply(stars_list, function(stars) {
    stars <- stars[lands_polygons, crop = FALSE]
    as.data.frame(stars)
    })

  all_data

  do.call("rbind", all_data) |>
    dplyr::mutate(Duration_winter_f = cut(.data$Duration_winter, breaks = seq(0, 370, 20), include.lowest = FALSE),
                  Temp_winter_mean_f = cut(.data$Temp_winter_mean, breaks = seq(-100, 100, 2), include.lowest = TRUE)) |>
    dplyr::summarise(n = sum(!is.na(.data$Duration_winter)),
                     Suitability = mean(.data$Survive, na.rm = TRUE),
                     .by = c("Duration_winter_f", "Temp_winter_mean_f")) |>
    dplyr::mutate(Suitability_f = cut(.data$Suitability, breaks = c(0, 0.1, 0.5, 0.9, 1), include.lowest = TRUE),
                  Suitability_f = factor(.data$Suitability_f, levels = rev(levels(.data$Suitability_f))),
                  n_f = dplyr::case_when(.data$n < 11 ~ as.character(.data$n),
                                         .data$n < 100 ~ ">10",
                                         .data$n < 1000 ~ ">100",
                                         TRUE ~ ">1000")) |>
    dplyr::filter(!is.na(.data$Duration_winter_f),
                  !is.na(.data$Temp_winter_mean_f)) -> data_plot

  ggplot2::ggplot(data_plot) +
    ggplot2::aes(x = .data$Duration_winter_f, y = .data$Temp_winter_mean_f, fill = .data$Suitability_f) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = .data$n_f), colour = "white", size = 2) +
    ggplot2::scale_fill_viridis_d(option = "H", direction = -1, begin = 0.1, end = 1) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Duration of the hibernation season (days)", y = "Mean temperature during the hibernation season (\u00B0C)", fill = "Suitability") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

