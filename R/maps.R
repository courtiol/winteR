#' Plot maps showing the variation in winter budget through Europe
#'
#' This function is not used for now and thus not exported.
#'
#' @inheritParams arguments
#'
#' @return a ggplot2 object
#'
#' @examples
#' run <- FALSE
#' if (run) {
#'  readRDS("../NC/stars_winter/gswp3-w5e5_OBSCLIM_winter.rds") |>
#'   dplyr::select(Budget_winter) |>
#'   dplyr::filter(year >= 1901, year <= 1930) -> winter_stars_OBS_1901_1930
#'
#'  readRDS("../NC/stars_winter/gswp3-w5e5_OBSCLIM_winter.rds") |>
#'    dplyr::select(Budget_winter) |>
#'    dplyr::filter(year >= 1989, year <= 2018) -> winter_stars_OBS_1989_2018
#'
#'  reshape_stars_across.models("../NC/stars_winter", SSP = "126") |>
#'    dplyr::filter(year >= 2070) -> winter_stars_SSP126_2070_2099
#'
#'  reshape_stars_across.models("../NC/stars_winter", SSP = "585") |>
#'    dplyr::filter(year >= 2070) -> winter_stars_SSP585_2070_2099
#'
#'  maps <- plot_fat_map.panel(winter_stars_OBS_1901_1930,
#'                             winter_stars_OBS_1989_2018,
#'                             winter_stars_SSP126_2070_2099,
#'                             winter_stars_SSP585_2070_2099)
#'  showtext::showtext_opts(dpi = 300)
#' }
#'
plot_fat_map.panel <- function(stars1, stars2, stars3, stars4,
                         strip_names = NULL,
                         threshold_mortality = 27) {

  if (is.null(strip_names)) {
    strip_names <- c("<span style='font-size:15pt'> **A** </span> <span> </span> <span> </span> Obs (1901-1930)",
                                         "<span style='font-size:15pt'> **B** </span> <span> </span> <span> </span> Obs (1989-2018)",
                                         "<span style='font-size:15pt'> **C** </span> <span> </span> <span> </span> SSP1-2.6 (2070-2099)",
                                         "<span style='font-size:15pt'> **D** </span> <span> </span> <span> </span> SSP5-8.5 (2070-2099)")
  }

  label1 <- paste0("<", round(threshold_mortality*1/3, 1), "gr")
  label2 <- paste0("<", round(threshold_mortality*2/3, 1), "gr")
  label3 <- paste0("\u2264", round(threshold_mortality*3/3, 1), "gr")
  label4 <- paste(">", round(threshold_mortality*3/3, 1), "gr (dead)")

  f <- \(x) dplyr::case_when(mean(x, na.rm = TRUE) < threshold_mortality*1/3 ~ label1,
                             mean(x, na.rm = TRUE) < threshold_mortality*2/3 ~ label2,
                             mean(x, na.rm = TRUE) <= threshold_mortality*3/3 ~ label3,
                             mean(x, na.rm = TRUE) > threshold_mortality ~ label4)

  lapply(list(stars1, stars2, stars3, stars4), \(stars) {
    stars::st_apply(stars, MARGIN = c("x", "y"), FUN = f, .fname = "Survive") |>
      dplyr::mutate(Survive = factor(.data$Survive, levels = c(label1, label2, label3, label4)))
  }) -> stars_winter_all_agg

  stars_winter_all_agg <- do.call("c", stars_winter_all_agg)
  names(stars_winter_all_agg) <- strip_names
  data_plot <- merge(stars_winter_all_agg, name = "source")
  names(data_plot) <- "Survive"
  data_plot

  utils::data(lands_polygons, package = "winteR")
  utils::data(oceans_polygons, package = "winteR")

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = lands_polygons, fill = "grey", colour = NA, size = 0.1) +
    stars::geom_stars(data = data_plot, ggplot2::aes(fill = .data$Survive), sf = TRUE) + ## sf mode for degrees in axes
    ggplot2::facet_wrap(~ source) +
    ggplot2::geom_sf(data = lands_polygons, fill = NA, colour = "lightgrey", size = 0.1) +
    ggplot2::geom_sf(data = oceans_polygons, fill = "white", colour = NA) +
    ggplot2::lims(x = range(stars::st_get_dimension_values(data_plot, "x")),
                  y = range(stars::st_get_dimension_values(data_plot, "y"))) +
    ggplot2::scale_fill_manual(values = c(grDevices::rgb(233, 224, 131, maxColorValue = 255), "#EF8D32", "#CC561E", "black"), drop = FALSE, na.value = "grey") +
    #ggplot2::scale_fill_manual(values = c(grDevices::rgb(179, 210, 52, maxColorValue = 255),
    #                                      grDevices::rgb(69, 129, 43, maxColorValue = 255),
    #                                      grDevices::rgb(13, 177, 75, maxColorValue = 255),
    #                                      "black"), drop = FALSE, na.value = "grey") +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "", y = "", fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.key.size = ggplot2::unit(0.5, "cm"), legend.key.width = ggplot2::unit(2, "cm"),
                   legend.position = "top", strip.background = ggplot2::element_rect(fill = NA, colour = NA),
                   strip.text = ggtext::element_markdown(hjust = 0))
}


#' Plot maps showing the variation in winter budget through Europe
#'
#' This function produces the figure 4.
#'
#' @inheritParams arguments
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' # see ?winterR
#'
plot_fat_map <- function(stars_object,
                         threshold_mortality = 27,
                         base_size = 11) {

  label1 <- paste0("<", round(threshold_mortality*1/3, 1), "gr")
  label2 <- paste0("<", round(threshold_mortality*2/3, 1), "gr")
  label3 <- paste0("\u2264", round(threshold_mortality*3/3, 1), "gr")
  label4 <- paste(">", round(threshold_mortality*3/3, 1), "gr")

  f <- \(x) dplyr::case_when(mean(x, na.rm = TRUE) < threshold_mortality*1/3 ~ label1,
                             mean(x, na.rm = TRUE) < threshold_mortality*2/3 ~ label2,
                             mean(x, na.rm = TRUE) <= threshold_mortality*3/3 ~ label3,
                             mean(x, na.rm = TRUE) > threshold_mortality ~ label4)

  stars::st_apply(stars_object, MARGIN = c("x", "y", "year"), FUN = f, .fname = "Survive") |>
      dplyr::mutate(Survive = factor(.data$Survive, levels = c(label1, label2, label3, label4))) -> data_plot

  utils::data(lands_polygons, package = "winteR")
  utils::data(oceans_polygons, package = "winteR")

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = lands_polygons, fill = "grey", colour = NA, size = 0.1) +
    stars::geom_stars(data = data_plot, ggplot2::aes(fill = .data$Survive), sf = TRUE) + ## sf mode for degrees in axes
    ggplot2::geom_sf(data = lands_polygons, fill = NA, colour = "lightgrey", size = 0.1) +
    ggplot2::geom_sf(data = oceans_polygons, fill = "white", colour = NA) +
    ggplot2::lims(x = range(stars::st_get_dimension_values(data_plot, "x")),
                  y = range(stars::st_get_dimension_values(data_plot, "y"))) +
    ggplot2::scale_fill_manual(values = c(grDevices::rgb(233, 224, 131, maxColorValue = 255), "#EF8D32", "#CC561E", "black"), drop = FALSE, na.value = "grey") +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "", y = "", fill = "") +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.key.size = ggplot2::unit(0.5, "cm"), legend.key.width = ggplot2::unit(1.5, "cm"),
                   legend.position = "top", strip.background = ggplot2::element_rect(fill = NA, colour = NA),
                   strip.text = ggtext::element_markdown(hjust = 0))
}
