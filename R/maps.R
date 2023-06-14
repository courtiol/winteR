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
plot_fat_map <- function(stars1, stars2, stars3, stars4,
                         strip_names = c("Obs (1901-1930)", "Obs (1989-2018)", "SSP1-2.6 (2070-2099)", "SSP5-8.5 (2070-2099)"),
                         threshold_mortality = 27) {

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

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = mask_country, fill = "grey", colour = NA, size = 0.1) +
    stars::geom_stars(data = data_plot, ggplot2::aes(fill = .data$Survive), sf = TRUE) + ## sf mode for degrees in axes
    ggplot2::facet_wrap(~ source) +
    ggplot2::geom_sf(data = mask_country, fill = NA, colour = "lightgrey", size = 0.1) +
    ggplot2::geom_sf(data = mask_ocean, fill = "white", colour = NA) +
    ggplot2::lims(x = range(stars::st_get_dimension_values(data_plot, "x")),
                  y = range(stars::st_get_dimension_values(data_plot, "y"))) +
    #ggplot2::scale_fill_manual(values = c("#BECA5C", "#EF8D32", "#CC561E", "black"), drop = FALSE, na.value = "grey") +
    ggplot2::scale_fill_manual(values = c(grDevices::rgb(179, 210, 52, maxColorValue = 255),
                                          grDevices::rgb(69, 129, 43, maxColorValue = 255),
                                          grDevices::rgb(13, 177, 75, maxColorValue = 255),
                                          "black"), drop = FALSE, na.value = "grey") +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(x = "", y = "", fill = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.key.size = ggplot2::unit(0.5, "cm"), legend.key.width = ggplot2::unit(2, "cm"),
                   legend.position = "top", strip.background = ggplot2::element_rect(fill = NA, colour = NA))
}
