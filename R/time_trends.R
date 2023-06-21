#' Plot how a variable connected to the hibernation niche changes over time
#'
#' @inheritParams arguments
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' ## see ?winteR
#'
plot_time_trends <- function(winters_stats_df,
                             window_length_smoothing = 10,
                             varname = "Latitude_median",
                             vartype = c('latitude', 'area'),
                             y_title = "\n Median latitude of suitable hibernation areas",
                             scenarios = c("OBSCLIM", "SSP126", "SSP585"),
                             .fill_values = c("NA", "blue", "orange"),
                             .color_values = c("black", "blue", "orange"),
                             .legend_position = "none",
                             base_size = 9) {

  winters_stats_df |>
    dplyr::mutate(Year = as.numeric(as.character(.data$Year))) |>
    dplyr::summarise(y = mean(.data[[varname]]),
                     y_min = min(.data[[varname]]),
                     y_max = max(.data[[varname]]), .by = c("Scenario", "Year")) |>
    dplyr::mutate(y_smooth = caTools::runmean(.data$y, k = window_length_smoothing),
                  y_min_smooth = caTools::runmean(.data$y_min, k = window_length_smoothing),
                  y_max_smooth = caTools::runmean(.data$y_max, k = window_length_smoothing),
                  .by = c("Scenario")) |>
    dplyr::filter(.data$Scenario %in% scenarios) -> avgdata_for_plot

  avgdata_for_plot |>
    dplyr::filter(.data$Scenario == "OBSCLIM") |>
    dplyr::pull(.data$y_smooth) |>
    utils::head(1) -> y_ref0

  plot <- ggplot2::ggplot(avgdata_for_plot) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$y_min_smooth, ymax = .data$y_max_smooth,
                                      x = .data$Year, fill = .data$Scenario),
                         alpha = 0.2, linewidth = 0.3, linetype = "dotted") +
    ggplot2::geom_line(mapping = ggplot2::aes(y = .data$y_smooth, x = .data$Year, colour = .data$Scenario)) +
    ggplot2::geom_hline(yintercept = y_ref0, linetype = "dashed", size = 0.3)

  if (vartype[1] == 'latitude') {

    max_distance <- compute_northward_move(max(avgdata_for_plot$y_max_smooth), lat_ref = y_ref0)

    increament_distance <- ifelse(max_distance < 500, 50, 100)

    plot <- plot +
      ggplot2::scale_y_continuous(breaks = -90:90, labels = paste0(-90:90,"\u00B0N"), minor_breaks = NULL,
                                  limits = range(c(avgdata_for_plot$y_max_smooth,
                                                   avgdata_for_plot$y_min_smooth,
                                                   avgdata_for_plot$y_smooth)),
                                  sec.axis = ggplot2::sec_axis(name = "Northward distance change (km)",
                                                               trans = ~ compute_northward_move(., lat_ref = y_ref0),
                                                               breaks = seq(-40000, 40000, increament_distance)))
  } else if (vartype[1] == 'area') {

    plot <- plot +
      ggplot2::scale_y_continuous(breaks = seq(0, 10e6, 0.5e6), labels = seq(0, 10e6, 0.5e6)/1e6,
                                  sec.axis = ggplot2::sec_axis(name = "Range expansion (%)",
                                                               trans = ~ 100*((. -y_ref0)/y_ref0),
                                                               breaks = seq(-100, 100, 10)))

  } else stop("`vartype` not found.")

  plot +
    ggplot2::scale_x_continuous(breaks = unique(c(seq(min(avgdata_for_plot$Year),
                                                      max(avgdata_for_plot$Year), 10),
                                                  max(avgdata_for_plot$Year))),
                                labels = paste0(unique(c(seq(min(avgdata_for_plot$Year),
                                                             max(avgdata_for_plot$Year), 10),
                                                         max(avgdata_for_plot$Year))),
                                                "-",
                                                unique(c(seq(min(avgdata_for_plot$Year) + 1,
                                                             max(avgdata_for_plot$Year) + 1, 10),
                                                         max(avgdata_for_plot$Year) + 1))),
                                minor_breaks = NULL) +
    ggplot2::scale_fill_manual(values = .fill_values) +
    ggplot2::scale_colour_manual(values = .color_values) +
    ggplot2::theme_bw(base_size = base_size, base_line_size = 0.1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                   strip.background = ggplot2::element_rect(fill = NA), legend.position = .legend_position) +
    ggplot2::labs(x = NULL, y = y_title, colour = "", shape = "", fill = "")
}