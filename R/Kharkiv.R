#' Build table with raw temperature data from Kharkiv, Ukraine
#'
#' This function loads the file containing temperature data from years 2011 and 2012 in Kharkiv, Ukraine.
#'
#' @inheritParams load_Tskin_datafile
#' @return a dataframe
#' @export
#' @examples
#' filepath <- list.files(system.file("extdata/weather_real", package = "winteR"),
#'                        full.names = TRUE)[1]
#' data_Kharkiv <- build_Kharkiv_table(filepath)
#' head(data_Kharkiv)
#'
build_Kharkiv_table <- function(filename) {
  d <- utils::read.csv(filename, header = TRUE, sep = ",", dec = ".")
  d$Time <- as.POSIXct(d$Time, format = "%d.%m.%Y %H:%M", tz = "EAT")
  d <- d[order(d$Time), ]
  d |>
    dplyr::mutate(Date = as.Date(.data$Time, origin = "1970-01-01")) |>
    dplyr::summarise(Temp = mean(.data$Temp), .by = "Date")
}


#' Plot Kharkiv temperature data
#'
#' @param data the dataframe created by [build_Kharkiv_table()]
#' @inheritParams plot_Tskin_fit
#' @param colour_var (optional) the unquotted name of a variable in `data` that defines the colour in the plot
#' @inheritParams classify_winter_temp2years
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' filepath <- list.files(system.file("extdata/weather_real", package = "winteR"),
#'                        full.names = TRUE)[1]
#' data_Kharkiv <- build_Kharkiv_table(filepath)
#' plot_Kharkiv_temp(data_Kharkiv)
#'
#'
plot_Kharkiv_temp <- function(data, base_size = 11, colour_var = NULL, temp_threshold = 7) {
  ggplot2::ggplot(data) +
    ggplot2::geom_hline(yintercept = temp_threshold, linetype = 2) +
    ggplot2::aes(y = .data$Temp, x = .data$Date) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(colour = {{colour_var}})) +
    ggplot2::theme_bw(base_size = base_size)
}
