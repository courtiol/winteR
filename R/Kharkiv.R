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

