
#' Classify dates within 2 consecutive years as belonging to biological winter or not
#'
#' @param data a dataframe with at least columns `Date` and `Temp`
#' @param split_summer the day of mid-summer (default = `'07-01'`)
#' @param temp_threshold the approximate temperature below which insects do not fly
#' @param min_days_trigger_winter the minimum number of days for which the temperature should be below `temp_threshold` to enter winter
#'
#' @return a vector of boolean
#' @export
#'
#' @examples
#' filepath <- list.files(system.file("extdata/weather_real", package = "winteR"),
#'                        full.names = TRUE)[1]
#' data_Kharkiv <- build_Kharkiv_table(filepath)
#' data_Kharkiv$Winter <- classify_winter_temp2years(data_Kharkiv)
#' plot_Kharkiv_temp(data_Kharkiv, colour_var = Winter)
#'
classify_winter_temp2years <- function(data,
                                       split_summer = "07-01",
                                       temp_threshold = 7,
                                       min_days_trigger_winter = 14) {

  if (!all(c("Date", "Temp") %in% colnames(data))) {
    stop("The function `classify_winter_temp()` requires at least 2 columns: `Date` and `Temp`")
  }

  ## extract years
  years <- as.numeric(unique(format(data$Date, "%Y")))
  year1 <- min(years)
  year2 <- max(years)

  ## compute mid summer
  mid_summer1 <- as.Date(paste0(year1, "-", split_summer), origin = "1970-01-01")
  mid_summer2 <- as.Date(paste0(year2, "-", split_summer), origin = "1970-01-01")

  ## extract year 1
  data_year1 <- data[years == year1, ]

  ## extract year 2
  data_year2 <- data[years == year2, ]

  ## detecting temperature below threshold
  data$daily_cold <- data$Temp <= temp_threshold

  ## detecting winter
  data$possible_winter <-  dplyr::lag(cumsum(data$daily_cold), min_days_trigger_winter) - cumsum(data$daily_cold) == -min_days_trigger_winter

  ## detecting begin of winter
  date_possible_begin <- stats::na.omit(data$Date[data$possible_winter])
  begin <- date_possible_begin[date_possible_begin > mid_summer1][1] - min_days_trigger_winter + 1

  ## detecting end of winter
  date_possible_end <- stats::na.omit(data$Date[data$possible_winter])
  end <- rev(date_possible_end[date_possible_end < mid_summer2])[1]

  ## special cases for harsh winters
  begin <- max(c(begin, mid_summer1))
  end <- min(c(end, mid_summer2 - 1))

  ## winter
  data$Date >= begin & data$Date <= end
}

