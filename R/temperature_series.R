
#' Extract summary statistics for a winter
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
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2011_2012.csv")
#' data_Kharkiv <- build_Kharkiv_table(file_Kharkiv)
#' extract_winter_stats(data_Kharkiv)
#'
extract_winter_stats <- function(data,
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

  ## is there any winter?
  winter <- ifelse(is.na(begin), FALSE, TRUE)

  ## extract winter temperatures
  temp_winter <- data$Temp[data$Date >= begin & data$Date <= end]

  ## return
  list(start_winter = begin,
       stop_winter = end,
       year1 = year1,
       year2 = year2,
       mid_summer1 = mid_summer1,
       mid_summer2 = mid_summer2,
       duration_winter = ifelse(winter, as.numeric(difftime(end, begin, units = "days")), 0),
       temp_winter_mean = ifelse(winter, mean(temp_winter), NA),
       temp_winter_sd = ifelse(winter, stats::sd(temp_winter), NA),
       temp_winter_median = ifelse(winter, stats::median(temp_winter), NA),
       temp_winter_min = ifelse(winter, min(temp_winter), NA),
       temp_winter_max = ifelse(winter, max(temp_winter), NA),
       temp_winter_autocorr = ifelse(winter, stats::acf(temp_winter, plot = FALSE, lag.max = 1)$acf[[2]], NA))
}


#' Plot temperature and winter data
#'
#' @param data a dataframe such as one created by [build_Kharkiv_table()]
#' @inheritParams plot_Tskin_fit
#' @inheritParams extract_winter_stats
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2011_2012.csv")
#' data_Kharkiv <- build_Kharkiv_table(file_Kharkiv)
#' plot_winter_temp2years(data_Kharkiv)
#'
#'
plot_winter_temp2years <- function(data, base_size = 11, temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14) {

  winter <- extract_winter_stats(data, temp_threshold = temp_threshold, split_summer = split_summer,
                                 min_days_trigger_winter = min_days_trigger_winter)
  data$Winter <- data$Date >= winter$start_winter & data$Date <= winter$stop_winter
  data_plot <- data[data$Date >= winter$mid_summer1 & data$Date < winter$mid_summer2, ]

  ## plot
  ggplot2::ggplot(data_plot) +
    ggplot2::geom_hline(yintercept = temp_threshold, linetype = 3) +
    ggplot2::geom_vline(xintercept = winter$start_winter, linetype = 2, colour = "#0057b7") +
    ggplot2::geom_vline(xintercept = winter$stop_winter, linetype = 2, colour = "#0057b7") +
    ggplot2::aes(y = .data$Temp, x = .data$Date) +
    ggplot2::geom_line(data = data_plot[data_plot$Date <= winter$start_winter, ], colour = "#ffd700") +
    ggplot2::geom_line(data = data_plot[data_plot$Date >= winter$stop_winter, ], colour = "#ffd700") +
    ggplot2::geom_line(data = data_plot[data_plot$Date >= winter$start_winter & data_plot$Date <= winter$stop_winter, ], colour = "#0057b7") +
    ggplot2::geom_point(size = 0.5, shape = 1) +
    ggplot2::scale_y_continuous(breaks = c(temp_threshold, seq(-100, 100, by = 5)), minor_breaks = NULL) +
    ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b 1st",
                          minor_breaks = "1 month", limits = c(winter$mid_summer1, winter$mid_summer2)) +
    ggplot2::labs(y = "Ambient temperature (\u00B0C)\n", x = NULL) +
    ggplot2::coord_cartesian(xlim = c(winter$mid_summer1, winter$mid_summer2)) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.position = "none")
}

