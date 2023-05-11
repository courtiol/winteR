#' Build table with raw temperature data
#'
#' This function loads a file containing date and temperature data for 2 consecutive years-
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#' @examples
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2011_2012.csv")
#' data_Kharkiv <- build_temp_2years(file_Kharkiv)
#' head(data_Kharkiv)
#'
build_temp_2years <- function(filename) {
  d <- utils::read.csv(filename, header = TRUE, sep = ",", dec = ".")
  d$Time <- as.POSIXct(d$Time, format = "%d.%m.%Y %H:%M", tz = "EAT")
  d <- d[order(d$Time), ]
  d |>
    dplyr::mutate(Date = as.Date(.data$Time, origin = "1970-01-01")) |>
    dplyr::summarise(Temp = mean(.data$Temp), .by = "Date")
}


#' Extract summary statistics for a winter
#'
#' @inheritParams arguments
#'
#' @return a vector of boolean
#' @export
#'
#' @examples
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2011_2012.csv")
#' data_Kharkiv <- build_temp_2years(file_Kharkiv)
#' extract_winter_stats(data_Kharkiv)
#'
extract_winter_stats <- function(data_temp,
                                 split_summer = "07-01",
                                 temp_threshold = 7,
                                 min_days_trigger_winter = 14) {

  if (!all(c("Date", "Temp") %in% colnames(data_temp))) {
    stop("The function `classify_winter_temp()` requires at least 2 columns: `Date` and `Temp`")
  }

  ## extract years
  years <- as.numeric(unique(format(data_temp$Date, "%Y")))
  year1 <- min(years)
  year2 <- max(years)

  ## compute mid summer
  mid_summer1 <- as.Date(paste0(year1, "-", split_summer), origin = "1970-01-01")
  mid_summer2 <- as.Date(paste0(year2, "-", split_summer), origin = "1970-01-01")

  ## extract year 1
  data_year1 <- data_temp[years == year1, ]

  ## extract year 2
  data_year2 <- data_temp[years == year2, ]

  ## detecting temperature below threshold
  data_temp$daily_cold <- data_temp$Temp <= temp_threshold

  ## detecting winter
  data_temp$possible_winter <-  dplyr::lag(cumsum(data_temp$daily_cold), min_days_trigger_winter) - cumsum(data_temp$daily_cold) == -min_days_trigger_winter

  ## detecting begin of winter
  date_possible_begin <- stats::na.omit(data_temp$Date[data_temp$possible_winter])
  begin <- date_possible_begin[date_possible_begin > mid_summer1 & date_possible_begin < mid_summer2][1] - min_days_trigger_winter + 1

  ## detecting end of winter
  date_possible_end <- stats::na.omit(data_temp$Date[data_temp$possible_winter])
  end <- rev(date_possible_end[date_possible_begin > mid_summer1 & date_possible_begin < mid_summer2])[1]

  ## special cases for harsh winters
  begin <- max(c(begin, mid_summer1))
  end <- min(c(end, mid_summer2 - 1))

  ## is there any winter?
  winter <- ifelse(is.na(begin), FALSE, TRUE)

  ## extract winter temperatures
  temp_winter <- data_temp$Temp[data_temp$Date >= begin & data_temp$Date <= end]

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
#' @inheritParams arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2011_2012.csv")
#' data_Kharkiv <- build_temp_2years(file_Kharkiv)
#' plot_winter_temp2years(data_Kharkiv)
#'
#'
plot_winter_temp2years <- function(data_temp, base_size = 11, temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                                   roost_insulation_dTa = 5) {

  winter <- extract_winter_stats(data_temp, temp_threshold = temp_threshold, split_summer = split_summer,
                                 min_days_trigger_winter = min_days_trigger_winter)
  data_temp$Winter <- data_temp$Date >= winter$start_winter & data_temp$Date <= winter$stop_winter
  data_plot <- data_temp[data_temp$Date >= winter$mid_summer1 & data_temp$Date < winter$mid_summer2, ]

  ## plot
  ggplot2::ggplot(data_plot) +
    ggplot2::geom_hline(yintercept = temp_threshold, linetype = 4, colour = "darkgrey") +
    ggplot2::geom_vline(xintercept = winter$start_winter, linetype = 2, colour = "#0057b7") +
    ggplot2::geom_vline(xintercept = winter$stop_winter, linetype = 2, colour = "#0057b7") +
    ggplot2::aes(y = .data$Temp, x = .data$Date) +
    ggplot2::geom_line(data = data_plot[data_plot$Date <= winter$start_winter, ], colour = "#ffd700") +
    ggplot2::geom_line(data = data_plot[data_plot$Date >= winter$stop_winter, ], colour = "#ffd700") +
    ggplot2::geom_line(data = data_plot[data_plot$Date >= winter$start_winter & data_plot$Date <= winter$stop_winter, ], colour = "#0057b7") +
    #ggplot2::geom_point(size = 0.5, shape = 1) +
    ggplot2::scale_y_continuous(breaks = c(temp_threshold, seq(-100, 100, by = 5)), minor_breaks = NULL,
                                sec.axis = ggplot2::sec_axis(~ . + roost_insulation_dTa, breaks = seq(-100, 100, by = 5),
                                                             name = "Roost temperature (\u00B0C)")) +
    ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b 1st",
                          minor_breaks = "1 month", limits = c(winter$mid_summer1, winter$mid_summer2)) +
    ggplot2::labs(y = "Ambient temperature (\u00B0C)", x = NULL) +
    ggplot2::coord_cartesian(xlim = c(winter$mid_summer1, winter$mid_summer2)) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.position = "none")
}

