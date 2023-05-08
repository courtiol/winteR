#' Compute the energy budget associated to given ambient temperatures
#'
#' Bugets are computed in grams of fat consumed.
#'
#' @inheritParams extract_winter_stats
#' @param fit_state a fitted model predicting the probability to be in normothermy
#' @param fit_MR a fitted model predicting the metabolic rate in KJh^-1
#' @param roost_insulation_dTa the increase in temperature in the roost, compared to outside (default = 5)
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' #See ?winteR
#'
compute_nrg_budget <- function(data, fit_state, fit_MR, roost_insulation_dTa = 5) {

  ## ambient temperature is higher in roost
  data$Ta <- data$Temp + roost_insulation_dTa

  ## predict proba to be in normothermy and in torpor
  prob_normo <- stats::predict(fit_state, newdata = data.frame(Ta = data$Ta), re.form = NA)[,1 ]
  prob_torpor <- 1 - prob_normo
  data$Prob_normo <- prob_normo
  data$Prob_torpor <- prob_torpor

  ## predict metabolic rate in both physiological states
  pred_MR <- suppressWarnings(torpor::tor_predict(fit_MR, Ta = data$Ta))
  pred_MR$pred <- pred_MR$pred/37.7
  pred_MR$upr_95 <- pred_MR$upr_95/37.7
  pred_MR$lwr_95 <- pred_MR$lwr_95/37.7

  params <- torpor::get_parameters(fit_MR)
  Tlc <- params[params$parameter == "Tlc", "mean"]

  data$MR_torpor <- 0
  data$MR_torpor[data$Ta < Tlc] <- pred_MR[pred_MR$assignment == "Torpor", "pred"]

  data$MR_normo <- NA
  data$MR_normo[data$Ta < Tlc] <- pred_MR[pred_MR$assignment == "Euthermia", "pred"]
  data$MR_normo[data$Ta >= Tlc] <- pred_MR[pred_MR$assignment == "Mtnz", "pred"]

  data$Prob_normo[data$Ta >= Tlc] <- 1
  data$Prob_torpor[data$Ta >= Tlc] <- 0

  ## compute budget
  data$Daily_duration_normo <- data$Prob_normo * 24
  data$Daily_duration_torpor <- data$Prob_torpor * 24
  data$Budget <- data$MR_normo * data$Daily_duration_normo + data$MR_torpor * data$Daily_duration_torpor

  ## return
  data
}


#' Plot the energy budget
#'
#' @inheritParams plot_winter_temp2years
#' @param data_budget a dataframe created by [compute_nrg_budget()]
#' @param y a string of characters indicating what y-variable to plot:
#'   "g_fat_per_state", "g_fat_per_day", or "g_fat_per_winter"
#' @param threshold_mortality the maximal amount of fat consumed before mortality occurs (default = 24)
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' filepath <- list.files(system.file("extdata/weather_real", package = "winteR"),
#'                        full.names = TRUE)[1]
#' data_Kharkiv <- build_temp_2years(filepath)
#' Tskin_files <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- build_Tskin_table(Tskin_files)
#' data_normothermy <- data_Tskin[data_Tskin$Included, ]
#' fit_normo_cauchit <- spaMM::fitme(Normo ~ Ta + (1|ID), data = data_normothermy,
#'                                  family = binomial(link = "cauchit"))
#' data_nrg <- compute_nrg_budget(data_Kharkiv, fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#' plot_nrg_budget(data_nrg, y = "g_fat_per_state")
#' plot_nrg_budget(data_nrg, y = "g_fat_per_day")
#' plot_nrg_budget(data_nrg, y = "g_fat_per_winter")
#'
plot_nrg_budget <- function(data_budget, y = "g_fat_per_winter", threshold_mortality = 24,
                            base_size = 11, temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14) {

  winter <- extract_winter_stats(data_budget, temp_threshold = temp_threshold, split_summer = split_summer,
                                 min_days_trigger_winter = min_days_trigger_winter)

  data_budget$Winter <- data_budget$Date >= winter$start_winter & data_budget$Date <= winter$stop_winter
  data_budget$Budget_cumul <- cumsum(data_budget$Budget * data_budget$Winter)
  data_plot <- data_budget[data_budget$Date >= winter$start_winter - 14 & data_budget$Date < winter$stop_winter + 14, ]

  plot <- ggplot2::ggplot(data_plot) +
    ggplot2::coord_cartesian(xlim = c(winter$start_winter - 14, winter$stop_winter + 14), ylim = c(0, NA)) +
    ggplot2::geom_vline(xintercept = winter$start_winter, linetype = 2, colour = "#0057b7") +
    ggplot2::geom_vline(xintercept = winter$stop_winter, linetype = 2, colour = "#0057b7")

  y <- match.arg(y, c("g_fat_per_state", "g_fat_per_day", "g_fat_per_winter"))

  if (y == "g_fat_per_state") {
    ylab <- expression(atop("Hourly expenditure"~"("*g[fat]*h^{-1}*")"), ",")
    plot <- plot +
        ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$MR_normo), colour = "red") +
        ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$MR_torpor), colour = "blue") +
        ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.05), minor_breaks = NULL)
  } else if (y == "g_fat_per_day") {
    ylab <- expression(atop("Daily expenditure"~"("*g[fat]*day^{-1}*")"), ",")
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.5), minor_breaks = NULL)
  } else if (y == "g_fat_per_winter") {
    ylab <- expression(atop("Cumulative expenditure"~"("*Sigma*g[fat]*")"), ",")
    plot <- plot +
      ggplot2::geom_hline(yintercept = threshold_mortality, linetype = 4, colour = "darkgrey") +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_cumul), data = data_plot[data_plot$Budget_cumul < threshold_mortality, ]) +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_cumul), data = data_plot[data_plot$Budget_cumul > threshold_mortality, ],
                         linetype = 3) +
      ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y),
                         data = data.frame(y = threshold_mortality,
                                           x = data_plot$Date[which(data_plot$Budget_cumul > threshold_mortality)[1]]),
                         shape = 4, size = 5) + # skull: "\U2620"
      ggplot2::scale_y_continuous(breaks = c(threshold_mortality, seq(0, 1000, by = 10)), minor_breaks = NULL)
  } else {
    stop("argument 'y' incorrect. It should be 'g_fat_per_state', 'g_fat_per_day', or 'g_fat_per_winter'")
  }

  plot +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%b 1st",
                          minor_breaks = NULL, limits = c(winter$start_winter - 14, winter$stop_winter + 14)) +
    ggplot2::labs(y = ylab, x = NULL) +
    ggplot2::theme_bw(base_size = base_size)
}


