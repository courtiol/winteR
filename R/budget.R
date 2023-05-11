#' Compute the energy budget during a winter
#'
#' Bugets are computed in KJ per hour and grams of fat consumed. If `data_MR` contains a column
#' `Date`, the function will also compute the cumulative budget during the hibernation season and
#' the predicted survival status.
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' #See ?winteR
#'
compute_budget_df <- function(data_MR, fit_state, fit_MR, roost_insulation_dTa = 5,
                              temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                              threshold_mortality = 27) {

  ## ambient temperature is higher in roost
  data_MR$Ta <- data_MR$Temp + roost_insulation_dTa

  # print(data_MR$Ta[1]) ## for debugging only
  # if (abs(data_MR$Ta[1] -  17.92367) < 1e-6) browser()

  ## predict proba to be in normothermy and in torpor
  prob_normo <- stats::predict(fit_state, newdata = data.frame(Ta = data_MR$Ta), re.form = NA)[,1 ]
  prob_torpor <- 1 - prob_normo
  data_MR$Prob_normo <- prob_normo
  data_MR$Prob_torpor <- prob_torpor

  ## predict metabolic rate in both physiological states
  pred_MR <- suppressWarnings(torpor::tor_predict(fit_MR, Ta = data_MR$Ta))
  pred_MR$pred <- pred_MR$pred
  pred_MR$upr_95 <- pred_MR$upr_95
  pred_MR$lwr_95 <- pred_MR$lwr_95

  # params <- torpor::get_parameters(fit_MR)
  # Tlc <- params[params$parameter == "Tlc", "median"]
  Tlc <- fit_MR$mod_parameter$data$Tlc # Note: torpor::get_parameters(fit_MR) not precise enough!

  data_MR$MR_normo <- NA
  data_MR$MR_normo[data_MR$Ta < Tlc] <- pred_MR[pred_MR$assignment == "Euthermia", "pred"]
  data_MR$MR_normo[data_MR$Ta >= Tlc] <- pred_MR[pred_MR$assignment == "Mtnz", "pred"]

  data_MR$MR_torpor <- 0
  data_MR$MR_torpor[data_MR$Ta < Tlc] <- pred_MR[pred_MR$assignment == "Torpor", "pred"]

  data_MR$Prob_normo[data_MR$Ta >= Tlc] <- 1
  data_MR$Prob_torpor[data_MR$Ta >= Tlc] <- 0

  ## compute budget
  data_MR$Fat_normo <- data_MR$MR_normo/37.7
  data_MR$Fat_torpor <- data_MR$MR_torpor/37.7

  data_MR$Daily_duration_normo <- data_MR$Prob_normo * 24
  data_MR$Daily_duration_torpor <- data_MR$Prob_torpor * 24
  data_MR$Budget_MR <- data_MR$MR_normo * data_MR$Daily_duration_normo + data_MR$MR_torpor * data_MR$Daily_duration_torpor
  data_MR$Budget_fat <- data_MR$Fat_normo * data_MR$Daily_duration_normo + data_MR$Fat_torpor * data_MR$Daily_duration_torpor

  ## if date provided, compute cumulative budget and survival
  if (!is.null(data_MR$Date)) {
    ## compute cumulative energy budget
    winter <- extract_winter_stats(data_MR, temp_threshold = temp_threshold, split_summer = split_summer,
                                   min_days_trigger_winter = min_days_trigger_winter)

    data_MR$Winter <- data_MR$Date >= winter$start_winter & data_MR$Date <= winter$stop_winter
    data_MR$Budget_cumul <- cumsum(data_MR$Budget_fat * data_MR$Winter)

    ## compute survival status
    data_MR$Survival <- TRUE
    date_mortality <- min(c(data_MR$Date[data_MR$Budget_cumul > threshold_mortality], Inf))
    data_MR$Survival[data_MR$Date >= date_mortality] <- FALSE
  }

  ## return
  data_MR
}


#' Compute the total energy budget during a winter and other summary statistics
#'
#' @inheritParams arguments
#'
#' @return a list
#' @export
#'
#' @examples
#' #TODO
#'
compute_budget_summarystats <- function(vec_Temp, vec_Dates,
                                        fit_state, fit_MR,
                                        roost_insulation_dTa = 5,
                                        temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                                        threshold_mortality = 27) {

    d <- data.frame(Temp = vec_Temp, Date = vec_Dates)

    budg <- compute_budget_df(data_MR = d,
                              fit_state = fit_state, fit_MR = fit_MR,
                              roost_insulation_dTa = roost_insulation_dTa,
                              temp_threshold = temp_threshold, split_summer = split_summer,
                              min_days_trigger_winter = min_days_trigger_winter,
                              threshold_mortality = threshold_mortality)

    data.frame(Budget_winter = max(budg$Budget_cumul), Survive = any(!budg$Survival))
}

#' Plot the energy budget
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
#' Tskin_files <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- build_Tskin_table(Tskin_files)
#' data_normothermy <- data_Tskin[data_Tskin$Included, ]
#' fit_normo_cauchit <- spaMM::fitme(Normo ~ Ta + (1|ID), data = data_normothermy,
#'                                  family = binomial(link = "cauchit"))
#' data_nrg <- compute_budget_df(data_Kharkiv, fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#' plot_budget_panel(data_nrg, y = "g_fat_per_state")
#' plot_budget_panel(data_nrg, y = "g_fat_per_day")
#' plot_budget_panel(data_nrg, y = "g_fat_per_winter")
#'
plot_budget_panel <- function(data_budget, y = "g_fat_per_winter", threshold_mortality = 27, base_size = 11) {

  start_winter <- min(data_budget$Date[data_budget$Winter])
  stop_winter <- max(data_budget$Date[data_budget$Winter])

  data_plot <- data_budget[data_budget$Date >= start_winter - 14 & data_budget$Date < stop_winter + 14, ]

  plot <- ggplot2::ggplot(data_plot) +
    ggplot2::coord_cartesian(xlim = c(start_winter - 14, stop_winter + 14), ylim = c(0, NA)) +
    ggplot2::geom_vline(xintercept = start_winter, linetype = 2, colour = "#0057b7") +
    ggplot2::geom_vline(xintercept = stop_winter, linetype = 2, colour = "#0057b7")

  y <- match.arg(y, c("g_fat_per_state", "g_fat_per_day", "g_fat_per_winter"))

  if (y == "g_fat_per_state") {
    ylab <- expression(atop("Hourly fat consumption"~"("*g[fat]*h^{-1}*")"))
    plot <- plot +
        ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Fat_normo), colour = "red") +
        ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Fat_torpor), colour = "blue") +
        ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.05), minor_breaks = NULL)
  } else if (y == "g_fat_per_day") {
    ylab <- expression(atop("Daily fat consumption"~"("*g[fat]*day^{-1}*")"))
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_fat)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.5), minor_breaks = NULL)
  } else if (y == "g_fat_per_winter") {
    ylab <- expression(atop("Cumulative fat consumption"~"("*Sigma*g[fat]*")"))
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
                          minor_breaks = NULL, limits = c(start_winter - 14, stop_winter + 14)) +
    ggplot2::labs(y = ylab, x = NULL) +
    ggplot2::theme_bw(base_size = base_size)
}


