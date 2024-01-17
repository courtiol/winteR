#' Compute the energy budget during a winter
#'
#' Bugets are computed in KJ and grams of fat consumed per hour (`MR_normo`, `MR_torpor`,
#' `Fat_normo`, `Fat_torpor`) and per day (`Budget_MR` and `Budget_fat`). If `data_MR` contains a
#' column `Date`, the function will also compute the cumulative budget during the hibernation season
#' and the predicted survival status.
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
                              threshold_mortality = 27, huddling_factor = 0.5, Tmirror = 2) {

  ## ambient temperature is higher in roost
  data_MR$Ta <- data_MR$Temp + roost_insulation_dTa

  ## ambient temperature to use for predictions of physiological state (i.e. with mirroring)
  data_MR$Ta_mirrored <- pmax(data_MR$Ta, -data_MR$Ta + 2*Tmirror)

  # print(data_MR$Ta[1]) ## for debugging only
  # if (abs(data_MR$Ta[1] -  17.92367) < 1e-6) browser()

  ## predict proba to be in normothermy and in torpor
  prob_normo <- stats::predict(fit_state, newdata = data.frame(Ta = data_MR$Ta_mirrored), re.form = NA)[,1 ]
  prob_torpor <- 1 - prob_normo
  data_MR$Prob_normo <- prob_normo
  data_MR$Prob_torpor <- prob_torpor

  ## predict metabolic rate in both physiological states
  pred_MR <- suppressWarnings(torpor::tor_predict(fit_MR, Ta = data_MR$Ta, CI = FALSE))
  pred_MR_noMTNZ <- pred_MR[pred_MR$assignment != "Mtnz", ]
  pred_MR_MTNZ <- pred_MR[pred_MR$assignment == "Mtnz", ]

  if (nrow(pred_MR_MTNZ) > 0) {
    pred_MR_MTNZ_torpor <- pred_MR_MTNZ_normo <- pred_MR_MTNZ
    pred_MR_MTNZ_torpor$assignment <- "Torpor"
    pred_MR_MTNZ_normo$assignment <- "Euthermia"
    pred_MR <- rbind(pred_MR_noMTNZ, pred_MR_MTNZ_torpor, pred_MR_MTNZ_normo)
  }

  # params <- torpor::get_parameters(fit_MR)
  # Tlc <- params[params$parameter == "Tlc", "median"]
  Tlc <- fit_MR$mod_parameter$data$Tlc # Note: torpor::get_parameters(fit_MR) not precise enough!
  Tt <- stats::median(fit_MR$mod_parameter$sims.list$Tt) ## approximation of Tt

  data_MR$MR_normo <- NA

  data_MR$MR_normo[data_MR$Ta < Tlc] <- pred_MR[pred_MR$Ta < Tlc & pred_MR$assignment == "Euthermia", "pred"] * huddling_factor
  data_MR$MR_normo[data_MR$Ta >= Tlc] <- pred_MR_MTNZ[pred_MR_MTNZ$Ta >= Tlc & pred_MR_MTNZ$assignment == "Mtnz", "pred"] # no change for huddling needed here

  data_MR$MR_torpor <- NA
  data_MR$MR_torpor[data_MR$Ta >= Tt] <- pred_MR[pred_MR$Ta >= Tt & pred_MR$assignment == "Torpor", "pred"] # no change for huddling needed here
  data_MR$MR_torpor[data_MR$Ta < Tt] <- pred_MR[pred_MR$Ta < Tt & pred_MR$assignment == "Torpor", "pred"] * huddling_factor

  # data_MR$Prob_normo[data_MR$Ta_mirrored >= Tlc] <- 1
  # data_MR$Prob_torpor[data_MR$Ta_mirrored >= Tlc] <- 0

  ## compute budget
  data_MR$Fat_normo <- data_MR$MR_normo/37.7
  data_MR$Fat_torpor <- data_MR$MR_torpor/37.7

  data_MR$Daily_duration_normo <- data_MR$Prob_normo * 24
  data_MR$Daily_duration_torpor <- data_MR$Prob_torpor * 24
  data_MR$Budget_MR_normo <- data_MR$MR_normo * data_MR$Daily_duration_normo
  data_MR$Budget_MR_torpor <- data_MR$MR_torpor * data_MR$Daily_duration_torpor
  data_MR$Budget_MR <- data_MR$Budget_MR_normo + data_MR$Budget_MR_torpor
  data_MR$Budget_fat <- data_MR$Fat_normo * data_MR$Daily_duration_normo + data_MR$Fat_torpor * data_MR$Daily_duration_torpor
  data_MR$Budget_fat_assumingcontinuoustorpor <- data_MR$Fat_torpor * 24

  ## if date provided, compute cumulative budget and survival
  if (!is.null(data_MR$Date)) {
    ## compute cumulative energy budget
    winter <- extract_winter_stats(data_MR, temp_threshold = temp_threshold, split_summer = split_summer,
                                   min_days_trigger_winter = min_days_trigger_winter)

    data_MR$Winter <- data_MR$Date >= winter$start_winter & data_MR$Date <= winter$stop_winter
    data_MR$Budget_cumul <- cumsum(data_MR$Budget_fat * data_MR$Winter)
    data_MR$Budget_cumul_assumingcontinuoustorpor <- cumsum(data_MR$Budget_fat_assumingcontinuoustorpor * data_MR$Winter)

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
#' @return a data frame
#' @export
#'
#' @examples
#' #TODO
#'
compute_budget_summarystats <- function(vec_Temp, vec_Dates,
                                        fit_state, fit_MR,
                                        roost_insulation_dTa = 5,
                                        huddling_factor = 0.5,
                                        temp_threshold = 7, split_summer = "07-01", min_days_trigger_winter = 14,
                                        threshold_mortality = 27) {

    d <- data.frame(Temp = vec_Temp)

    budg <- compute_budget_df(data_MR = d,
                              fit_state = fit_state, fit_MR = fit_MR,
                              roost_insulation_dTa = roost_insulation_dTa,
                              huddling_factor = huddling_factor,
                              temp_threshold = temp_threshold, split_summer = split_summer,
                              min_days_trigger_winter = min_days_trigger_winter,
                              threshold_mortality = threshold_mortality)

    d2 <- data.frame(Temp = vec_Temp, Date = vec_Dates)

    winter <- extract_winter_stats(d2, temp_threshold = temp_threshold, split_summer = split_summer,
                                   min_days_trigger_winter = min_days_trigger_winter)

    ## compute cumulative energy budget
    budg$Date <- d2$Date
    budg$Winter <- budg$Date >= winter$start_winter & budg$Date <= winter$stop_winter
    budg$Budget_cumul <- cumsum(budg$Budget_fat * budg$Winter)

    ## compute survival status
    budg$Survival <- TRUE
    date_mortality <- min(c(budg$Date[budg$Budget_cumul > threshold_mortality], Inf))
    budg$Survival[budg$Date >= date_mortality] <- FALSE

    data.frame(Budget_winter = max(budg$Budget_cumul),
               Survive = all(budg$Survival),
               Start_winter = winter$start_winter,
               Stop_winter = winter$stop_winter,
               Duration_winter = winter$duration_winter,
               Temp_winter_mean = winter$temp_winter_mean,
               Temp_winter_sd = winter$temp_winter_sd,
               Temp_winter_median = winter$temp_winter_median,
               Temp_winter_min = winter$temp_winter_min,
               Temp_winter_max = winter$temp_winter_max,
               Temp_winter_autocorr = winter$temp_winter_autocorr)
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
#' data("fit_torpor", package = "winteR")
#' data_nrg <- compute_budget_df(data_Kharkiv, fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#' plot_budget_panel(data_nrg, y = "g_fat_per_state")
#' plot_budget_panel(data_nrg, y = "g_fat_per_day")
#' plot_budget_panel(data_nrg, y = "g_fat_per_winter")
#'
plot_budget_panel <- function(data_budget, y = "g_fat_per_winter", threshold_mortality = 27, base_size = 11, y_max = NA) {

  start_winter <- min(data_budget$Date[data_budget$Winter])
  stop_winter <- max(data_budget$Date[data_budget$Winter])

  data_plot <- data_budget[data_budget$Date >= start_winter - 14 & data_budget$Date < stop_winter + 14, ]

  plot <- ggplot2::ggplot(data_plot) +
    ggplot2::coord_cartesian(xlim = c(start_winter - 14, stop_winter + 14), ylim = c(0, y_max)) +
    ggplot2::geom_vline(xintercept = start_winter, linetype = 2, colour = "#0057b7") +
    ggplot2::geom_vline(xintercept = stop_winter, linetype = 2, colour = "#0057b7")

  y <- match.arg(y, c("g_fat_per_state", "g_fat_per_day", "g_fat_per_winter"))

  if (y == "g_fat_per_state") {
    ylab <- expression(atop("Hourly fat consumption"~"("*g*.*h^{-1}*")"))
    plot <- plot +
        ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Fat_normo), colour = "red") +
        ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Fat_torpor), colour = "blue") +
        ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.025), minor_breaks = NULL)
  } else if (y == "g_fat_per_day") {
    ylab <- expression(atop("Daily fat consumption"~"("*g*.*day^{-1}*")"))
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_fat_assumingcontinuoustorpor), colour = "blue") +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_fat)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.25), minor_breaks = NULL)
  } else if (y == "g_fat_per_winter") {
    date_death <- data_plot$Date[which(data_plot$Budget_cumul > threshold_mortality)[1]]
    print(paste("death date =", date_death))
    date_death_torpor <- data_plot$Date[which(data_plot$Budget_cumul_assumingcontinuoustorpor > threshold_mortality)[1]]
    print(paste("death date torpor only =", date_death_torpor))
    ylab <- expression(atop("Cumulative fat consumption"~"("*g*.*Sigma*" "*day^{-1}*")"))
    plot <- plot +
      ggplot2::geom_hline(yintercept = threshold_mortality, linetype = 4, colour = "darkgrey") +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_cumul), data = data_plot[data_plot$Budget_cumul < threshold_mortality, ]) +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_cumul), data = data_plot[data_plot$Budget_cumul > threshold_mortality, ],
                         linetype = 3) +
      ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y),
                         data = data.frame(y = threshold_mortality,
                                           x = date_death),
                         shape = 4, size = 5) + # skull: "\U2620"
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_cumul_assumingcontinuoustorpor), data = data_plot[data_plot$Budget_cumul_assumingcontinuoustorpor < threshold_mortality, ],
                         colour = "blue") +
      ggplot2::geom_line(ggplot2::aes(x = .data$Date, y = .data$Budget_cumul_assumingcontinuoustorpor), data = data_plot[data_plot$Budget_cumul_assumingcontinuoustorpor > threshold_mortality, ],
                         colour = "blue", linetype = 3) +
      ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y),
                          data = data.frame(y = threshold_mortality,
                                            x = date_death_torpor),
                          colour = "blue", shape = 4, size = 5) + # skull: "\U2620"
      ggplot2::scale_y_continuous(breaks = c(threshold_mortality, seq(0, 1000, by = 10)), minor_breaks = NULL)
  } else {
    stop("argument 'y' incorrect. It should be 'g_fat_per_state', 'g_fat_per_day', or 'g_fat_per_winter'")
  }

  plot +
    ggplot2::scale_x_date(date_breaks = "1 months", date_labels = "%b",
                          minor_breaks = NULL, limits = c(start_winter - 14, stop_winter + 14)) +
    ggplot2::labs(y = ylab, x = NULL) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0), plot.margin = ggplot2::margin(l = 15))
}


#' Plot budget curves
#'
#' @inheritParams arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' data("fit_torpor", package = "winteR")
#' data("fit_normo_cauchit", package = "winteR")
#' plot_budget_curves(fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'
plot_budget_curves <- function(fit_state, fit_MR, rangeTa = c(-15, 35), Tmirror = 2, base_size = 11) {

  xlab <- "Ambient temperature (\u00B0C)"
  ylab <- expression(atop("Daily fat consumption"~"("*g*.*day^{- 1}*")"))

  budg <- compute_budget_df(data.frame(Temp = seq(rangeTa[1], rangeTa[2], by = 0.01)),
                                       roost_insulation_dTa = 0, # note remove insulation
                                       fit_state = fit_state,
                                       fit_MR = fit_MR, huddling_factor = 1)

  budg <- budg[, c("Ta", "Budget_fat", "Budget_fat_assumingcontinuoustorpor")]
  budg_long <- tidyr::pivot_longer(budg, cols = c("Budget_fat", "Budget_fat_assumingcontinuoustorpor"))
  budg_long$name <- factor(budg_long$name, levels = c("Budget_fat_assumingcontinuoustorpor", "Budget_fat"))

  par <- torpor::get_parameters(fit_MR)
  Tt <- par[par$parameter == "Tt", "mean"]

  ggplot2::ggplot(budg_long) +
    ggplot2::geom_vline(xintercept = Tt, linetype = 2) +
    ggplot2::geom_line(ggplot2::aes(x = .data$Ta, y = .data$value, colour = .data$name)) +
    ggplot2::labs(y = ylab, x = NULL, colour = "State") +
    ggplot2::scale_x_continuous(breaks = seq(rangeTa[1], rangeTa[2], by = 5), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = seq(0, 10, by = 0.5), minor_breaks = NULL) +
    ggplot2::scale_colour_manual(values = c("blue", "black")) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(#plot.margin = ggplot2::margin(l = 15),
                   legend.position = "none") +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)
}


