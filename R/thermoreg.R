#' Build table with raw physiological data on metabolic rate
#'
#' This function loads the file containing physiological data that will be used to fit the
#' thermoregulatory curves and it converts the metabolic rate in various units. It should be
#' structured as follows: TODO Kseniia, please add description XXX .
#'
#' @inheritParams load_Tskin_datafile
#' @return a dataframe
#' @export
#' @examples
#' filepath <- list.files(system.file("extdata/thermoreg", package = "winteR"), full.names = TRUE)[1]
#' data_MR <- build_MR_datafile(filepath)
#' head(data_MR)
#'
build_MR_datafile <- function(filename) {
  ## read MR data
  d <- utils::read.table(filename, header = TRUE, sep = " ", dec = ".")
  d |>
    dplyr::rename(VCO2_ml_g_h = .data$MR) |>
    dplyr::mutate(VO2_ml_g_h = .data$VCO2_ml_g_h/0.7,
                  VCO2_L_g_h = .data$VCO2_ml_g_h/1000,
                  VO2_L_g_h = .data$VO2_ml_g_h/1000,
                  kJ_g_h = .data$VCO2_L_g_h*27.8,
                  J_g_h = .data$VCO2_ml_g_h*27.8,
                  fat_g_g = .data$kJ_g_h/37.7,
                  fat_mg_g = .data$J_g_h/37.7,
                  kj_h = .data$kJ_g_h * 30.78,
                  ID2 = as.factor(.data$ID2)) |>
    dplyr::select("ID" = .data$ID2, .data$Ta, .data$Tb,
                  .data$VCO2_ml_g_h, .data$VO2_ml_g_h, .data$VCO2_L_g_h, .data$VO2_L_g_h,
                  .data$kJ_g_h, .data$J_g_h, .data$fat_g_g, .data$fat_mg_g, .data$kj_h)
}


#' Extract parameters from model fitted with torpor
#'
#' @param fit a model fit produced by [torpor::tor_fit()]
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' summarise_MR_fit(fit_torpor)
#'
summarise_MR_fit <- function(fit) {
  estimates <- torpor::get_parameters(fit)
  estimates <- estimates[estimates$parameter %in% c("Tlc", "Tbt", "Tt", "TMR"), ]
  estimates$info[estimates$parameter == "Tlc"] <- "minimum ambient temperature in thermoneutral zone"
  estimates$info[estimates$parameter == "Tbt"] <- "body temperature at minimal metabolic rate"
  estimates$info[estimates$parameter == "Tt"] <- "temperature at minimal metabolic rate"
  estimates$info[estimates$parameter == "TMR"] <- "minimal metabolic rate in torpor"
  estimates <- estimates[, c(ncol(estimates), 1:(ncol(estimates) - 1))]
  rownames(estimates) <- NULL
  estimates
}


#' Plot thermoregulatory curves
#'
#' @inheritParams summarise_MR_fit
#' @inheritParams plot_Tskin_fit
#' @param data the data used to fit the thermoregulatory curves as produced by [build_MR_datafile()]
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' filepath <- list.files(system.file("extdata/thermoreg", package = "winteR"), full.names = TRUE)[1]
#' data_MR <- build_MR_datafile(filepath)
#' plot_MR_fit(fit = fit_torpor, data = data_MR)
#'
plot_MR_fit <- function(fit, data, rangeTa = c(-5, 35), base_size = 11) {

  ## code adapted from torpor::tor_plot()

  xlab <- "Ambient temperature (\u00B0C)"
  ylab <- expression(paste("Metabolic rate (kJ ", h^{-1}, ")"))

  da <- torpor::tor_assign(fit)

  if (!all(data$Ta == da$measured_Ta)) stop("The object data is not consistent with the one used to fit the model")
  da_extended <- cbind(da, data)

  pred <- suppressWarnings(torpor::tor_predict(fit, seq(min(da_extended$measured_Ta),
                                                        max(da_extended$measured_Ta), length = 1000)))

  min_euther <- min(da_extended$measured_Ta[da_extended$assignment == "Euthermia"])

  par <- torpor::get_parameters(fit)
  Tt <- par[par$parameter == "Tt", "mean"]

  ggplot2::ggplot() +
      ggplot2::ylim(range(c(da_extended$measured_M, pred$upr_95, pred$lwr_95))) +
      ggplot2::geom_line(data = pred[pred$assignment == "Torpor", ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "darkgrey") +
      ggplot2::geom_ribbon(data = pred[pred$assignment == "Torpor", ],
                           ggplot2::aes(x = .data$Ta, ymin = .data$lwr_95, ymax = .data$upr_95), fill =  "darkgrey",
                           alpha = 0.2, col = NA) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Torpor", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID, shape = .data$ID)) +
      ggplot2::geom_line(data = pred[pred$assignment == "Euthermia" & pred$Ta > min_euther, ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "black",
                         linetype = 1) +
      ggplot2::geom_line(data = pred[pred$assignment == "Euthermia" & pred$Ta < min_euther, ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "black",
                         linetype = 3) +
      ggplot2::geom_ribbon(data = pred[pred$assignment == "Euthermia", ],
                           ggplot2::aes(x = .data$Ta, ymin = .data$lwr_95, ymax = .data$upr_95), fill = "black",
                           alpha = 0.2, col = NA) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Euthermia", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID, shape = .data$ID)) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Mtnz", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID, shape = .data$ID)) +
      ggplot2::geom_line(data = pred[pred$assignment == "Mtnz", ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "black",
                         linetype = 1) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Undefined", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID), shape = 4) +
      ggplot2::geom_vline(xintercept = Tt, linetype = 2) +
      ggplot2::scale_x_continuous(breaks = seq(rangeTa[1], rangeTa[2], by = 5), minor_breaks = NULL,
                                  limits = range(c(rangeTa, da_extended$measured_Ta))) +
      ggplot2::scale_y_continuous(breaks = seq(floor(min(c(da_extended$measured_M, pred$upr_95, pred$lwr_95))),
                                               ceiling(max(c(da_extended$measured_M, pred$upr_95, pred$lwr_95))),
                                               by = 0.5),
                                  minor_breaks = NULL) +
      ggplot2::scale_shape_manual(values = seq_along(unique(data$ID))) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme(legend.position = "none")

}


#' Plot ambient vs skin temperature
#'
#' @inheritParams plot_MR_fit
#' @param rangeTskin the range of skin temperature to consider in plot
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' filepath <- list.files(system.file("extdata/thermoreg", package = "winteR"), full.names = TRUE)[1]
#' data_MR <- build_MR_datafile(filepath)
#' plot_TaTskin_data(data_MR)
#'
plot_TaTskin_data <- function(data, rangeTa = c(-5, 35), rangeTskin = c(0, 40), base_size = 11) {

  xlab <- "Ambient temperature (\u00B0C)"
  ylab <- "Skin temperature (\u00B0C)"

  ggplot2::ggplot(data) +
    ggplot2::aes(y = .data$Tb, x = .data$Ta, colour = .data$ID, shape = .data$ID) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(breaks = seq(rangeTa[1], rangeTa[2], by = 5), minor_breaks = NULL,
                                limits = range(c(rangeTa, data$Ta))) +
    ggplot2::scale_y_continuous(breaks = seq(rangeTskin[1], rangeTskin[2], by = 5), minor_breaks = NULL,
                                limits = range(c(rangeTskin, data$Tb))) +
    ggplot2::scale_shape_manual(values = seq_along(unique(data$ID))) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.position = "none")
}

