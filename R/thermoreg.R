#' Build table with raw physiological data on metabolic rate
#'
#' This function loads the file containing physiological data that will be used to fit the
#' thermoregulatory curves and it converts the metabolic rate in various units. It should be
#' structured as follows: TODO Kseniia, please add description XXX .
#'
#' @inheritParams arguments
#' @return a dataframe
#' @export
#' @examples
#' filepath <- paste0(system.file("extdata/thermoreg", package = "winteR"),
#'                    "/metabolic_rate.txt")
#' data_MR <- build_MR_table(filepath)
#' head(data_MR)
#'
build_MR_table <- function(filename) {
  ## read MR data
  d <- utils::read.table(filename, header = TRUE, sep = "\t", dec = ".")
  d |>
    dplyr::rename(VCO2_ml_h = .data$Whole_animal_VCO2) |>
    dplyr::mutate(VCO2_L_h = .data$VCO2_ml_h/1000,
                  kJ_h = .data$VCO2_L_h*27.8,
                  fat_h = .data$kJ_h/37.7,
                  ID2 = as.factor(.data$ID2)) |>
    dplyr::select(ID = "ID2", "Ta", Tskin = "Tb",
                  "VCO2_ml_h", "VCO2_L_h",
                  "kJ_h", "kJ_h", "fat_h")
}


#' Extract parameters from model fitted with torpor
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' data("fit_torpor", package = "winteR")
#' summarise_MR_fit(fit_torpor)
#'
summarise_MR_fit <- function(fit_MR) {
  estimates <- torpor::get_parameters(fit_MR)
  estimates <- estimates[estimates$parameter %in% c("Tlc", "Tbt", "Tt", "TMR"), ]
  estimates$info[estimates$parameter == "Tlc"] <- "minimum ambient temperature in thermoneutral zone"
  estimates$info[estimates$parameter == "Tbt"] <- "skin temperature at minimal metabolic rate"
  estimates$info[estimates$parameter == "Tt"] <- "temperature at minimal metabolic rate"
  estimates$info[estimates$parameter == "TMR"] <- "minimal metabolic rate in torpor"
  estimates <- estimates[, c(ncol(estimates), 1:(ncol(estimates) - 1))]
  rownames(estimates) <- NULL
  estimates
}


#' Plot thermoregulatory curves
#'
#' @inheritParams arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' filepath <- paste0(system.file("extdata/thermoreg", package = "winteR"),
#'                    "/metabolic_rate.txt")
#' data_MR <- build_MR_table(filepath)
#' data("fit_torpor", package = "winteR")
#' plot_MR_fit(fit_MR = fit_torpor, data_MR = data_MR)
#'
plot_MR_fit <- function(fit_MR, data_MR, rangeTa = c(-5, 35), base_size = 11) {

  ## code adapted from torpor::tor_plot()

  xlab <- "Ambient temperature (\u00B0C)"
  ylab1 <- expression(atop("Metabolic rate"~(k*J*.*h^{-1})))
  #ylab2 <- expression(atop("Fat consumption"~(g[fat]*h^{-1})))
  ylab2 <- expression(atop("Fat consumption"~(g*.*h^{-1})))

  da <- torpor::tor_assign(fit_MR)

  if (!all(data_MR$Ta == da$measured_Ta)) stop("The object data_MR is not consistent with the one used to fit the model")
  da_extended <- cbind(da, data_MR)

  pred <- suppressWarnings(torpor::tor_predict(fit_MR, seq(min(da_extended$measured_Ta),
                                                        max(da_extended$measured_Ta), length = 1000)))

  min_euther <- min(da_extended$measured_Ta[da_extended$assignment == "Euthermia"])

  par <- torpor::get_parameters(fit_MR)
  Tt <- par[par$parameter == "Tt", "mean"]

  ggplot2::ggplot() +
      ggplot2::ylim(range(c(da_extended$measured_M, pred$upr_95, pred$lwr_95))) +
      ggplot2::geom_vline(xintercept = Tt, linetype = 2) +
      ggplot2::geom_line(data = pred[pred$assignment == "Mtnz", ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "black",
                         linetype = 1) +
      ggplot2::geom_line(data = pred[pred$assignment == "Torpor", ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "darkgrey") +
      ggplot2::geom_line(data = pred[pred$assignment == "Euthermia" & pred$Ta > min_euther, ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "black",
                         linetype = 1) +
      ggplot2::geom_line(data = pred[pred$assignment == "Euthermia" & pred$Ta < min_euther, ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "black",
                         linetype = 3) +
      ggplot2::geom_ribbon(data = pred[pred$assignment == "Torpor", ],
                           ggplot2::aes(x = .data$Ta, ymin = .data$lwr_95, ymax = .data$upr_95), fill =  "darkgrey",
                           alpha = 0.2, col = NA) +
      ggplot2::geom_ribbon(data = pred[pred$assignment == "Euthermia", ],
                           ggplot2::aes(x = .data$Ta, ymin = .data$lwr_95, ymax = .data$upr_95), fill = "black",
                           alpha = 0.2, col = NA) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Torpor", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, shape = .data$ID), colour = "blue", size = 1) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Euthermia", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, shape = .data$ID), colour = "red", size = 1) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Mtnz", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, shape = .data$ID), colour = "red", size = 1) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Undefined", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID), shape = 4) +
      ggplot2::scale_x_continuous(breaks = seq(rangeTa[1], rangeTa[2], by = 5), minor_breaks = NULL,
                                  limits = range(c(rangeTa, da_extended$measured_Ta))) +
      ggplot2::scale_y_continuous(breaks = seqy <- seq(floor(min(c(da_extended$measured_M, pred$upr_95, pred$lwr_95))),
                                               ceiling(max(c(da_extended$measured_M, pred$upr_95, pred$lwr_95))),
                                               by = 0.5),
                                  minor_breaks = NULL,
                                  sec.axis = ggplot2::sec_axis(~ . /37.7, breaks = round(seqy/37.7, digits = 3), name = ylab2)) +
      ggplot2::scale_shape_manual(values = seq_along(unique(data_MR$ID))) +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab1) +
      ggplot2::theme(legend.position = "none")

}


#' Plot ambient vs skin temperature
#'
#' @inheritParams arguments
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' filepath <- paste0(system.file("extdata/thermoreg", package = "winteR"),
#'                    "/metabolic_rate.txt")
#' data_MR <- build_MR_table(filepath)
#' data("fit_torpor", package = "winteR")
#' plot_TaTskin_data(fit_MR = fit_torpor, data_MR = data_MR)
#'
plot_TaTskin_data <- function(fit_MR, data_MR, rangeTa = c(-5, 35), rangeTskin = c(0, 40), base_size = 11) {

  xlab <- "\nAmbient temperature (\u00B0C)"
  ylab <- "Skin temperature (\u00B0C)\n"

  da <- torpor::tor_assign(fit_MR)

  if (!all(data_MR$Ta == da$measured_Ta)) stop("The object data_MR is not consistent with the one used to fit the model")
  da_extended <- cbind(da, data_MR)

  ggplot2::ggplot() +
    ggplot2::geom_abline(slope = 1, linetype = 2) +
    ggplot2::geom_point(data = da_extended[da_extended$assignment == "Euthermia", ],
                        ggplot2::aes(y = .data$Tskin, x = .data$Ta, colour = .data$ID, shape = .data$ID), size = 3) +
    ggplot2::geom_point(data = da_extended[da_extended$assignment == "Mtnz", ],
                        ggplot2::aes(y = .data$Tskin, x = .data$Ta, colour = .data$ID, shape = .data$ID), size = 3) +
    ggplot2::geom_point(data = da_extended[da_extended$assignment == "Torpor", ],
                        ggplot2::aes(y = .data$Tskin, x = .data$Ta, colour = .data$ID, shape = .data$ID), size = 1) +
    ggplot2::geom_point(data = da_extended[da_extended$assignment == "Undefined", ],
                        ggplot2::aes(x = .data$Ta, y = .data$Tskin, colour = .data$ID), shape = 4) +
    ggplot2::scale_x_continuous(breaks = seq(rangeTa[1], rangeTa[2], by = 5), minor_breaks = NULL,
                                limits = range(c(rangeTa, da_extended$Ta))) +
    ggplot2::scale_y_continuous(breaks = seq(rangeTskin[1], rangeTskin[2], by = 5), minor_breaks = NULL,
                                limits = range(c(rangeTskin, da_extended$Tskin))) +
    ggplot2::scale_shape_manual(values = seq_along(unique(da_extended$ID))) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(legend.position = "none")

}


#' Keep physiological data corresponding to lowest metabolic rate per ambient temperature
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' filepath <- paste0(system.file("extdata/thermoreg", package = "winteR"),
#'                    "/metabolic_rate.txt")
#' data_MR <- build_MR_table(filepath)
#' nrow(data_MR)
#' data("fit_torpor", package = "winteR")
#' data_MR_filtered <- filter_MR_table(data_MR, fit_torpor)
#' nrow(data_MR_filtered)
#'
filter_MR_table <- function(data_MR, fit_MR) {
  data_MR$State <- torpor::tor_assign(fit_MR)$assignment
  data_MR |>
    dplyr::mutate(round_Ta = round(.data$Ta)) |>
    dplyr::slice_min(.data$kJ_h, by = c("ID", "State", "round_Ta"))
}
