#' Build table with raw physiological data on metabolic rate
#'
#' This function loads the file containing physiological data that will be used to fit the thermoregulatory curves.
#' It should be structured as follows: TODO Kseniia, please add description XXX .
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
  utils::read.table(filename, header = TRUE, sep = " ", dec = ".")
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
plot_MR_fit <- function(fit, data) {

  ## code adapted from torpor::tor_plot()

  xlab <- "Ambient temperature (\u00B0C)"
  ylab <- expression(paste("Metabolic rate (ml ", CO[2], " ", g^{-1}, h^{-1}, ")"))

  da <- torpor::tor_assign(fit)

  if (!all(data$Ta == da$measured_Ta)) stop("The object data is not consistent with the one used to fit the model")
  da_extended <- cbind(da, data)
  da_extended$ID2 <- as.factor(da_extended$ID2)

  pred <- suppressWarnings(torpor::tor_predict(fit, seq(min(da_extended$measured_Ta, na.rm = TRUE),
                                                        max(da_extended$measured_Ta, na.rm = TRUE), length = 1000)))

  min_euther <- min(da_extended$measured_Ta[da_extended$assignment == "Euthermia"], na.rm = TRUE)

  par <- torpor::get_parameters(fit)
  Tt <- par[par$parameter == "Tt", "mean"]

  ggplot2::ggplot() +
      ggplot2::xlim(range(da_extended$measured_Ta, na.rm = TRUE)) +
      ggplot2::ylim(range(c(da_extended$MR, pred$upr_95, pred$lwr_95))) +
      ggplot2::geom_line(data = pred[pred$assignment == "Torpor", ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "darkgrey") +
      ggplot2::geom_ribbon(data = pred[pred$assignment == "Torpor", ],
                           ggplot2::aes(x = .data$Ta, ymin = .data$lwr_95, ymax = .data$upr_95), fill =  "darkgrey",
                           alpha = 0.2, col = NA) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Torpor", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID2), shape = 1) +
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
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID2), shape = 1) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Mtnz", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID2), shape = 1) +
      ggplot2::geom_line(data = pred[pred$assignment == "Mtnz", ],
                         ggplot2::aes(x = .data$Ta, y = .data$pred), col = "black",
                         linetype = 1) +
      ggplot2::geom_point(data = da_extended[da_extended$assignment == "Undefined", ],
                          ggplot2::aes(x = .data$measured_Ta, y = .data$measured_M, colour = .data$ID2), shape = 4) +
      ggplot2::geom_vline(xintercept = Tt, linetype = 2) +
      ggplot2::theme_light() +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::theme(legend.position = "none")

}
