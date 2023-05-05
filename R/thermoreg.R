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
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_MR_fit(fit_torpor)
#'
plot_MR_fit <- function(fit) {

  estimates <- summarise_MR_fit(fit)

  ## plotting fitted thermoregulatory curves
  suppressWarnings(torpor::tor_plot(fit, plot_type = "ggplot", col_torp = "darkgrey", col_euth = "black", col_Mtnz = "black")) +
  ggplot2::geom_vline(xintercept = estimates[estimates$parameter == "Tlc", "mean"], linetype = "dashed") +
  ggplot2::scale_x_continuous(breaks = seq(-65, 70, 5), minor_breaks = NULL) +
  ggplot2::scale_y_continuous(breaks = 0:7, minor_breaks = NULL) +
  ggplot2::labs(y = expression(paste("Metabolic rate (ml ", CO[2], " ", g^{-1}, h^{-1}, ")")),
                x = "Ambient temperature (\u00B0C)")

  ## we hack the plot produced by torpor
  plot_torpor <- ggplot2::ggplot_build(ggplot2::last_plot())
  plot_torpor$data[[1]]$shape <- 1
  plot_torpor$data[[2]]$linetype <- 1
  plot_torpor$data[[5]]$linetype <- 1
  plot_torpor$data[[8]]$linetype <- 1
  plot_torpor$data[[4]]$shape <- 2
  plot_torpor$data[[7]]$shape <- 2
  ggplotify::as.ggplot(ggplot2::ggplot_gtable(plot_torpor))

}
