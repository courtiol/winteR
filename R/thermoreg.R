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

