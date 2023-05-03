#' Load a file containing the physiological data
#'
#' TODO Kseniia: explain logger the structure of the files.
#' This function should not be directly called by the user.
#' It is called internally when calling [build_physio_table()].
#'
#' Note: for the function to work, the files must be named following the same structure we used.
#'
#' @param filename the name of the data to be imported
#' @return a dataframe
#' @export
#' @seealso [classify_physio_state()], [build_physio_table()], [clean_physio_table()]
#' @examples
#' filespath <- list.files(system.file("extdata/physio", package = "winteR"), full.names = TRUE)
#' data_physio <- load_physio_datafile(filespath[1])
#' head(data_physio)
#'
load_physio_datafile <- function(filename) {

  ## extract ambient temperature, ID, and status (i.e. alive or something else) from filename
  name_elements <- strsplit(filename, split = "_")[[1]]
  Ta <- as.numeric(name_elements[2])
  ID <- strsplit(name_elements[3], split = "\\.")[[1]][1]
  Status <- ifelse(length(name_elements) > 3, strsplit(name_elements[4], split = "\\.")[[1]][1], "alive")

  ## read physio data
  d <- utils::read.table(filename, header = FALSE, sep = " ", dec = ".")

  ## format dataframe
  names(d) <- c("Date", "Time", "AM_PM", "Unit", "Tb")
  d$Ta <- Ta
  d$Tb_max <- max(d$Tb) ## add max body temperature
  d$ID <- ID
  d$Status <- Status
  d
}


#' Classify the physiological state of individuals
#'
#' This function classifies the physiological state based on the difference between the body temperature
#' and the ambient temperature. If $Tb > Ta + ((max(Tb) - Ta) / 2)$ the bat is considered in normothermy,
#' otherwise, it is considered to be in torpor.
#' This function should not be directly called by the user.
#' It is called internally when calling [build_physio_table()].
#'
#' @param data a file created with [load_physio_datafile()]
#' @return a vector with the physiological state at each time point
#' @export
#' @seealso [load_physio_datafile()], [build_physio_table()], [clean_physio_table()]
#' @examples
#' filespath <- list.files(system.file("extdata/physio", package = "winteR"), full.names = TRUE)
#' data_physio <- load_physio_datafile(filespath[1])
#' data_physio$State <- classify_physio_state(data_physio)
#' head(data_physio)
#'
classify_physio_state <- function(data) {

  ## compute threshold for normothermy
  if (length(unique(data$Ta)) > 1) {
    stop("Function can only handle one ambient temperature per file")
  }

  Tb_threshold <- data$Ta[1] + ((data$Tb_max[1] - data$Ta[1]) / 2)

  ## apply the threshold to define the physiological state and return it
  ifelse(data$Tb >= Tb_threshold, "normothermy", "torpor")
}


#' Build table with raw physiological data
#'
#' This function creates the table of raw physiological data.
#'
#' @param files a vector of paths to the files storing the physiological data
#' @param clean a boolean indicating if the data need to be filter by [clean_physio_table()] or not (default = `TRUE`)
#' @return a dataframe
#' @export
#' @seealso [load_physio_datafile()], [classify_physio_state()], [clean_physio_table()]
#' @examples
#' files_to_do <- list.files(system.file("extdata/physio", package = "winteR"), full.names = TRUE)
#' data_physio <- build_physio_table(files_to_do)
#' head(data_physio)
#'
build_physio_table <- function(files, clean = TRUE) {

  ## call load_physio_datafile() on every file
  all <- sapply(files, function(file) {
    d <- load_physio_datafile(filename = file)
    d$State <- classify_physio_state(d)
    d$Index <- seq_len(nrow(d))
    d
    }, simplify = FALSE)

  ## combine the output into a single dataframe
  all <- do.call("rbind", all)
  rownames(all) <- NULL

  ## filter data
  if (clean) {
    all <- clean_physio_table(all)
  }

  ## output
  all
}


#' Clean up the physiological data
#'
#' This function removes problematic observations of the physiological state; that is, observations
#' that seem to have happened after the death of the individual and all observations corresponding
#' to the drop of a sensor.
#' This function should not be directly called by the user.
#' It is called internally when calling [build_physio_table()].
#'
#' @param data the dataframe produced by [build_physio_table()]
#' @return a dataframe
#' @export
#' @seealso [load_physio_datafile()], [classify_physio_state()], [build_physio_table()]
#' @examples
#' files_to_do <- list.files(system.file("extdata/physio", package = "winteR"), full.names = TRUE)
#' data_physio <- build_physio_table(files_to_do, clean = FALSE)
#' dim(data_physio)
#' data_physio_cleaned <- clean_physio_table(data_physio)
#' dim(data_physio_cleaned)
#'
clean_physio_table <- function(data) {

  ## we remove completely observations when sensor is detached
  data <- data[data$Status != "notOnBat", ]

  ## we remove last torpor bout for dead individuals as it could correspond to death
  data_dead <- data[data$Status == "dead", ]

  #data_dead$last_obs <- as.numeric(stats::ave(data_dead$State, data_dead$ID, FUN = function(x) rev(which(x == "normothermy"))[1]))
  #data_dead$Include <- data_dead$Index <= data_dead$last_obs
  #data_dead$last_obs <- data_dead$Include <- NULL

  ## we combine clean data from dead and all data from alive bats
  rbind(data[data$Status == "alive", ], data_dead)
}

