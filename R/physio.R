#' Load a file containing physiological data
#'
#' This function loads a file containing physiological data that will be used to classify the physiological state of individuals (normothermy vs torpor).
#' In our case it is a time series of skin temperature measurements.
#' Each file name contain ambient temperature value and ID of individual.
#' The file contains output from iButton temperature loggers, which have 5 columns:
#' Date, Time, Time Unit (in either AM or PM), Temperature Unit (in degrees Celsius, in our case), and Temperature Value.
#'
#' This function should not be directly called by the user.
#' It is called internally when calling [build_physio_table()].
#' Note also that for the function to work, the files must be named following the same structure we used.
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
#'
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
#'
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
  data$Included <- TRUE

  ## we remove last torpor bout for dead individuals as it could correspond to death
  data_dead <- data[data$Status == "dead", ]

  data_dead |>
    dplyr::mutate(New_bout = (.data$Index == min(.data$Index)) | ((.data$Index - 1) != dplyr::lag(.data$Index)),
                  .by = c(.data$ID, .data$State)) |>
    dplyr::mutate(Bout = cumsum(.data$New_bout)) |>
    dplyr::mutate(Warm = .data$Tb > 0.9*.data$Tb_max, .by = .data$ID) |>
    dplyr::summarize(Included = any(.data$Warm),
                     Index_max = max(.data$Index),
                     .by = c(.data$ID, .data$Bout)) |>
    dplyr::summarize(Index_max = max(.data$Index_max[.data$Included]), .by = .data$ID) -> filter_to_apply

  data_dead <- merge(data_dead, filter_to_apply)
  data_dead$Included <- data_dead$Index <= data_dead$Index_max
  data_dead$Index_max <- NULL

  ## we combine clean data from dead and all data from alive bats
  rbind(data[data$Status == "alive", ], data_dead)
}

#' Plot the physiological data
#'
#' @param data the dataframe produced by [build_physio_table()]
#' @return a plot
#' @export
#'
#' @examples
#' files_to_do <- list.files(system.file("extdata/physio", package = "winteR"), full.names = TRUE)
#' data_physio <- build_physio_table(files_to_do)
#' plot_physio_table(data_physio)
#'
plot_physio_table <- function(data) {
  ggplot2::ggplot(data) +
    ggplot2::aes(y = .data$Tb, x = .data$Index,
                 shape = .data$State, colour = .data$Included) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ ID)
  }

