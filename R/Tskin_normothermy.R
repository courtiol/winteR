#' Load a file containing physiological data on skin temperature
#'
#' This function loads a file containing physiological data that will be used to classify the
#' physiological state of individuals (normothermy vs torpor). In our case it is a time series of
#' skin temperature measurements. Each file name contain ambient temperature value and ID of
#' individual. The file contains output from iButton temperature loggers, which have 5 columns:
#' Date, Time, Time Unit (in either AM or PM), Temperature Unit (in degrees Celsius, in our case),
#' and Temperature Value.
#'
#' This function should not be directly called by the user. It is called internally when calling
#' [build_Tskin_table()]. Note also that for the function to work, the files must be named following
#' the same structure we used.
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#' @seealso [classify_Tskin_state()], [build_Tskin_table()], [clean_Tskin_table()]
#' @examples
#' filespath <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- load_Tskin_datafile(filespath[1])
#' head(data_Tskin)
#'
load_Tskin_datafile <- function(filename) {

  ## extract ambient temperature, ID, and status (i.e. alive or something else) from filename
  name_elements <- strsplit(basename(filename), split = "_")[[1]]
  Ta <- as.numeric(name_elements[2])
  ID <- strsplit(name_elements[3], split = "\\.")[[1]][1]
  Status <- ifelse(length(name_elements) > 3, strsplit(name_elements[4], split = "\\.")[[1]][1], "alive")

  ## read Tskin data
  d <- utils::read.table(filename, header = FALSE, sep = " ", dec = ".")

  ## format dataframe
  names(d) <- c("Date", "Time", "AM_PM", "Unit", "Tskin")
  d$Ta <- Ta
  d$Tskin_max <- max(d$Tskin) ## add max skin temperature
  d$ID <- ID
  d$Status <- Status
  d
}


#' Classify the physiological state of individuals based on skin temperature
#'
#' This function classifies the physiological state based on the difference between the skin
#' temperature and the ambient temperature. If $Tskin > Ta + ((max(Tskin) - Ta) / 2)$ the bat is
#' considered in normothermy, otherwise, it is considered to be in torpor.
#'
#' This function should not be directly called by the user.
#' It is called internally when calling [build_Tskin_table()].
#'
#' @inheritParams arguments
#'
#' @return a vector with the physiological state at each time point
#' @export
#' @seealso [load_Tskin_datafile()], [build_Tskin_table()], [clean_Tskin_table()]
#' @examples
#' filespath <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- load_Tskin_datafile(filespath[1])
#' data_Tskin$State <- classify_Tskin_state(data_Tskin)
#' head(data_Tskin)
#'
classify_Tskin_state <- function(data_Tskin) {

  ## compute threshold for normothermy
  if (length(unique(data_Tskin$Ta)) > 1) {
    stop("Function can only handle one ambient temperature per file")
  }

  Tskin_threshold <- data_Tskin$Ta[1] + ((data_Tskin$Tskin_max[1] - data_Tskin$Ta[1]) / 2)

  ## apply the threshold to define the physiological state and return it
  ifelse(data_Tskin$Tskin >= Tskin_threshold, "normothermy", "torpor")
}


#' Build table with raw physiological data on skin temperature
#'
#' This function creates the table of raw physiological data.
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#' @seealso [load_Tskin_datafile()], [classify_Tskin_state()], [clean_Tskin_table()]
#' @examples
#' files_to_do <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- build_Tskin_table(files_to_do)
#' head(data_Tskin)
#'
build_Tskin_table <- function(filenames, clean = TRUE) {

  ## call load_Tskin_datafile() on every file
  all <- sapply(filenames, function(file) {
    d <- load_Tskin_datafile(filename = file)
    d$State <- classify_Tskin_state(d)
    d$Normo <- as.numeric(d$State == "normothermy")
    d$Index <- seq_len(nrow(d))
    d
    }, simplify = FALSE)

  ## combine the output into a single dataframe
  all <- do.call("rbind", all)
  rownames(all) <- NULL

  ## filter data
  if (clean) {
    all <- clean_Tskin_table(all)
  }

  ## output
  all
}


#' Clean up the physiological data on skin temperature
#'
#' This function removes problematic observations of the physiological state; that is, observations
#' that seem to have happened after the death of the individual and all observations corresponding
#' to the drop of a sensor.
#'
#' This function should not be directly called by the user.
#' It is called internally when calling [build_Tskin_table()].
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#' @seealso [load_Tskin_datafile()], [classify_Tskin_state()], [build_Tskin_table()]
#' @examples
#' files_to_do <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- build_Tskin_table(files_to_do, clean = FALSE)
#' dim(data_Tskin)
#' data_Tskin_cleaned <- clean_Tskin_table(data_Tskin)
#' dim(data_Tskin_cleaned)
#'
clean_Tskin_table <- function(data_Tskin) {

  ## we remove completely observations when sensor is detached
  data_Tskin <- data_Tskin[data_Tskin$Status != "notOnBat", ]
  data_Tskin$Included <- TRUE

  ## we remove last torpor bout for dead individuals as it could correspond to death
  data_dead <- data_Tskin[data_Tskin$Status == "dead", ]

  data_dead |>
    dplyr::mutate(New_bout = (.data$Index == min(.data$Index)) | ((.data$Index - 1) != dplyr::lag(.data$Index)),
                  .by = c("ID", "State")) |>
    dplyr::mutate(Bout = cumsum(.data$New_bout)) |>
    dplyr::mutate(Warm = .data$Tskin > 0.9*.data$Tskin_max, .by = "ID") |>
    dplyr::summarize(Included = any(.data$Warm),
                     Index_max = max(.data$Index),
                     .by = c("ID", "Bout")) |>
    dplyr::summarize(Index_max = max(.data$Index_max[.data$Included]), .by = "ID") -> filter_to_apply

  data_dead <- merge(data_dead, filter_to_apply)
  data_dead$Included <- data_dead$Index <= data_dead$Index_max
  data_dead$Index_max <- NULL

  ## we combine clean data from dead and all data from alive bats
  rbind(data_Tskin[data_Tskin$Status == "alive", ], data_dead)
}

#' Plot the physiological data on skin temperature
#'
#' This function plots the data created with [build_Tskin_table()].
#'
#' @inheritParams arguments
#' @return a plot
#' @export
#'
#' @examples
#' files_to_do <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- build_Tskin_table(files_to_do)
#' plot_Tskin_table(data_Tskin)
#'
plot_Tskin_table <- function(data_Tskin) {
  ggplot2::ggplot(data_Tskin) +
    ggplot2::aes(y = .data$Tskin, x = .data$Index,
                 shape = .data$State, colour = .data$Included) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ ID) +
    ggplot2::theme_bw()
  }


#' Plot a fitted model between ambient temperature and normothermy
#'
#' This function plots the predictions from a model fitting the relationship between ambient
#' temperature and normothermy. It assumes that the predictions are mirrored around a particular
#' ambient temperature value (default = 2 degrees C). We use this function to create Fig 1 and Fig
#' S1 in the paper.
#'
#' @inheritParams arguments

#' @return a plot
#' @export
#'
#' @examples
#' #See ?winteR
#'
plot_Tskin_fit <- function(fit_state, rangeTa = c(-5, 35), Tmirror = 2, base_size = 11) {

  ## computing predictions with confidence interval
  predictions <- stats::predict(fit_state, newdata = data.frame(Ta = seq(Tmirror, max(abs(rangeTa) + 2*Tmirror), by = 0.1)),
                         re.form = NA, binding = "prob", intervals = "predVar")
  predictions <- cbind(predictions, attr(predictions, "intervals"))

  ## mirroring predictions around Tmirror degrees
  predictions_mirrored <- predictions
  predictions_mirrored$Ta <- -predictions_mirrored$Ta + 2*Tmirror
  predictions <- rbind(predictions_mirrored[(nrow(predictions_mirrored) - 1):1, ], predictions)

  ## filtering range of ambient temperature to plot
  predictions <- predictions[predictions$Ta >= min(rangeTa) & predictions$Ta <= max(rangeTa), ]

  ## raw probability to plot
  fit_state$data |>
    dplyr::summarise(prob_normo = mean(.data$Normo), .by = c("ID", "Ta")) -> raw_data

  ## plot
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x = .data$Ta, ymin = .data$predVar_0.025, ymax = .data$predVar_0.975),
                         data = predictions, colour = "lightgrey", alpha = 0.2) +
    ggplot2::geom_line(ggplot2::aes(x = .data$Ta, y = .data$prob), data = predictions, linetype = "dashed") +
    ggplot2::geom_point(ggplot2::aes(x = .data$Ta, y = .data$prob_normo), data = raw_data, shape = 1) +
    ggplot2::coord_trans(xlim = rangeTa, y = "log") +
    ggplot2::scale_x_continuous(breaks = seq(rangeTa[1], rangeTa[2], by = 5), minor_breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = c(0.01*2^(0:6), 1), minor_breaks = NULL,
                                sec.axis = ggplot2::sec_axis(~ . * 30 * 2 * 24,
                                                             name = "Daily time spent in normothermy (min)",
                                                             breaks = c(10*2^(0:6), 60 * 24))) +
    ggplot2::labs(y = "Probability of normothermy", x = "Ambient temperature (\u00B0C)") +
    ggplot2::theme_bw(base_size = base_size)
}
