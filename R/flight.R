#' Estimate the excess power that an average common noctule of a given mass has for flying
#'
#' This function is used to determines if an average common noctule can fly or not.
#' If the excess power is positive, it can fly, otherwise, it cannot.
#' If the `output4optim` parameter is set to `TRUE`, the function report the absolute excess power, which allows for searching the maximum mass that a noctule can reach and still be able to fly (see example).
#'
#' @inheritParams arguments
#'
#' @return a data frame with various information about the power and ability of the bat to fly, unless `output4optim = TRUE`
#' @export
#'
#' @examples
#' compute_flightpower_excess() # avg noctule can fly
#' compute_flightpower_excess(50) # 50 g noctule cannot
#' \dontrun{
#' optimize(compute_flightpower_excess, lower = 0, upper = 100, output4optim = TRUE)$minimum # max mass able to fly
#' }
#'
compute_flightpower_excess <- function(mass_g = 26.5, output4optim = FALSE) {

  # generate a 'bird object' representing an average common noctule
  Noctule <- afpt::Bird(name.scientific = "Nyctalus noctula",
                        massEmpty = 26.5/1000, #in kg
                        massFat = (mass_g - 26.5)/1000,
                        wingSpan = 0.34,
                        wingArea = 0.0161,
                        wingAspect = 7.4,
                        coef.bodyDragCoefficient = 0.4,
                        type = 'bat',
                        wingbeatFrequency = 0.0698 * mass_g + 5.554, # wingbeat frequency, from O'Mara et al. 2019: http://dx.doi.org/10.1098/rsos.181942
                        muscleFraction = 0.09,
                        source = "Norberg & Rayner 1987")

  # estimate minimum power required to fly
  required_power <- afpt::findMinimumPowerSpeed(Noctule)[["power"]]

  # estimate power available to fly
  available_power <- afpt::computeAvailablePower(Noctule)

  # excess power
  excess_power <- available_power - required_power
  if (output4optim) return(abs(excess_power))
  data.frame(mass_empty = 26.5, mass_fat = mass_g - 26.5,
             mass_total = mass_g, available_power = available_power, required_power = required_power,
             can_fly = available_power > required_power)
}


#' Plot power available for flight and power required for flight as a function of body mass
#'
#' @inheritParams arguments
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' \dontrun{
#' plot_flightpower()
#' }
#'
plot_flightpower <- function(base_size = 11) {

  print("(this function needs to run for a minute or so... be patient)")
  print("estimating maximum mass for flight...")

  max_total_mass <- optimize(compute_flightpower_excess, lower = 0, upper = 100, output4optim = TRUE)$minimum # max mass able to fly
  print(paste("maximum fat mass =", round(max_total_mass - 26.5, 2), "g"))
  print(paste("maximum total mass =", round(max_total_mass, 2), "g"))

  res_list <- lapply(seq(0, 25, by = 0.1), \(m) compute_flightpower_excess(mass_g = 26.5 + m))
  res_df <- do.call("rbind", res_list)

  res_df |>
    dplyr::select(c("mass_total", "available_power", "required_power")) |>
    dplyr::rename(available = .data$available_power, required = .data$required_power) |>
    tidyr::pivot_longer(-"mass_total") |>
    ggplot2::ggplot() +
      ggplot2::aes(x = .data$mass_total, y = .data$value, colour = .data$name, linetype = .data$name) +
      ggplot2::geom_line(linewidth = 2) +
      ggplot2::geom_vline(xintercept = max_total_mass, linetype = "dotted", colour = "black", linewidth = 2) +
      ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis( ~ . - 26.5, name = "Fat mass above average (g)",
                                                                breaks = seq(0, 100, by = 5)),
                                  breaks = c(26.5, seq(0, 50, by = 5)), minor_breaks = NULL) +
      ggplot2::scale_y_continuous(minor_breaks = NULL, breaks = seq(0, 1, by = 0.1)) +
      ggplot2::scale_color_manual(values = c("orange", "blue")) +
      ggplot2::labs(x = "Total mass (g)", y = "Flight power (W)", colour = "", linetype = "") +
      ggplot2::theme_bw(base_size = base_size) +
      ggplot2::theme(legend.position = "right", legend.key.size = ggplot2::unit(1, "cm"))
}
