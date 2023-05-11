#' Fitted thermoregulatory curves
#'
#' A model fitted with [torpor::tor_fit()].
#'
#' @format ## `fit_torpor`
#' A list with two elements
#' \describe{
#'   \item{params}{parameters estimated by [torpor::tor_fit()]}
#'   \item{ppo}{posteriors overlap values}
#' }
"fit_torpor"


#' Fitted model predicting probability to be in normothermy
#'
#' A model fitted with [spaMM::fitme()].
#'
"fit_normo_cauchit"
