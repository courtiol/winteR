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

#' A mask with country borders for the world
"mask_country"

#' A mask with large water bodies the world
"mask_ocean"
