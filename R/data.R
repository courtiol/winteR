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
#' @name fit_torpor
#' @docType data
NULL


#' Fitted model predicting probability to be in normothermy
#'
#' A model fitted with [spaMM::fitme()] using the cauchit link.
#'
#' @name fit_normo_cauchit
#' @docType data
NULL


#' Fitted model predicting probability to be in normothermy
#'
#' A model fitted with [spaMM::fitme()] using the probit link.
#'
#' @name fit_normo_probit
#' @docType data
NULL

#' Mask of world land and country borders
#'
#' This dataset contains a polygon shapefile that can be used to plot the
#' borders of CountryBorders and to select land areas.
#'
#' @name lands_polygons
#' @docType data
#' @format A *simple feature collection* object
#' @seealso [oceans_polygons] for another mask
#' @source This object is derived from the package \pkg{rnaturalearth}.
#' @keywords datasets
#' @examples
#' data("lands_polygons", package = "winteR")
#' lands_polygons
NULL


#' Mask of world oceans
#'
#' This dataset contains a polygon shapefile that can be used to mask large
#' bodies of water.
#'
#' @name oceans_polygons
#' @docType data
#' @format A *simple feature collection* object
#' @seealso [lands_polygons] for another mask
#' @source This object is derived from the package \pkg{rnaturalearth}.
#' @keywords datasets
#' @examples
#' data("oceans_polygons", package = "winteR")
#' oceans_polygons
NULL

globalVariables(names = c("lands_polygons", "oceans_polygons"))
