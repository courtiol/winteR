#' Compute the distance in km between two latitudes
#'
#' @inheritParams arguments
#'
#' @return a distance in km
#' @export
#'
#' @examples
#' ## Earth circumference through the poles should be 40,007.863 km
#' compute_northward_move(90, -90)*2 # very close!
#'
compute_northward_move <- function(lat, lat_ref = 0, signed = TRUE) {
  lat <- c(lat_ref, lat) # add reference as first location
  radius <- 6371.009 ## Earth radius used for approximation = 6371.009 = 1/3*(2*6378.137+6356.752)  [details on https://en.wikipedia.org/wiki/Great-circle_distance]
  rad_deg <- pi/180 # input should be in degrees, and this converts to radians.
  lat_rad <- lat*rad_deg
  zz <- cbind(cos(lat_rad), 0, sin(lat_rad)) # computation assumed longitude fixed at zero
  pp <- zz %*% t(zz)[, 1] # we only keep first column as we don't want pairwise distances
  pp <- pmin(pmax(pp[, 1], -1), 1) # correct numerical epsilon error
  d <- radius * acos(pp)
  if (signed) {
    d[lat < lat_ref] <- -d[lat < lat_ref]
  }
  d[-1] # remove reference
}

