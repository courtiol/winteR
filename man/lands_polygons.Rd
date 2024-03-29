% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{lands_polygons}
\alias{lands_polygons}
\title{Mask of world land and country borders}
\format{
A \emph{simple feature collection} object
}
\source{
This object is derived from the package \pkg{rnaturalearth}.
}
\description{
This dataset contains a polygon shapefile that can be used to plot the
borders of CountryBorders and to select land areas.
}
\examples{
data("lands_polygons", package = "winteR")
lands_polygons
}
\seealso{
\link{oceans_polygons} for another mask
}
\keyword{datasets}
