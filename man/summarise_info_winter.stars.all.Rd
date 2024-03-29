% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handleStars.R
\name{summarise_info_winter.stars.all}
\alias{summarise_info_winter.stars.all}
\title{Summarise winter information}
\usage{
summarise_info_winter.stars.all(directory_stars, mask = NULL)
}
\arguments{
\item{directory_stars}{the destination location for the rds files storing the stars object}

\item{mask}{a MULTIPOLYGON object to be used as a mask}
}
\value{
a dataframe
}
\description{
This function summarises the information contained in a stars object produced with \code{\link[=compute_budget_stars]{compute_budget_stars()}}.
}
\examples{
run <- FALSE
if (run) {
  data(lands_polygons, package = "winteR")
  summarise_info_winter.stars.all("../NC/stars_winter", mask = lands_polygons)
}

}
