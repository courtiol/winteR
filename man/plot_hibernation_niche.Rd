% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suitability_series.R
\name{plot_hibernation_niche}
\alias{plot_hibernation_niche}
\title{Plot the suitability niche}
\usage{
plot_hibernation_niche(stars_list)
}
\arguments{
\item{stars_list}{a list of stars objects initially produced by \code{\link[=build_suitability_stars]{build_suitability_stars()}}}
}
\value{
a ggplot2
}
\description{
This function combines all winter stars and count for each combination of mean winter temperature
and winter duration (discretised), for how many hibernation seasons an average bat is expected to
survive.
}
\examples{
## See ?winteR

}
