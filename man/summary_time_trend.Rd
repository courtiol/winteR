% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time_trends.R
\name{summary_time_trend}
\alias{summary_time_trend}
\title{Compute summary of how the hibernation niche changes over time}
\usage{
summary_time_trend(
  winters_stats_df,
  varname = "Suitable_area_km2_smooth_delta_pct_since1901",
  OBSCLIM = FALSE,
  year = ifelse(OBSCLIM, 2018, 2099),
  window_length_smoothing = 10,
  digits = 2
)
}
\arguments{
\item{winters_stats_df}{a dataframe of winter statistics produced by \code{\link[=summarise_info_winter.stars.all]{summarise_info_winter.stars.all()}}}

\item{varname}{the quoted name of the variable to retain from each stars object (default depends on the function)}

\item{OBSCLIM}{a boolean indicating if the function must run on the OBSCLIM data (default = \code{FALSE})}

\item{year}{a numeric scalar indicating the year of the winter to consider}

\item{window_length_smoothing}{the width of the window used for smoothing via \code{\link[caTools:runmean]{caTools::runmean()}}}

\item{digits}{the number of digits used for rounding averaged results using \code{\link[=round]{round()}}}
}
\value{
a ggplot2 object
}
\description{
Compute summary of how the hibernation niche changes over time
}
\examples{
## see ?winteR

}
