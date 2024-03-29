% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/budget.R
\name{compute_budget_df}
\alias{compute_budget_df}
\title{Compute the energy budget during a winter}
\usage{
compute_budget_df(
  data_MR,
  fit_state,
  fit_MR,
  roost_insulation_dTa = 5,
  temp_threshold = 7,
  split_summer = "07-01",
  min_days_trigger_winter = 14,
  threshold_mortality = 27,
  huddling_factor = 0.5,
  Tmirror = 2
)
}
\arguments{
\item{data_MR}{the data used to fit the thermoregulatory curves as produced by \code{\link[=build_MR_table]{build_MR_table()}}}

\item{fit_state}{a fitted model predicting the probability to be in normothermy}

\item{fit_MR}{a fitted model predicting the metabolic rate in KJh^-1 produced by \code{\link[torpor:tor_fit]{torpor::tor_fit()}}}

\item{roost_insulation_dTa}{the increase in temperature in the roost, compared to outside (default = 5)}

\item{temp_threshold}{the approximate temperature below which insects do not fly}

\item{split_summer}{the day of mid-summer (default = \code{'07-01'})}

\item{min_days_trigger_winter}{the minimum number of days for which the temperature should be below \code{temp_threshold} to enter winter}

\item{threshold_mortality}{the maximal amount of fat consumed before mortality occurs (default = 27)}

\item{huddling_factor}{a factor multiplied to the metabolic rate, to account for huddling, when individuals are thermoregulating (default = 0.5)}

\item{Tmirror}{the ambient temperature around which predictions are mirrored}
}
\value{
a dataframe
}
\description{
Bugets are computed in KJ and grams of fat consumed per hour (\code{MR_normo}, \code{MR_torpor},
\code{Fat_normo}, \code{Fat_torpor}) and per day (\code{Budget_MR} and \code{Budget_fat}). If \code{data_MR} contains a
column \code{Date}, the function will also compute the cumulative budget during the hibernation season
and the predicted survival status.
}
\examples{
#See ?winteR

}
