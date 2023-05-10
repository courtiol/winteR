#' Definition of the function arguments
#'
#' Here is the list of most function arguments used in the package.
#' The arguments not contained in this list are those for which the exact meaning depends on the context.
#'
#'
#' @name arguments
#'
#' @param base_size base font size, given in pts
#' @param clean a boolean indicating if the data need to be filter by [clean_Tskin_table()] or not (default = `TRUE`)
#' @param data_budget a dataframe produced by [compute_nrg_budget()]
#' @param data_MR the data used to fit the thermoregulatory curves as produced by [build_MR_table()]
#' @param data_temp a dataframe with at least columns `Date` and `Temp`, such as those produced by [build_temp_2years()]
#' @param data_Tskin a file produced by [load_Tskin_datafile()]
#' @param filename the path to the file from which the data are to be imported
#' @param filenames a vector of paths to the files from which the data are to be imported
#' @param fit_MR a fitted model predicting the metabolic rate in KJh^-1 produced by [torpor::tor_fit()]
#' @param fit_state a fitted model predicting the probability to be in normothermy
#' @param min_days_trigger_winter the minimum number of days for which the temperature should be below `temp_threshold` to enter winter
#' @param rangeTa the range of ambient temperature to consider in plot
#' @param rangeTskin the range of skin temperature to consider in plot
#' @param roost_insulation_dTa the increase in temperature in the roost, compared to outside (default = 5)
#' @param split_summer the day of mid-summer (default = `'07-01'`)
#' @param temp_threshold the approximate temperature below which insects do not fly
#' @param threshold_mortality the maximal amount of fat consumed before mortality occurs (default = 24)
#' @param Tmirror the ambient temperature around which predictions are mirrored
#' @param y a string of characters indicating what y-variable to plot:
#'   "g_fat_per_state", "g_fat_per_day", or "g_fat_per_winter"
#'
NULL
