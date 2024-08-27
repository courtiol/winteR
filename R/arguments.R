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
#' @param .cluster the registered default cluster (set internally)
#' @param .color_values a vector of colors for the `color` variable (default = `c("NA", "blue", "orange")`)
#' @param crop a boolean indicating whether to crop or not (default = `TRUE`)
#' @param .crop_args the list of coordinates to use for cropping (default = `list(S = 27, N = 72, W = -13, E = 56)`)
#' @param add_winter.stats a boolean indicating whether or not to compute winter summary statistics (default = `TRUE`)
#' @param data_budget a dataframe produced by [compute_budget_df()]
#' @param data_MR the data used to fit the thermoregulatory curves as produced by [build_MR_table()]
#' @param data_suitability a dataframe with at least columns `Year` and `Suitable_winter`
#' @param data_temp a dataframe with at least columns `Date` and `Temp`, such as those produced by [build_temp_2years()]
#' @param data_Tskin a file produced by [load_Tskin_datafile()]
#' @param digits the number of digits used for rounding averaged results using [round()]
#' @param directory_NCfiles the path to a folder containing subfolders with NC files
#' @param directory_stars the destination location for the rds files storing the stars object
#' @param downsample a boolean indicating whether to reduce the resolution or not (default = `FALSE`)
#' @param .downsampling_args arguments for [stars::st_downsample()] (default = `c(5, 5, 0)`, i.e. aggregate space but not dates)
#' @param filename the path to the file from which the data are to be imported
#' @param filenames a vector of paths to the files from which the data are to be imported
#' @param .fill_values a vector of colors for the `fill` variable (default = `c("NA", "blue", "orange")`)
#' @param fit_MR a fitted model predicting the metabolic rate in KJh^-1 produced by [torpor::tor_fit()]
#' @param fit_state a fitted model predicting the probability to be in normothermy
#' @param flatten whether to average the winter stars across forcing models (default = `TRUE`)
#' @param fn a function to be applied
#' @param huddling_factor a factor multiplied to the metabolic rate, to account for huddling, when individuals are thermoregulating (default = 0.5)
#' @param lapply_pkg the R package used to implement a `lapply()` kind of function (default = "pbmcapply"; other possibilities are "parallel" and "base")
#' @param lat a latitude
#' @param lat_ref a latitude of reference
#' @param .legend_position the value for the ggplot argument "legend.position" used in [ggplot2::theme()]
#' @param mass_g the mass of a common noctule
#' @param name_bool_var the quoted name of a boolean variable to use to filter cells in the grid (default is "Survive")
#' @param nb_cores the number of CPU cores to use (for Linux or MacOS only, not Windows, and don't use too many cores otherwise, you may reach the RAM limit and it won't work. If error, do reduce)
#' @param mask a MULTIPOLYGON object to be used as a mask
#' @param max_Year the maximum year (in numeric format) to consider in the factor
#' @param metadirectory_NCfiles a folder containing one subfolder per climate model, themselves containing one subfolder per SSP scenarios, themselves containing the NC files
#' @param min_days_trigger_winter the minimum number of days for which the temperature should be below `temp_threshold` to enter winter
#' @param min_Year the minimum year (in numeric format) to consider in the factor
#' @param min_years_trigger_suitability the minimum number of suitable winters to trigger the establishment or the disappearance of bats in a region
#' @param OBSCLIM a boolean indicating if the function must run on the OBSCLIM data (default = `FALSE`)
#' @param output4optim a boolean indicating if the function output must be modifed to direct use of optimization routines
#' @param polygons a list of sf (MULTI)POLYGON object representing the distribution of the species
#' @param rangeTa the range of ambient temperature to consider in plot
#' @param rangeTskin the range of skin temperature to consider in plot
#' @param roost_insulation_dTa the increase in temperature in the roost, compared to outside (default = 5)
#' @param scenario the scenario to be selected (`"SSP126"`, `"SSP245"`, `"SSP370"` or `"SP585"`)
#' @param scenarios a vector of the scenarios to retain (default = `c("OBSCLIM", "SSP126", "SSP585")`)
#' @param signed whether the output value should be signed or absolute (default = `TRUE`, which implies it is signed)
#' @param split_summer the day of mid-summer (default = `'07-01'`)
#' @param SSP the Shared Socioeconomic Pathways to consider ("126", "245", or "585")
#' @param stars1 a stars object produced by [compute_budget_stars()]
#' @param stars2 a stars object produced by [compute_budget_stars()]
#' @param stars3 a stars object produced by [compute_budget_stars()]
#' @param stars4 a stars object produced by [compute_budget_stars()]
#' @param stars_object a stars object
#' @param stars_tbl a tibble of stars objects produced by [build_suitability_stars()]
#' @param stars_list a list of stars objects initially produced by [build_suitability_stars()]
#' @param starsname the name of the column containing the stars objects to be used (in the tibble produced by [build_suitability_stars()])
#' @param strip_names the title to be used on top of each map (default, if NULL = `= c("Obs (1901-1930)", "Obs (1989-2018)", "SSP1-2.6 (2070-2099)", "SSP5-8.5 (2070-2099)")`)
#' @param temp_threshold the approximate temperature below which insects do not fly
#' @param threshold_mortality the maximal amount of fat consumed before mortality occurs (default = 27)
#' @param Tmirror the ambient temperature around which predictions are mirrored
#' @param varname the quoted name of the variable to retain from each stars object (default depends on the function)
#' @param vartype the type of information for the y variable: `"latitude"` or `"area"`
#' @param vec_Dates a vector of dates over 2 consecutive years
#' @param vec_Proportion a vector of proportion
#' @param vec_Suitable_winter a vector of boolean indicating if a hibernation season was suitable (`TRUE`) or not (`FALSE`) (nb: `FALSE` includes lack of hibernation season as well as hibernation season where the budget is not sufficient to survive)
#' @param vec_Temp a vector of ambient temperatures over 2 consecutive years
#' @param vec_Year a vector of years
#' @param window_length_smoothing the width of the window used for smoothing via [caTools::runmean()]
#' @param winters_stats_df a dataframe of winter statistics produced by [summarise_info_winter.stars.all()]
#' @param x a vector
#' @param y a string of characters indicating what y-variable to plot:
#'   "g_fat_per_state", "g_fat_per_day", or "g_fat_per_winter"
#' @param y_max the maximum value for the y axis
#' @param y_title the title for the y axis
#' @param year  a numeric scalar indicating the year of the winter to consider
#' @param year_start a numeric scalar indicating the first year of the winter to consider
#' @param years_to_combine the number of year to include in so-called decade (default = 10)
#'
NULL
