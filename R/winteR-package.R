#' ADD TITLE PAPER HERE
#'
#' The goal of winteR is to reproduce the results from the paper entitled "XXX" by Kravchenko et al.
#'
#' For reproducing the results of our paper, just follow the code presented in the section
#' "Examples" below.
#'
#' @name winteR-package
#' @aliases winteR-package winteR
#' @docType package
#'
#' @references
#' ADD REF PAPER HERE
#'
#' @seealso [build_Tskin_table()], [clean_Tskin_table()]
#'
#' @keywords package
#' @examples
#'
#'
#' ## Step 0: preparation
#'
#' ### Step 0A: check that all R packages used below are installed if you wish to reproduce everything
#' checkDependencies()
#'
#' ### Step 0B: decide to run slow operations from this workflow
#' run <- FALSE # change to TRUE for running everything (slow)
#'
#' ## Step 1: predicting normothermy from skin temperature (Tskin)
#' # Note: it is important _not_ to model temporal autocorrelation
#' # for accurate predictions in our context
#'
#' ### Step 1A: loading the data, see ?build_Tskin_table
#'
#' Tskin_files <- list.files(system.file("extdata/Tskin", package = "winteR"), full.names = TRUE)
#' data_Tskin <- build_Tskin_table(Tskin_files)
#' head(data_Tskin)
#' plot_Tskin_table(data_Tskin)
#'
#' ### Step 1B: filtering data to be included, see ?clean_Tskin_table
#' data_normothermy <- data_Tskin[data_Tskin$Included, ]
#'
#' nrow(data_normothermy) # = 22554 (number of usable observations from experiment 1)
#' length(unique(data_normothermy$ID)) # = 22 (number of individuals from experiment 1)
#' data_normothermy |>
#'   dplyr::summarise(N = length(unique(.data$ID)), .by = "Ta") ## individuals per ambient temp
#' #   Ta N
#' # 1 12 8
#' # 2  2 6 ## 2 loggers detached
#' # 3  7 8
#'
#'
#' ### Step 1C: fitting the model predicting normothermy from Tskin
#'
#' fit_normo_logit <- spaMM::fitme(Normo ~ Ta + (1|ID), data = data_normothermy,
#'                                 family = binomial(link = "logit"))
#' fit_normo_logit
#' #  formula: Normo ~ Ta + (1 | ID)
#' # Estimation of lambda by ML (p_v approximation of logL).
#' # Estimation of fixed effects by ML (p_v approximation of logL).
#' # family: binomial( link = logit )
#' #  ------------ Fixed effects (beta) ------------
#' #             Estimate Cond. SE t-value
#' # (Intercept)  -4.6460   0.2299 -20.205
#' # Ta            0.1786   0.0265   6.742
#' #  --------------- Random effects ---------------
#' # Family: gaussian( link = identity )
#' #            --- Variance parameters ('lambda'):
#' # lambda = var(u) for u ~ Gaussian;
#' #    ID  :  0.2033
#' #              --- Coefficients for log(lambda):
#' #  Group        Term Estimate Cond.SE
#' #     ID (Intercept)   -1.593  0.3273
#' # # of obs: 22554; # of groups: ID, 22
#' #  ------------- Likelihood values  -------------
#' #                         logLik
#' # logL       (p_v(h)): -3699.397
#'
#'
#' fit_normo_cauchit <- spaMM::fitme(Normo ~ Ta + (1|ID), data = data_normothermy,
#'                                  family = binomial(link = "cauchit"))
#' ## rm(list = ls()[ls() != "fit_normo_cauchit"])
#' ## usethis::use_data(fit_normo_cauchit, compress = "xz", overwrite = TRUE)
#' fit_normo_cauchit
#' # formula: Normo ~ Ta + (1 | ID)
#' # Estimation of lambda by ML (P_v approximation of logL).
#' # Estimation of fixed effects by ML (P_v approximation of logL).
#' # family: binomial( link = cauchit )
#' #  ------------ Fixed effects (beta) ------------
#' #             Estimate Cond. SE t-value
#' # (Intercept)  -22.518   1.6833 -13.377
#' # Ta             1.516   0.1856   8.167
#' #  --------------- Random effects ---------------
#' # Family: gaussian( link = identity )
#' #            --- Variance parameters ('lambda'):
#' # lambda = var(u) for u ~ Gaussian;
#' #    ID  :  8.702
#' #              --- Coefficients for log(lambda):
#' #  Group        Term Estimate Cond.SE
#' #     ID (Intercept)    2.164  0.3631
#' # # of obs: 22554; # of groups: ID, 22
#' #  ------------- Likelihood values  -------------
#' #                        logLik
#' # logL      (P_v(h)): -3700.402
#'
#' ### Step 1D: compare goodness of fit
#' AIC(fit_normo_logit, verbose = FALSE)["       marginal AIC:"] # 7404.795
#' AIC(fit_normo_cauchit, verbose = FALSE)["       marginal AIC:"] # 7406.804
#'
#' ### Step 1E: check model
#' DHARMa::simulateResiduals(fit_normo_logit, plot = TRUE)
#' DHARMa::simulateResiduals(fit_normo_cauchit, plot = TRUE)
#'
#' ### Step 1F: plot model predictions
#' plot_Tskin_fit(fit_normo_logit)
#' plot_Tskin_fit(fit_normo_cauchit)
#'
#' ### Step 1G: create fig 1
#' plot_Tskin_fit(fit_normo_cauchit)
#' if (!dir.exists("figures")) dir.create("figures")
#' showtext::showtext_opts(dpi = 300)
#' ggplot2::ggsave(filename = "figures/fig1.pdf", width = 12, height = 12, units = "cm")
#' ggplot2::ggsave(filename = "figures/fig1.png", width = 12, height = 12, units = "cm")
#'
#' ### Step 1H: create fig S1
#' pS1_1 <- plot_Tskin_fit(fit_normo_cauchit, rangeTa = c(-35, 35))
#' pS1_2 <- plot_Tskin_fit(fit_normo_logit, rangeTa = c(-35, 35))
#' pS1_12 <- cowplot::plot_grid(pS1_1, pS1_2, nrow = 2, labels= "AUTO", align = "hv")
#' ggplot2::ggsave(filename = "figures/figS1.pdf", plot = pS1_12,
#'                 width = 12, height = 20, units = "cm")
#'
#'
#' ## Step 2: fitting thermoregulatory curves
#'
#' ### Step 2A: loading the data, see ?build_MR_table
#' filepath <- list.files(system.file("extdata/thermoreg", package = "winteR"), full.names = TRUE)[1]
#' data_MR <- build_MR_table(filepath)
#' head(data_MR)
#'
#' length(unique(data_MR$ID)) # = 12 (number of individuals from experiment 2)
#' nrow(data_MR) # = 282 (number of hourly averages of the metabolic rate)
#'
#' ### Step 2B: fitting thermoregulatory curves using the torpor package
#' # Note: this step is slow, so we stored the fitted model in winteR, but you can refit the model
#' # by setting run to TRUE
#' if (run) {
#'   set.seed(123)
#'   fit_torpor <- torpor::tor_fit(Ta = data_MR$Ta, M = data_MR$kJ_h) ## slow
#'   ## rm(list = ls()[ls() != "fit_torpor"]);
#'   ## usethis::use_data(fit_torpor, compress = "xz", overwrite = TRUE)
#'   params <- torpor::tor_summarise(fit_torpor)$params[, "parameter"]
#'   jagsUI::traceplot(fit_torpor$mod_parameter, parameters = params) ## inspect MCMC chains
#'   jagsUI::densityplot(fit_torpor$mod_parameter, parameters = params) ## inspect posterior distrib
#' } else {
#'   data("fit_torpor", package = "winteR")
#' }
#'
#' fit_torpor
#' #$params
#' #   parameter   mean CI_2.5 median CI_97.5  Rhat
#' #1       tau1  0.143  0.124  0.143   0.167 1.000
#' #2       tau2  0.357  0.320  0.356   0.398 1.002
#' #3       tau3  0.294  0.257  0.291   0.346 1.002
#' #4       inte  4.645  4.268  4.642   5.025 1.000
#' #5       intc  0.031  0.025  0.030   0.041 1.000
#' #6       intr  0.627  0.550  0.627   0.707 1.000
#' #7      betat -0.117 -0.129 -0.117  -0.106 1.000
#' #8      betac  0.095  0.085  0.096   0.101 1.000
#' #9         Tt  4.937  4.216  4.920   5.722 1.000
#' #10       TMR  0.049  0.043  0.047   0.061 1.000
#' #11        Mr  0.713  0.664  0.718   0.734 1.000
#' #12       Tbe 39.547 38.978 39.535  40.198 1.000
#' #13       Tbt  5.352  4.610  5.334   6.164 1.000
#' #14       Tlc 33.263 31.807 33.279  34.694 1.000
#' #15      Mtnz  0.735  0.713  0.751   0.756    NA
#' #
#' #$ppo
#' #  name  ppo
#' #1   Mr 11.2
#' #2  TMR  4.5
#'
#' ### Step 2C: extract useful information from the fit
#' summarise_MR_fit(fit_torpor)
#' #                                              info parameter   mean CI_2.5 median CI_97.5 Rhat
#' #1             temperature at minimal metabolic rate        Tt  4.937  4.216  4.920   5.722    1
#' #2                  minimal metabolic rate in torpor       TMR  0.049  0.043  0.047   0.061    1
#' #3        skin temperature at minimal metabolic rate       Tbt  5.352  4.610  5.334   6.164    1
#' #4 minimum ambient temperature in thermoneutral zone       Tlc 33.263 31.807 33.279  34.694    1
#'
#'
#' ### Step 2D: extract statistics for main text
#' Tt <- summarise_MR_fit(fit_torpor)$mean[summarise_MR_fit(fit_torpor)$parameter == "Tt"] ##TODO, use median as in budget comput?
#' MR_at_Tt <- torpor::tor_predict(fit_torpor, Ta = Tt)$pred
#' round(100*MR_at_Tt[1] / MR_at_Tt[2], digits = 2) # MR in torpor is 1.38 % of MR in normothermy
#'
#' ### Step 2E: plotting thermoregulatory curves and temperature relationship
#' pS2_1 <- plot_MR_fit(fit_torpor, data_MR)
#' pS2_2 <- plot_TaTskin_data(fit_torpor, data_MR)
#' pS2_12 <- cowplot::plot_grid(pS2_1, pS2_2, nrow = 2, labels = "AUTO", align = "hv")
#' showtext::showtext_opts(dpi = 300)
#' ggplot2::ggsave(filename = "figures/fig2.pdf", plot = pS2_12,
#'                 width = 14, height = 20, units = "cm")
#' ggplot2::ggsave(filename = "figures/fig2.png", plot = pS2_12,
#'                 width = 14, height = 20, units = "cm")
#'
#' ## Step 3: example of budget requirements at 5 degrees in roost
#'
#' budget_5degrees <- compute_budget_df(data.frame(Temp = 0), # note: 0 degree in env = 5 in roost
#'                                      fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#' budget_5degrees
#'
#' ### Step 3A: time spent in each physiological state
#' round(budget_5degrees[, c("Daily_duration_normo", "Daily_duration_torpor")], digit = 2)
#' #   Daily_duration_normo Daily_duration_torpor
#' # 1                 0.51                 23.49
#'
#' ### Step 3B: metabolic rate in each physiological state
#' round(budget_5degrees[, c("MR_normo", "MR_torpor")], digit = 3)
#' #   MR_normo   MR_torpor
#' # 1     4.055      0.055
#'
#' ### Step 3C: daily metabolic rate
#' round(budget_5degrees[, c("Budget_MR")], digit = 3)
#' # 3.354
#'
#' ### Step 3D: daily fat consumption
#' round(budget_5degrees[, c("Budget_fat")], digit = 3)
#' # 0.089
#'
#' ## Step 4: illustration of winter and budget for Kharkiv
#'
#' ### Step 4A: plotting winter 2011-2012
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2011_2012.csv")
#' data_Kharkiv <- build_temp_2years(file_Kharkiv)
#' pS4_1 <- plot_winter_temp2years(data_Kharkiv, base_size = 9)
#'
#' ### Step 4B: computing budget
#' data_nrg <- compute_budget_df(data_Kharkiv, fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'
#' ### Step 3C: plotting temperature and budget
#' pS4_2 <- plot_budget_panel(data_nrg, y = "g_fat_per_state", base_size = 9)
#' pS4_3 <- plot_budget_panel(data_nrg, y = "g_fat_per_day", base_size = 9)
#' pS4_4 <- plot_budget_panel(data_nrg, y = "g_fat_per_winter", base_size = 9)
#' pS4_1234 <- cowplot::plot_grid(pS4_1, NULL, pS4_2, pS4_3, NULL, pS4_4,
#'                                nrow = 2,
#'                                labels = c("A", "", "B", "C", "", "D"),
#'                                align = "hv",
#'                                rel_widths = c(1, 0.1, 1), hjust = 0)
#' pS4_1234
#' showtext::showtext_opts(dpi = 300)
#' ggplot2::ggsave(filename = "figures/fig3.pdf", plot = pS4_1234,
#'                 width = 18, height = 12.5, units = "cm")
#' ggplot2::ggsave(filename = "figures/fig3.png", plot = pS4_1234,
#'                 width = 18, height = 12.5, units = "cm")
#'
#'
#' ## Step 5: creating stars objects storing daily temperature under different climate models
#'
#' ### Step 5a: download the large NC files from ISIMIP (https://www.isimip.org/)
#' # Note: the files are not stored in this packages since it requires to download 42.2 Gb of data
#' # distributed across 235 files and 27 sub-folders.
#' # Also, this must be done in Python. See the following file for details and
#' # check latest guidelines on the ISIMIP website (e.g. you may need to create an account):
#' list.files(paste0(system.file("extdata/python", package = "winteR")),
#'            pattern = "*.py$", full.names = TRUE)
#'
#' ### Step 5b: create the stars objects and store them as rds files
#' # Note: this is not run by default since you need the NC files on your hard drive.
#' # It also takes a while and requires a lot of RAM on your computer.
#' # The output should be a set of 21 files (1 for historical predictions and 5 models x 4 SSP
#' # scenarios = 20 objects for future predictions) amounting to a total of 62.2 Gb.
#' # In case of error caused by memory limitation, reduce `nb_cores` and try again.
#' # Do also make sure to adjust your path as required
#' if (run) {
#'   build_source_stars(metadirectory_NCfiles = "../NC/ISIMIP_sources/",
#'                      directory_stars = "../NC/stars/", nb_cores = 10)
#' }
#'
#'
#' ## Step 6: creating stars objects with winter summary statistics for each year
#'
#' if (!dir.exists("../NC/stars_winter/")) dir.create("../NC/stars_winter/")
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be very patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_cauchit,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     saveRDS(stars_winter, file = paste0("../NC/stars_winter/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#' ## Step 7: plotting the map of the prediction for the current distribution
#'
#'
#' ### Step 7A: preparing IUCN distribution map polygon file
#' IUCN_polygon_file <- list.files(system.file("extdata/IUCN", package = "winteR"),
#'                                 pattern = "*.shp", full.names = TRUE)
#' distrib_IUCN <- sf::read_sf(IUCN_polygon_file)
#'
#' ### Step 7B: plotting winter budget over Europe for 2018
#' if (run) {
#'  readRDS("../NC/stars_winter/gswp3-w5e5_OBSCLIM_winter.rds") |>
#'    dplyr::select(Budget_winter) |>
#'    dplyr::filter(year == 2018) |>
#'    plot_fat_map() +
#'    ggplot2::geom_sf(data = distrib_IUCN, fill = NA, colour = "darkgreen", linewidth = 1) +
#'    ggplot2::coord_sf(expand = FALSE)
#'  showtext::showtext_opts(dpi = 300)
#'  ggplot2::ggsave(filename = "figures/fig4.png",
#'                  width = 14, height = 14, units = "cm")
#'  ggplot2::ggsave(filename = "figures/fig4.pdf",
#'                  width = 14, height = 14, units = "cm")
#' }
#'
#' ## Step 8: predicting shift in hibernation niche
#'
#' if (run) {
#'
#' ### Step 8A: extracting winter info from all stars
#'   data("lands_polygons", package = "winteR") # load the mask
#'   winters_stats <- summarise_info_winter.stars.all("../NC/stars_winter",
#'                                                    mask = lands_polygons) ## takes a few min
#'
#' ### Step 8B: plotting time trends about the suitable area
#'   pS8_1 <- plot_time_trends(winters_stats)
#'   pS8_2 <- plot_time_trends(winters_stats, varname = "Latitude_min",
#'                                 y_title = "\n Minimal latitude of suitable hibernation areas")
#'   pS8_3 <- plot_time_trends(winters_stats, varname = "Latitude_max",
#'                                 y_title = "\n Maximal latitude of suitable hibernation areas")
#'   pS8_4 <- plot_time_trends(winters_stats, varname = "Suitable_area_km2", vartype = "area",
#'                            y_title = "Suitable hibernation surface area\n (x 1,000,000 km²)")
#'   pS8_1234 <- cowplot::plot_grid(pS8_4, pS8_1, pS8_2, pS8_3,
#'                                  nrow = 2, labels = c("A", "B", "C", "D"))
#'   pS8_1234
#'   showtext::showtext_opts(dpi = 300)
#'   ggplot2::ggsave(filename = "figures/fig5.png",
#'                  width = 18, height = 16, units = "cm")
#'
#' ## Step 9: plotting change in suitability maps
#'
#' ### Step 9A: building suitability maps
#'   stars_suitability <- build_suitability_stars("../NC/stars_winter",
#'                                                min_years_trigger_suitability = 10)
#'
#' ### Step 9B: plotting hibernation suitability other Europe
#'   pS9_1 <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP126",
#'                                 starsname = "stars_with_OBSCLIM_avg",
#'                                 varname = "decade_establishment",
#'                                 base_size = 9)
#'   pS9_2 <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP585",
#'                                 starsname = "stars_with_OBSCLIM_avg",
#'                                 varname = "decade_establishment",
#'                                 base_size = 9)
#'   pS9_3 <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP126",
#'                                 starsname = "stars_avg",
#'                                 varname = "freq_suitability",
#'                                 base_size = 9)
#'   pS9_4 <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP585",
#'                                 starsname = "stars_avg",
#'                                 varname = "freq_suitability",
#'                                 base_size = 9)
#'  pS9_1234 <- cowplot::plot_grid(pS9_1, pS9_3, pS9_2, pS9_4,
#'                                 nrow = 2, labels = c("A", "B", "C", "D"))
#'  pS9_1234
#' }
NULL
