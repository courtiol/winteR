#' Shorter and warmer winters constrain the energetics and distribution of hibernating animals
#'
#' The goal of winteR is to reproduce the results from the paper entitled "Shorter and warmer
#' winters constrain the energetics and distribution of hibernating animals" by Kravchenko et al.
#'
#' For reproducing the results of our paper, just follow the code presented in the section
#' "Examples" below.
#'
#' @name winteR-package
#' @aliases winteR-package winteR
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
#' alldeps <- checkDependencies()
#'
#' ### Step 0B: decide to run slow operations from this workflow
#' run <- FALSE # change to TRUE for running everything (slow)
#'
#' ### Step 0C: set the plotting resolution
#' if(alldeps) showtext::showtext_opts(dpi = 300)
#'
#' ### Step 0D: create a repository to store the figures
#' if (!dir.exists("figures")) dir.create("figures")
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
#' if (alldeps){
#'   plot_Tskin_table(data_Tskin, base_size = 8)
#'   ggplot2::ggsave(filename = "figures/figSXnew.pdf",
#'                   width = 18, height = 18, units = "cm")
#'   ggplot2::ggsave(filename = "figures/figSXnew.png",
#'                   width = 18, height = 18, units = "cm")
#' }
#'
#'
#' ### Step 1B: filtering data to be included, see ?clean_Tskin_table
#' data_normothermy <- data_Tskin[data_Tskin$Included, ]
#'
#' nrow(data_normothermy) # = 22554 (number of usable observations from experiment 1)
#' length(unique(data_normothermy$ID)) # = 22 (number of individuals from experiment 1)
#' dplyr::summarise(data_normothermy,
#'                  N = length(unique(.data$ID)), .by = "Ta") ## individuals per ambient temp
#' #   Ta N
#' # 1 12 8
#' # 2  2 6 ## 2 loggers got detached
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
#' fit_normo_probit <- spaMM::fitme(Normo ~ Ta + (1|ID), data = data_normothermy,
#'                                  family = binomial(link = "probit"))
#' ## rm(list = ls()[ls() != "fit_normo_probit"])
#' ## usethis::use_data(fit_normo_probit, compress = "xz", overwrite = TRUE)
#' fit_normo_probit
#' # formula: Normo ~ Ta + (1 | ID)
#' # Estimation of lambda by ML (P_v approximation of logL).
#' # Estimation of fixed effects by ML (P_v approximation of logL).
#' # family: binomial( link = probit )
#' #  ------------ Fixed effects (beta) ------------
#' #             Estimate Cond. SE t-value
#' # (Intercept) -2.38168  0.10461 -22.768
#' # Ta           0.07936  0.01219   6.509
#' #  --------------- Random effects ---------------
#' # Family: gaussian( link = identity )
#' #            --- Variance parameters ('lambda'):
#' # lambda = var(u) for u ~ Gaussian;
#' #    ID  :  0.04458
#' #              --- Coefficients for log(lambda):
#' #  Group        Term Estimate Cond.SE
#' #     ID (Intercept)   -3.111  0.3229
#' # # of obs: 22554; # of groups: ID, 22
#' #  ------------- Likelihood values  -------------
#' #                        logLik
#' # logL      (P_v(h)): -3700.482
#'
#'
#' fit_normo_cauchit <- spaMM::fitme(Normo ~ Ta + (1|ID), data = data_normothermy,
#'                                   family = binomial(link = "cauchit"))
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
#' AIC(fit_normo_probit, verbose = FALSE)["       marginal AIC:"] # 7406.964
#' AIC(fit_normo_cauchit, verbose = FALSE)["       marginal AIC:"] # 7406.804
#'
#' ### Step 1E: check model
#' if (alldeps) {
#'   DHARMa::simulateResiduals(fit_normo_logit, plot = TRUE)
#'   DHARMa::simulateResiduals(fit_normo_probit, plot = TRUE)
#'   DHARMa::simulateResiduals(fit_normo_cauchit, plot = TRUE)
#' }
#'
#' ### Step 1F: plot model predictions
#' plot_Tskin_fit(fit_normo_logit)
#' plot_Tskin_fit(fit_normo_probit)
#' plot_Tskin_fit(fit_normo_cauchit)
#'
#' ### Step 1G: create Extended Data Fig. 1
#' pS1_1 <- plot_Tskin_fit(fit_normo_cauchit, rangeTa = c(-5, 35))
#' pS1_2 <- plot_Tskin_fit(fit_normo_logit, rangeTa = c(-5, 35))
#' pS1_3 <- plot_Tskin_fit(fit_normo_probit, rangeTa = c(-5, 35))
#' if (alldeps) {
#'   pS1_123 <- cowplot::plot_grid(pS1_1, pS1_2, pS1_3, nrow = 3, labels= "AUTO", align = "hv")
#'   ggplot2::ggsave(filename = "figures/EDfig1.pdf", plot = pS1_123,
#'                   width = 12, height = 30, units = "cm")
#'   ggplot2::ggsave(filename = "figures/EDfig1.png", plot = pS1_123,
#'                   width = 12, height = 30, units = "cm")
#' }
#'
#' ## Step 2: fitting thermoregulatory curves
#'
#' ### Step 2A: loading the data, see ?build_MR_table
#' filepath <- list.files(system.file("extdata/thermoreg", package = "winteR"), full.names = TRUE)[1]
#' data_MR <- build_MR_table(filepath)
#' head(data_MR)
#'
#' length(unique(data_MR$ID)) # = 12 (number of individuals from experiment 2)
#' nrow(data_MR) # = 281 (number of hourly averages of the metabolic rate)
#'
#' ### Step 2B: fitting thermoregulatory curves using the torpor package
#' # Note: this step is slow, so we stored the fitted model in winteR, but you can refit the model
#' # by setting run to TRUE
#' if (run) {
#'   set.seed(123)
#'   n
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
#' #    parameter   mean CI_2.5 median CI_97.5  Rhat
#' # 1       tau1  0.153  0.132  0.153   0.177 1.000
#' # 2       tau2  0.357  0.320  0.356   0.398 1.001
#' # 3       tau3  0.313  0.277  0.311   0.356 1.002
#' # 4       inte  5.021  4.634  5.023   5.407 1.000
#' # 5       intc  0.033  0.024  0.032   0.050 1.000
#' # 6       intr  0.662  0.603  0.659   0.738 1.000
#' # 7      betat -0.138 -0.150 -0.138  -0.126 1.000
#' # 8      betac  0.088  0.074  0.089   0.098 1.000
#' # 9         Tt  4.456  4.184  4.397   5.159 1.000
#' # 10       TMR  0.049  0.037  0.047   0.069 1.000
#' # 11        Mr  0.560  0.498  0.567   0.584 1.001
#' # 12       Tbe 36.451 36.104 36.442  36.848 1.000
#' # 13       Tbt  4.811  4.477  4.756   5.509 1.000
#' # 14       Tlc 32.273 31.195 32.200  33.752 1.004
#' # 15      Mtnz  0.585  0.560  0.601   0.610    NA
#' #
#' # $ppo
#' #   name  ppo
#' # 1   Mr 17.0 ## note: these numbers can change upon reruning the code, this is expected.
#' # 2  TMR  9.1
#'
#' ### Step 2C: extract useful information from the fit
#' summarise_MR_fit(fit_torpor)
#' #                                              info parameter   mean CI_2.5 median CI_97.5 Rhat
#' #1             temperature at minimal metabolic rate        Tt  4.456  4.184  4.397   5.159 1.000
#' #2                  minimal metabolic rate in torpor       TMR  0.049  0.037  0.047   0.069 1.000
#' #3        skin temperature at minimal metabolic rate       Tbt  4.811  4.477  4.756   5.509 1.000
#' #4 minimum ambient temperature in thermoneutral zone       Tlc 32.273 31.195 32.200  33.752 1.004
#'
#'
#' ### Step 2D: extract statistics for main text
#' Tt <- summarise_MR_fit(fit_torpor)$median[summarise_MR_fit(fit_torpor)$parameter == "Tt"]
#' # 4.397
#' MR_at_Tt <- torpor::tor_predict(fit_torpor, Ta = Tt)$pred
#' round(100*MR_at_Tt[1] / MR_at_Tt[2], digits = 2) # MR in torpor is 1.29 % of MR in normothermy
#'
#' ### Step 2E: plotting thermoregulatory curves and temperature relationship
#' pS2_A <- plot_Tskin_fit(fit_normo_cauchit, base_size = 9)
#' pS2_B <- plot_MR_fit(fit_torpor, data_MR, base_size = 9)
#' if (alldeps){
#'   pS2_AB <- cowplot::plot_grid(pS2_A, pS2_B, ncol = 2, labels = "AUTO", align = "hv")
#'   ggplot2::ggsave(filename = "figures/fig1new.pdf", plot = pS2_AB,
#'                   width = 18, height = 6.5, units = "cm")
#'   ggplot2::ggsave(filename = "figures/fig1new.png", plot = pS2_AB,
#'                   width = 18, height = 6.5, units = "cm")
#' }
#'
#' ### Step 2F: plotting temperature relationship
#' pS2_C <- plot_TaTskin_data(fit_torpor, data_MR, base_size = 9)
#' if (alldeps){
#'   pS2_BC <- cowplot::plot_grid(pS2_B, pS2_C, ncol = 1, labels = "AUTO", align = "hv")
#'   ggplot2::ggsave(filename = "figures/EDfig2.pdf", plot = pS2_BC,
#'                   width = 9, height = 13, units = "cm")
#'   ggplot2::ggsave(filename = "figures/EDfig2.png", plot = pS2_BC,
#'                   width = 9, height = 13, units = "cm")
#' }
#'
#' ## Step 3: example of budget requirements at -5 degrees in roost
#'
#' budget_minus5degrees <- compute_budget_df(data.frame(Temp = -5),
#'                                           roost_insulation_dTa = 0, # note remove insulation
#'                                           fit_state = fit_normo_cauchit,
#'                                           fit_MR = fit_torpor, huddling_factor = 1) # note remove huddling
#'
#' ### Step 3A: time spent in each physiological state
#' round(budget_minus5degrees[, c("Daily_duration_normo", "Daily_duration_torpor")], digit = 2)
#' #   Daily_duration_normo Daily_duration_torpor
#' # 1                 0.86                 23.14
#'
#' ### Step 3B: metabolic rate in each physiological state
#' signif(budget_minus5degrees[, c("MR_normo", "MR_torpor")], digit = 3)
#' #   MR_normo   MR_torpor
#' # 1     5.71      1.35
#'
#' ### Step 3C: daily metabolic rate per state
#' signif(budget_minus5degrees[, c("Budget_MR_normo", "Budget_MR_torpor")], digit = 3)
#' #   Budget_MR_normo Budget_MR_torpor
#' # 1             4.9             31.2
#'
#' ### Step 3D: daily metabolic rate
#' signif(budget_minus5degrees[, c("Budget_MR")], digit = 3)
#' # 36.1
#'
#' ### Step 3E: daily fat consumption
#' signif(budget_minus5degrees[, c("Budget_fat")], digit = 3)
#' # 0.958
#'
#' ## Step 3bis: example of budget requirements at 0 degrees in roost due to insulation
#'
#' budget_minus5degrees_insul <- compute_budget_df(data.frame(Temp = -5),
#'                                    fit_state = fit_normo_cauchit, fit_MR = fit_torpor,
#'                                    huddling_factor = 1)
#'
#' ### Step 3F: time spent in each physiological state
#' round(budget_minus5degrees_insul[, c("Daily_duration_normo", "Daily_duration_torpor")], digit = 2)
#'   # Daily_duration_normo Daily_duration_torpor
#'   # 1                 0.46                 23.54
#'
#' ### Step 3G: metabolic rate in each physiological state
#' signif(budget_minus5degrees_insul[, c("MR_normo", "MR_torpor")], digit = 3)
#' #   MR_normo   MR_torpor
#' #1     5.02     0.659
#'
#' ### Step 3H: daily metabolic rate per state
#' signif(budget_minus5degrees_insul[, c("Budget_MR_normo", "Budget_MR_torpor")], digit = 3)
#' #   Budget_MR_normo Budget_MR_torpor
#' #1            2.33             15.5
#'
#' ### Step 3I: daily metabolic rate
#' signif(budget_minus5degrees_insul[, c("Budget_MR")], digit = 3)
#' # 17.8
#'
#' ### Step 3J: daily fat consumption
#' signif(budget_minus5degrees_insul[, c("Budget_fat")], digit = 3)
#' # 0.473
#'
#' ## Step 3ter: example of budget requirements at 0 degrees in roost due to insulation
#' ## and with huddling factor at 0.5
#'
#' budget_minus5degrees_insul_huddl <- compute_budget_df(data.frame(Temp = -5),
#'                                          fit_state = fit_normo_cauchit, fit_MR = fit_torpor,
#'                                          huddling_factor = 0.5)
#'
#' ### Step 3K: daily metabolic rate per state
#' signif(budget_minus5degrees_insul_huddl[, c("Budget_MR_normo", "Budget_MR_torpor")], digit = 3)
#' #   Budget_MR_normo Budget_MR_torpor
#' #1            1.16             7.76
#'
#' ### Step 3L: daily metabolic rate
#' signif(budget_minus5degrees_insul_huddl[, c("Budget_MR")], digit = 3)
#' # 8.92
#'
#' ### Step 3J: daily fat consumption
#' signif(budget_minus5degrees_insul_huddl[, c("Budget_fat")], digit = 3)
#' # 0.237
#'
#'
#' ## Step 4: illustration of winter and budget for Kharkiv
#'
#' ### Step 4A: plotting winter 2015-2016
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2015_2016.csv")
#' data_Kharkiv <- build_temp_2years(file_Kharkiv)
#' pS4_A <- plot_winter_temp2years(data_Kharkiv, base_size = 9)
#'
#' ### Step 4B: computing budget
#' data_nrg <- compute_budget_df(data_Kharkiv, fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'
#' ### Step 3C: plotting temperature and budget
#' pS4_B <- plot_budget_panel(data_nrg, y = "g_fat_per_state", base_size = 9)
#' pS4_C <- plot_budget_panel(data_nrg, y = "g_fat_per_day", base_size = 9, y_max = 2.5)
#' pS4_D <- plot_budget_panel(data_nrg, y = "g_fat_per_winter", base_size = 9)
#' # [1] "death date = 2016-01-26"
#' # [1] "death date torpor only = NA"
#' if (alldeps){
#'  pS4_ABCD <- cowplot::plot_grid(pS4_A, NULL, pS4_B, pS4_C, NULL, pS4_D,
#'                                 nrow = 2,
#'                                 labels = c("A", "", "B", "C", "", "D"),
#'                                 align = "hv",
#'                                 rel_widths = c(1, 0.1, 1), hjust = 0)
#'   pS4_ABCD
#'   ggplot2::ggsave(filename = "figures/fig2.pdf", plot = pS4_ABCD,
#'                   width = 18, height = 12.5, units = "cm")
#'   ggplot2::ggsave(filename = "figures/fig2.png", plot = pS4_ABCD,
#'                   width = 18, height = 12.5, units = "cm")
#' }
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
#'                      directory_stars = "../NC/stars/", nb_cores = 21)
#' }
#'
#'
#' ## Step 6: creating stars objects with winter summary statistics for each year
#'
#' ### Step 6A: standard setup: cauchit model and 5 degree insulation with huddling factor at 0.5
#' folder_winter_stars <- "stars_winter_cauchit_dTa5"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_cauchit,
#'                                          roost_insulation_dTa = 5,
#'                                          huddling_factor = 0.5,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#'
#' ### Step 6B: alternative setup: cauchit model and 0 degree insulation with huddling factor at 0.5
#' folder_winter_stars <- "stars_winter_cauchit_dTa0"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_cauchit,
#'                                          roost_insulation_dTa = 0,
#'                                          huddling_factor = 0.5,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#'
#' ### Step 6C: alternative setup: probit model and 5 degree insulation with huddling factor at 0.5
#' folder_winter_stars <- "stars_winter_probit_dTa5"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_probit,
#'                                          roost_insulation_dTa = 5,
#'                                          huddling_factor = 0.5,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#'
#' ### Step 6D: alternative setup: probit model and 0 degree insulation with huddling factor at 0.5
#' folder_winter_stars <- "stars_winter_probit_dTa0"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_probit,
#'                                          roost_insulation_dTa = 0,
#'                                          huddling_factor = 0.5,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#' ### Step 6E: alternative setup: cauchit model and 5 degree insulation with huddling factor at 1
#' folder_winter_stars <- "stars_winter_cauchit_dTa5_huddl1"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_cauchit,
#'                                          roost_insulation_dTa = 5,
#'                                          huddling_factor = 1,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#'
#' ### Step 6F: alternative setup: cauchit model and 0 degree insulation with huddling factor at 1
#' folder_winter_stars <- "stars_winter_cauchit_dTa0_huddl1"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_cauchit,
#'                                          roost_insulation_dTa = 0,
#'                                          huddling_factor = 1,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#'
#' ### Step 6G: alternative setup: probit model and 5 degree insulation with huddling factor at 1
#' folder_winter_stars <- "stars_winter_probit_dTa5_huddl1"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_probit,
#'                                          roost_insulation_dTa = 5,
#'                                          huddling_factor = 1,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#'
#' ### Step 6H: alternative setup: probit model and 0 degree insulation with huddling factor at 1
#' folder_winter_stars <- "stars_winter_probit_dTa0_huddl1"
#'
#' if (!dir.exists(paste0("../NC/", folder_winter_stars))) dir.create(paste0("../NC/", folder_winter_stars))
#' if (run) {
#'   all_rds_to_do <- list.files(path = "../NC/stars/", full.names = TRUE, pattern =  ".rds")
#'   for (rds_index in seq_along(all_rds_to_do)) {
#'     rds_to_do <- all_rds_to_do[rds_index]
#'     stars_to_do <- readRDS(file = rds_to_do)
#'     name_stars <- strsplit(basename(rds_to_do), split = "\\.rds")[[1]]
#'     print(paste("processing stars", name_stars, "be patient!"))
#'     stars_winter <- compute_budget_stars(stars_to_do,
#'                                          fit_state = fit_normo_probit,
#'                                          roost_insulation_dTa = 0,
#'                                          huddling_factor = 1,
#'                                          fit_MR = fit_torpor, nb_cores = 30)
#'     if (!grepl("OBSCLIM", basename(rds_to_do))) { ## remove overlap between OBSCLIM years
#'                                                   ## and future predictions
#'       stars_winter |>
#'         dplyr::filter(year > 2018) -> stars_winter
#'     }
#'     saveRDS(stars_winter, file = paste0("../NC/", folder_winter_stars, "/", name_stars, "_winter.rds"),
#'             compress = FALSE)
#'     rm(list = "stars_to_do"); gc()
#'   }
#' }
#'
#'
#' ## Step 7: plotting the map of the prediction for the current distribution
#'
#' ### Step 7A: preparing IUCN distribution map polygon file (note: no smoothing used here)
#' winter_2015_polygon_file <- list.files(system.file("extdata/distribution_range/winter_2015",
#'                                                    package = "winteR"),
#'                                        pattern = "*.shp", full.names = TRUE)
#' winter_2015 <- sf::read_sf(winter_2015_polygon_file)
#'
#' winter_1980_polygon_file <- list.files(system.file("extdata/distribution_range/winter_1980",
#'                                                    package = "winteR"),
#'                                        pattern = "*.shp", full.names = TRUE)
#' winter_1980 <- sf::read_sf(winter_1980_polygon_file)
#'
#' ### Step 7B: plotting winter budget over Europe for 1979 and 2014
#' folder_winter_stars <- "stars_winter_cauchit_dTa5"
#' #folder_winter_stars <- "stars_winter_cauchit_dTa0"
#' #folder_winter_stars <- "stars_winter_probit_dTa5"
#' #folder_winter_stars <- "stars_winter_probit_dTa0"
#' #folder_winter_stars <- "stars_winter_cauchit_dTa5_huddl1"
#' #folder_winter_stars <- "stars_winter_cauchit_dTa0_huddl1"
#' #folder_winter_stars <- "stars_winter_probit_dTa5_huddl1"
#' #folder_winter_stars <- "stars_winter_probit_dTa0_huddl1"
#'
#' if (!dir.exists(paste0("figures/", folder_winter_stars))) dir.create(paste0("figures/", folder_winter_stars))
#'
#' if (run) {
#'  pS7_A <- readRDS(paste0("../NC/", folder_winter_stars, "/gswp3-w5e5_OBSCLIM_winter.rds")) |>
#'    dplyr::select(Budget_winter) |>
#'    dplyr::filter(year == 1979) |>
#'    plot_fat_map(polygons = list(winter_1980), base_size = 9)
#'  pS7_B <-readRDS(paste0("../NC/", folder_winter_stars, "/gswp3-w5e5_OBSCLIM_winter.rds")) |>
#'    dplyr::select(Budget_winter) |>
#'    dplyr::filter(year == 2014) |>
#'    plot_fat_map(polygons = list(winter_2015), base_size = 9)
#'  #pS7_legend <- cowplot::get_legend(pS7_A) ## broken at the time of coding: https://github.com/wilkelab/cowplot/issues/202
#'  pS7_legend <- ggplot2::ggplotGrob(pS7_A)$grob[ggplot2::ggplotGrob(pS7_A)$layout$name == "guide-box-left"][[1]]
#'  pS7_AB <- cowplot::plot_grid(pS7_legend,
#'                               pS7_A + ggplot2::theme(legend.position="none"),
#'                               pS7_B + ggplot2::theme(legend.position="none"),
#'                               nrow = 1, labels = c("", "A. Winter 1979-1980",
#'                                                    "B. Winter 2014-2015"),
#'                               rel_widths = c(0.1, 0.45, 0.45))
#'  pS7_AB
#'
#'  ggplot2::ggsave(filename = paste0("figures/", folder_winter_stars, "/fig3.png"),
#'                  width = 18, height = 9, units = "cm")
#'  ggplot2::ggsave(filename = paste0("figures/", folder_winter_stars, "/fig3.pdf"),
#'                  width = 18, height = 9, units = "cm", device = Cairo::CairoPDF)
#' }
#'
#' ### Step 7C: extracting info about stars characteristics
#' folder_winter_stars <- "stars_winter_cauchit_dTa5"
#'
#' if (run) {
#'   stars_example <- readRDS(paste0("../NC/", folder_winter_stars, "/gswp3-w5e5_OBSCLIM_winter.rds"))
#'   ## coordinates
#'   range(sf::st_coordinates(stars_example)$y) # range of latitude of center of cells
#'   # [1] 27.25 71.75
#'
#'   range(sf::st_coordinates(stars_example)$x) # range of longitude of center of cells
#'   # [1] -12.75  55.75
#'
#'   ## CRS, projection
#'   sf::st_crs(stars_example, type = "proj")
#'   # Coordinate Reference System:
#'   #   User input: +proj=longlat +datum=WGS84 +no_defs
#'   #   wkt:
#'   # GEOGCRS["unknown",
#'   #     DATUM["World Geodetic System 1984",
#'   #         ELLIPSOID["WGS 84",6378137,298.257223563,
#'   #             LENGTHUNIT["metre",1]],
#'   #         ID["EPSG",6326]],
#'   #     PRIMEM["Greenwich",0,
#'   #         ANGLEUNIT["degree",0.0174532925199433],
#'   #         ID["EPSG",8901]],
#'   #     CS[ellipsoidal,2],
#'   #         AXIS["longitude",east,
#'   #             ORDER[1],
#'   #             ANGLEUNIT["degree",0.0174532925199433,
#'   #                 ID["EPSG",9122]]],
#'   #         AXIS["latitude",north,
#'   #             ORDER[2],
#'   #             ANGLEUNIT["degree",0.0174532925199433,
#'   #                 ID["EPSG",9122]]]]
#'
#'   ## area
#'   mean(units::set_units(sf::st_area(sf::st_as_sf(stars_example)), km^2))
#'   # 1956.294 [km^2]
#'
#'   sqrt(mean(units::set_units(sf::st_area(sf::st_as_sf(stars_example)), km^2)))
#'   # 44.23001 [km]
#'
#'   sd(units::set_units(sf::st_area(sf::st_as_sf(stars_example)), km^2))
#'   # 526.7266
#' }
#'
#'
#' ## Step 8: predicting shift in hibernation niche
#'
#' if (run) {
#'
#' ### Step 8A: extracting winter info from all stars
#'   data("lands_polygons", package = "winteR") # load the mask
#'   winters_stats_cauchit_dTa5 <- summarise_info_winter.stars.all("../NC/stars_winter_cauchit_dTa5",
#'                                                                 mask = lands_polygons) ## takes a few min
#'   winters_stats_cauchit_dTa0 <- summarise_info_winter.stars.all("../NC/stars_winter_cauchit_dTa0",
#'                                                                 mask = lands_polygons) ## takes a few min
#'   winters_stats_probit_dTa5 <- summarise_info_winter.stars.all("../NC/stars_winter_probit_dTa5",
#'                                                                mask = lands_polygons) ## takes a few min
#'   winters_stats_probit_dTa0 <- summarise_info_winter.stars.all("../NC/stars_winter_probit_dTa0",
#'                                                                mask = lands_polygons) ## takes a few min
#'   winters_stats_cauchit_dTa5_huddl1 <- summarise_info_winter.stars.all("../NC/stars_winter_cauchit_dTa5_huddl1",
#'                                                                        mask = lands_polygons) ## takes a few min
#'   winters_stats_cauchit_dTa0_huddl1 <- summarise_info_winter.stars.all("../NC/stars_winter_cauchit_dTa0_huddl1",
#'                                                                        mask = lands_polygons) ## takes a few min
#'   winters_stats_probit_dTa5_huddl1 <- summarise_info_winter.stars.all("../NC/stars_winter_probit_dTa5_huddl1",
#'                                                                       mask = lands_polygons) ## takes a few min
#'   winters_stats_probit_dTa0_huddl1 <- summarise_info_winter.stars.all("../NC/stars_winter_probit_dTa0_huddl1",
#'                                                                       mask = lands_polygons) ## takes a few min
#'
#'  winters_stats <-  winters_stats_cauchit_dTa5 ## we use this combination as default
#'
#' ### Step extra: check extrapolation with respect to normothermy model
#' winters_stats |>
#'   dplyr::summarise(Proportion_outside_modelled_range = mean(Duration_winter_outside_range_temp/Duration_winter),
#'   .by = c("Forcing", "Scenario")) |>
#'   dplyr::filter(Scenario != "OBSCLIM") |>
#'   dplyr::pull(Proportion_outside_modelled_range) |>
#'   range()
#'
#' ## Step 9: computing change in potential hibernation area
#'
#'  summary_time_trend(winters_stats, varname = "Suitable_area_km2_smooth_delta_pct_since1901")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126          9.41           18.9            10.7         3.01          20.4  12.5               10.5
#'  # 2 SSP245         15.1            29.2            12.3         9.97          40.8  21.5               16.7
#'  # 3 SSP370         18.8            22.7            15.5        14.4           21.5  18.6               17.8
#'  # 4 SSP585         20.0            26.0            26.9        12.3           10.5  19.2               21.3
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_median_smooth_move_since1901")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126          237.           482.            271.         193.          504.  337.               296.
#'  # 2 SSP245          437.           571.            315.         382.          860.  513.               426.
#'  # 3 SSP370          593.           938.            604.         482.         1338.  791.               654.
#'  # 4 SSP585          804.          1249.            849.         582.         1471.  991.               871.
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_min_smooth_move_since1901")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126          61.2           106.            83.4         83.4          117.  90.1               83.4
#'  # 2 SSP245         128.            128.            72.3        128.           228. 137.               114.
#'  # 3 SSP370         239.            261.           206.         161.           328. 239.               217.
#'  # 4 SSP585         261.            395.           261.         306.           384. 321.               306.
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_max_smooth_move_since1901")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126           0             22.2             0            0            89.0  22.2               5.56
#'  # 2 SSP245          22.2           77.8            66.7         66.7         367.  120.               58.4
#'  # 3 SSP370         145.           167.            211.          77.8         389.  198.              150.
#'  # 4 SSP585         200.           256.            289.         189.          389.  265.              234.
#'
#'  summary_time_trend(winters_stats, varname = "Suitable_area_km2_smooth_delta_pct_since1901",
#'                     OBSCLIM = TRUE)
#'  # A tibble: 1 × 4
#'  # Rowwise:
#'  #   Scenario `gswp3-w5e5`  mean mean_without_ukesm
#'  #   <chr>           <dbl> <dbl>              <dbl>
#'  # 1 OBSCLIM          6.33  6.33               6.33
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_median_smooth_move_since1901",
#'                     OBSCLIM = TRUE)
#'  # A tibble: 1 × 4
#'  # Rowwise:
#'  #   Scenario `gswp3-w5e5`  mean mean_without_ukesm
#'  #   <chr>           <dbl> <dbl>              <dbl>
#'  # 1 OBSCLIM          259.  259.               259.
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_min_smooth_move_since1901",
#'                     OBSCLIM = TRUE)
#'  # A tibble: 1 × 4
#'  # Rowwise:
#'  #   Scenario `gswp3-w5e5`  mean mean_without_ukesm
#'  #   <chr>           <dbl> <dbl>              <dbl>
#'  # 1 OBSCLIM          106.  106.               106.
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_max_smooth_move_since1901",
#'                     OBSCLIM = TRUE)
#'  # A tibble: 1 × 4
#'  # Rowwise:
#'  #   Scenario `gswp3-w5e5`  mean mean_without_ukesm
#'  #   <chr>           <dbl> <dbl>              <dbl>
#'  # 1 OBSCLIM             0     0                  0
#'
#'  summary_time_trend(winters_stats, varname = "Suitable_area_km2_smooth_delta_pct_since2018")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126          2.89           11.8            4.07        -3.12         13.2   5.77               3.91
#'  # 2 SSP245          8.25           21.5            5.65         3.42         32.4  14.2                9.71
#'  # 3 SSP370         11.7            15.4            8.59         7.59         14.3  11.5               10.8
#'  # 4 SSP585         12.9            18.5           19.4          5.58          3.93 12.1               14.1
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_median_smooth_move_since2018")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126         -22.2           222.            11.1        -66.7          245.  77.8               36.1
#'  # 2 SSP245         178.            311.            55.6        122.           600. 254.               167.
#'  # 3 SSP370         334.            678.           345.         222.          1079. 532.               395.
#'  # 4 SSP585         545.            990.           589.         322.          1212. 732.               612.
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_min_smooth_move_since2018")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126         -44.5            0             -22.2        -22.2          11.1 -15.6             -22.2
#'  # 2 SSP245          22.2           22.2           -33.4         22.2         122.   31.1               8.34
#'  # 3 SSP370         133.           156.            100.          55.6         222.  133.              111.
#'  # 4 SSP585         156.           289.            156.         200.          278.  216.              200.
#'
#'  summary_time_trend(winters_stats, varname = "Latitude_max_smooth_move_since2018")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126           0             22.2             0            0            89.0  22.2               5.56
#'  # 2 SSP245          22.2           77.8            66.7         66.7         367.  120.               58.4
#'  # 3 SSP370         145.           167.            211.          77.8         389.  198.              150.
#'  # 4 SSP585         200.           256.            289.         189.          389.  265.              234.
#'
#'  summary_time_trend(winters_stats, varname = "Temp_winter_mean_smooth_delta_since1901",
#'                     OBSCLIM = TRUE)
#'  # A tibble: 1 × 4
#'  # Rowwise:
#'  #   Scenario `gswp3-w5e5`  mean mean_without_ukesm
#'  #   <chr>           <dbl> <dbl>              <dbl>
#'  # 1 OBSCLIM          1.02  1.02               1.02
#'
#'  summary_time_trend(winters_stats, varname = "Duration_winter_smooth_delta_since1901",
#'                     OBSCLIM = TRUE, digits = 0)
#'  # A tibble: 1 × 4
#'  # Rowwise:
#'  #   Scenario `gswp3-w5e5`  mean mean_without_ukesm
#'  #   <chr>           <dbl> <dbl>              <dbl>
#'  # 1 OBSCLIM         -19.5   -19                -19
#'
#'  summary_time_trend(winters_stats, varname = "Temp_winter_mean_smooth_delta_since2018")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`  mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl> <dbl>              <dbl>
#'  # 1 SSP126        -0.402          0.333         -0.191       -0.909           1.72  0.11              -0.29
#'  # 2 SSP245         0.296          1.06           0.0543      -0.0204          3.07  0.89               0.35
#'  # 3 SSP370         0.671          2.40           0.923        0.379           3.60  1.59               1.09
#'  # 4 SSP585         2.32           3.05           1.56         1.09            3.71  2.35               2.01
#'
#'  summary_time_trend(winters_stats, varname = "Duration_winter_smooth_delta_since2018")
#'  # A tibble: 4 × 8
#'  # Rowwise:
#'  #   Scenario `gfdl-esm4` `ipsl-cm6a-lr` `mpi-esm1-2-hr` `mri-esm2-0` `ukesm1-0-ll`   mean mean_without_ukesm
#'  #   <chr>          <dbl>          <dbl>           <dbl>        <dbl>         <dbl>  <dbl>              <dbl>
#'  # 1 SSP126          5.64          -8.36            2.29         6.07         -12.8  -1.43                1.41
#'  # 2 SSP245        -11.4          -17.5            -7.29        -5.47         -37.5 -15.8               -10.4
#'  # 3 SSP370        -23.1          -35.8           -27.4        -16.4          -62.5 -33.0               -25.7
#'  # 4 SSP585        -29.9          -50.1           -35.5        -19.5          -70.4 -41.1               -33.7
#'
#'  ### Example of data summary: range of change in smoothed median latitude:
#'  summary_time_trend(winters_stats, varname = "Latitude_median_smooth_move_since1901") |>
#'    dplyr::select(-"mean") |>
#'    tidyr::pivot_longer(!"Scenario") |>
#'    dplyr::summarise(min = min(.data$value), max = max(.data$value), .by = "Scenario")
#'  # A tibble: 4 × 3
#'  #   Scenario   min   max
#'  #   <chr>    <dbl> <dbl>
#'  # 1 SSP126    193.  504.
#'  # 2 SSP245    315.  860.
#'  # 3 SSP370    482. 1338.
#'  # 4 SSP585    582. 1471.
#'
#'
#' ### Step 8B: plotting time trends about the suitable area
#' folder_winter_stars <- "stars_winter_cauchit_dTa5"
#' #folder_winter_stars <- "stars_winter_cauchit_dTa0"
#' #folder_winter_stars <- "stars_winter_probit_dTa5"
#' #folder_winter_stars <- "stars_winter_probit_dTa0"
#' #folder_winter_stars <- "stars_winter_cauchit_dTa5_huddl1"
#' #folder_winter_stars <- "stars_winter_cauchit_dTa0_huddl1"
#' #folder_winter_stars <- "stars_winter_probit_dTa5_huddl1"
#' #folder_winter_stars <- "stars_winter_probit_dTa0_huddl1"
#'
#' if (!dir.exists(paste0("figures/", folder_winter_stars))) dir.create(paste0("figures/", folder_winter_stars))
#'
#'  winters_stats <- get(gsub(pattern = "stars_winter", replacement = "winters_stats", folder_winter_stars))
#'
#'  pS8_A <- plot_time_trends(winters_stats, varname = "Suitable_area_km2_smooth", vartype = "area",
#'                           y_title = "Potential hibernation area\n (x 1,000,000 km²)")
#'  pS8_B <- plot_time_trends(winters_stats)
#'  pS8_C <- plot_time_trends(winters_stats, varname = "Latitude_min_smooth",
#'                            y_title = "\n Minimal latitude of potential hibernation area",
#'                            y_max = 32)
#'  pS8_D <- plot_time_trends(winters_stats, varname = "Latitude_max_smooth",
#'                            y_title = "\n Maximal latitude of potential hibernation area",
#'                            y_max = 72)
#'
#'  if(alldeps) {
#'    pS8_ABCD <- cowplot::plot_grid(pS8_A, pS8_B, pS8_C, pS8_D,
#'                                   nrow = 2, labels = c("A", "B", "C", "D"))
#'    pS8_ABCD
#'    ggplot2::ggsave(filename = paste0("figures/", folder_winter_stars, "/fig4.png"),
#'                    width = 18, height = 16, units = "cm")
#'    ggplot2::ggsave(filename = paste0("figures/", folder_winter_stars, "/fig4.pdf"),
#'                    width = 18, height = 16, units = "cm")
#' }
#'
#' ### Step 8C: comparing trends
#'   var <- "Suitable_area_km2_smooth_delta_pct_since1901"
#'   #var <- "Suitable_area_km2_smooth_delta_pct_since2018"
#'
#'   summary_time_trend(winters_stats_cauchit_dTa5, varname = var) |>
#'     dplyr::select("mean", "Scenario") |>
#'     dplyr::mutate(what = "cauchit_dTa5") -> cauchit_dTa5
#'
#'   summary_time_trend(winters_stats_cauchit_dTa0, varname = var) |>
#'     dplyr::select("mean", "Scenario") |>
#'     dplyr::mutate(what = "cauchit_dTa0") -> cauchit_dTa0
#'
#'   summary_time_trend(winters_stats_cauchit_dTa5_huddl1, varname = var) |>
#'     dplyr::select("mean", "Scenario") |>
#'     dplyr::mutate(what = "cauchit_dTa5_huddl1") -> cauchit_dTa5_huddl1
#'
#'   summary_time_trend(winters_stats_cauchit_dTa0_huddl1, varname = var) |>
#'     dplyr::select("mean", "Scenario") |>
#'     dplyr::mutate(what = "cauchit_dTa0_huddl1") -> cauchit_dTa0_huddl1
#'
#'   dplyr::bind_rows(cauchit_dTa5, cauchit_dTa0, cauchit_dTa5_huddl1, cauchit_dTa0_huddl1) |>
#'     dplyr::arrange(Scenario, dplyr::desc(mean))
#'
#'
#' ## Step 9: plotting change in suitability maps
#'
#' ### Step 9A: building suitability maps
#'   stars_suitability <- build_suitability_stars("../NC/stars_winter",
#'                                                min_years_trigger_suitability = 10)
#'
#' ### Step 9B: plotting hibernation suitability other Europe
#'   pS9_A <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP126",
#'                                 starsname = "stars_with_OBSCLIM_avg",
#'                                 varname = "decade_establishment",
#'                                 polygons = list(winter_2015),
#'                                 years_to_combine = 20,
#'                                 base_size = 9, legend_position = "left") +
#'            ggplot2::labs(fill = "Decade of\n possible\n establishment")
#'   pS9_B <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP585",
#'                                 starsname = "stars_with_OBSCLIM_avg",
#'                                 varname = "decade_establishment",
#'                                 polygons = list(winter_2015),
#'                                 years_to_combine = 20,
#'                                 base_size = 9, legend_position = "none")
#'   pS9_C <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP126",
#'                                 starsname = "stars_avg",
#'                                 varname = "freq_suitability_pct",
#'                                 polygons = list(winter_2015),
#'                                 years_to_combine = 20,
#'                                 base_size = 9) +
#'            ggplot2::labs(fill = "Proportion of\n suitable\n winters")
#'   pS9_D <- plot_suitability_map(stars_tbl = stars_suitability,
#'                                 scenario = "SSP585",
#'                                 starsname = "stars_avg",
#'                                 varname = "freq_suitability_pct",
#'                                 polygons = list(winter_2015),
#'                                 years_to_combine = 20,
#'                                 base_size = 9, legend_position = "none")
#'
#'  if(alldeps) {
#'    pS9_legend1 <- cowplot::get_legend(pS9_A)
#'    pS9_legend2 <- cowplot::get_legend(pS9_C)
#'    pS9_ABCD <- cowplot::plot_grid(pS9_legend1,
#'                                   pS9_A + ggplot2::theme(legend.position="none"), pS9_B,
#'                                   pS9_legend2,
#'                                   pS9_C + ggplot2::theme(legend.position="none"), pS9_D,
#'                                   nrow = 2, labels = c("", "A. GSWP3-W5E5 + SSP1-2.6",
#'                                                        "B. GSWP3-W5E5 + SSP5-8.5",
#'                                                        "", "C. GSWP3-W5E5 + SSP1-2.6",
#'                                                        "D. GSWP3-W5E5 + SSP5-8.5"),
#'                                   rel_widths = c(0.15, 0.425, 0.425), hjust = -0.15)
#'    pS9_ABCD
#'    ggplot2::ggsave(filename = "figures/fig5.png",
#'                    width = 18, height = 17, units = "cm")
#'    ggplot2::ggsave(filename = "figures/fig5.pdf",
#'                    width = 18, height = 17, units = "cm")
#' }
#'
#' ## Step 10: plot the hibernation niche
#'
#'   stars_list <- lapply(list.files(path = "~/collaborations/kseniia/NC/stars_winter",
#'                                   pattern = "*.rds", full.names = TRUE), readRDS)
#'   sum(sapply(stars_list, \(star) length(star$Budget_winter))) ## number of time-series
#'   # 21585960
#'   plot_hibernation_niche(stars_list = stars_list)
#'   ggplot2::ggsave(filename = "figures/fig6.png",
#'                   width = 18, height = 16, units = "cm")
#'   ggplot2::ggsave(filename = "figures/fig6.pdf",
#'                   width = 18, height = 16, units = "cm")
#'
#' ## Step 11: change of winter characteristics through time
#'
#'  pS11_A <- tabulate_time_trend(winters_stats) |>
#'     dplyr::filter(Scenario != "OBSCLIM") |>
#'     ggplot2::ggplot() +
#'       ggplot2::aes(y = .data$Temp_winter_mean_smooth, x = .data$Year, linetype = .data$Forcing,
#'                    colour = .data$Forcing) +
#'       ggplot2::geom_line() +
#'       ggplot2::facet_wrap(~ Scenario) +
#'       ggplot2::scale_y_continuous(minor_break = NULL, limits = c(-3, 3), breaks = -3:3) +
#'       ggplot2::scale_x_continuous(minor_break = NULL) +
#'       ggplot2::theme_bw() +
#'       ggplot2::theme(legend.direction = "horizontal",
#'                      axis.text.x = ggplot2::element_text(angle = 0, vjust = 1, hjust = 0),
#'                      strip.background = ggplot2::element_rect(fill = NA),
#'                      plot.margin = ggplot2::margin(b = 10, l = 15, r = 15)) +
#'       ggplot2::labs(colour = "Forcing model", linetype = "Forcing model",
#'                     y = "Mean temperature during the hibernation season (\u00B0C)", x = NULL)
#'
#'   pS11_B <- tabulate_time_trend(winters_stats) |>
#'     dplyr::filter(Scenario != "OBSCLIM") |>
#'     ggplot2::ggplot() +
#'       ggplot2::aes(y = .data$Duration_winter_smooth, x = Year, linetype = .data$Forcing,
#'                    colour = .data$Forcing) +
#'       ggplot2::geom_line() +
#'       ggplot2::facet_wrap(~ Scenario) +
#'       ggplot2::scale_y_continuous(minor_break = NULL) +
#'       ggplot2::scale_x_continuous(minor_break = NULL) +
#'       ggplot2::coord_cartesian(ylim = c(0, NA)) +
#'       ggplot2::theme_bw() +
#'       ggplot2::theme(legend.direction = "horizontal",
#'                      axis.text.x = ggplot2::element_text(angle = 0, vjust = 1, hjust = 0),
#'                      strip.background = ggplot2::element_rect(fill = NA),
#'                      plot.margin = ggplot2::margin(b = 10, l = 15, r = 15)) +
#'       ggplot2::labs(colour = "Forcing model", linetype = "Forcing model",
#'                     y = "Duration of the hibernation season (days)", x = NULL)
#'
#' if (alldeps) {
#'   pS11_legend <- cowplot::get_legend(pS11_A)
#'   pS11_AB <- cowplot::plot_grid(pS11_legend,
#'                                 pS11_A + ggplot2::theme(legend.position="none"),
#'                                 pS11_B + ggplot2::theme(legend.position="none"),
#'                                 nrow = 3, labels = c("", "A", "B"),
#'                                 rel_heights = c(0.05, 0.45, 0.45))
#'   pS11_AB
#'   ggplot2::ggsave(filename = "figures/EDfig3.png",
#'                   width = 18, height = 35, units = "cm")
#'   ggplot2::ggsave(filename = "figures/EDfig3.pdf",
#'                   width = 18, height = 35, units = "cm")
#' }
#'
#'  tabulate_time_trend(winters_stats) |>
#'     dplyr::filter(Scenario == "SSP585", Year == 2099) |>
#'     dplyr::select(c("Forcing", "Temp_winter_mean_smooth_delta_since2018",
#'                     "Duration_winter_smooth_delta_since2018")) |>
#'     dplyr::summarise(mean_duration = mean(.data$Duration_winter_smooth_delta_since2018),
#'                      mean_temp = mean(.data$Temp_winter_mean_smooth_delta_since2018))
#' #  mean_duration mean_temp
#' # 1     -40.92207  2.346603
#'
#'
#' ## Step 12: extra info for discussion
#'  compar <- compute_budget_df(data.frame(Temp = c(-5, 2)),
#'                              fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'  compar[1, "Budget_fat"] / compar[2, "Budget_fat"]
#'  # [1] 3.32319
#'
#'
#' ## Step 13: estimation of maximum mass for flight
#'  plot_flightpower()
#'  # [1] "maximum fat mass = 18.92 g"
#'  # [1] "maximum total mass = 45.42 g"
#'  ggplot2::ggsave(filename = "figures/EDfig4.png",
#'                  width = 18, height = 14, units = "cm")
#'  ggplot2::ggsave(filename = "figures/EDfig4.pdf",
#'                  width = 18, height = 14, units = "cm")
#'
#' ## Step 14: additional figure on energy expenditure
#'   plot_budget_curves(fit_state = fit_normo_cauchit, fit_MR = fit_torpor)
#'   if (alldeps) {
#'     ggplot2::ggsave(filename = "figures/figS3.pdf",
#'                     width = 18, height = 14, units = "cm")
#'     ggplot2::ggsave(filename = "figures/figS3.png",
#'                     width = 18, height = 14, units = "cm")
#'   }
#'
#' }
"_PACKAGE"
