#' ADD TITLE PAPER HERE
#'
#' The goal of winteR is to reproduce the results from the paper entitled "XXX" by Kravchenko et al.
#'
#' For reproducing the results of our paper, just follow the code presented in the section "Examples" below.
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
#' ## Step 0: check that all R packages required below are installed
#' checkDependencies()
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
#' ggplot2::ggsave(filename = "figures/fig1.pdf", width = 12, height = 12, units = "cm")
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
#' ### Step 2B: fitting thermoregulatory curves using the torpor package
#' # # Note: this step is slow, so we stored the fitted model in winteR, but you can refit the model
#' # # by uncommenting the code below
#' # set.seed(123)
#' # fit_torpor <- torpor::tor_fit(Ta = data_MR$Ta, M = data_MR$kJ_h) ## slow
#' # params <- torpor::tor_summarise(fit_torpor)$params[, "parameter"]
#' # jagsUI::traceplot(fit_torpor$mod_parameter, parameters = params) ## inspect MCMC chains
#' # jagsUI::densityplot(fit_torpor$mod_parameter, parameters = params) ## inspect posterior distrib
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
#' ### Step 2D: plotting thermoregulatory curves and temperature relationship
#' pS2_1 <- plot_MR_fit(fit_torpor, data_MR)
#' pS2_2 <- plot_TaTskin_data(fit_torpor, data_MR)
#' pS2_12 <- cowplot::plot_grid(pS2_1, pS2_2, nrow = 2, labels = "AUTO", align = "hv")
#' ggplot2::ggsave(filename = "figures/fig2.pdf", plot = pS2_12,
#'                 width = 14, height = 20, units = "cm")
#'
#'
#'  ## Step 3: illustration of winter and budget for Kharkiv
#'
#'  ### Step 3A: plotting winter 2011-2012
#' file_Kharkiv <- paste0(system.file("extdata/weather_real", package = "winteR"),
#'                        "/Kharkiv_weather_2011_2012.csv")
#' data_Kharkiv <- build_Kharkiv_table(file_Kharkiv)
#' plot_winter_temp2years(data_Kharkiv)
#'
#'
NULL
