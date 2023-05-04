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
#' p1 <- plot_Tskin_fit(fit_normo_cauchit, rangeTa = c(-35, 35))
#' p2 <- plot_Tskin_fit(fit_normo_logit, rangeTa = c(-35, 35))
#' p12 <- gridExtra::grid.arrange(p1 + ggplot2::labs(tag = "A"), p2 + ggplot2::labs(tag = "B"))
#' ggplot2::ggsave(filename = "figures/figS1.pdf", plot = p12, width = 12, height = 20, units = "cm")
#'
NULL
