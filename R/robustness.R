# Some code to fit thermoregulatory curves differently
#
# data_MR_filtered <- filter_MR_table(data_MR, fit_torpor)
# fit_torpor_filtered <- torpor::tor_fit(Ta = data_MR_filtered$Ta, M = data_MR_filtered$kJ_h)
#

#' pS2_1_filtered <- plot_MR_fit(fit_torpor_filtered, data_MR_filtered)
#' pS2_2_filtered <- plot_TaTskin_data(fit_torpor_filtered, data_MR_filtered)
#' pS2_12_filtered <- cowplot::plot_grid(pS2_1_filtered, pS2_2_filtered,
#'                                       nrow = 2, labels = "AUTO", align = "hv")
#' ggplot2::ggsave(filename = "figures/fig2_filtered.pdf", plot = pS2_12_filtered,
#'                 width = 14, height = 20, units = "cm")

# cbind(data_MR, torpor::tor_assign(fit_torpor)) |>
#   dplyr::count(ID, assignment)
#
# jacknife <- sapply(unique(data_MR_filtered$ID), function(id) {
#   print(id)
#   data_MR_filtered |>
#     dplyr::filter(.data$ID != id) -> d
#   fit_torpor_noid <- torpor::tor_fit(Ta = d$Ta, M = d$kJ_h)
#   torpor::tor_plot(fit_torpor_noid)
#   param <- torpor::get_parameters(fit_torpor_noid)
#   p <- param$mean
#   names(p) <- param$parameter
#   p
# }, simplify = FALSE)
#
#
# res <- as.data.frame(do.call("rbind", jacknife))
# rownames(res) <- unique(data_MR_filtered$ID)
# res
#
# plot(res$Tt, type = "h")
#
# #     tau1  tau2  tau3  inte  intc  intr  betat betac    Tt   TMR    Mr    Tbe   Tbt    Tlc  Mtnz
# # 11 0.143 0.362 0.276 4.713 0.031 0.623 -0.121 0.095 4.751 0.048 0.697 38.849 5.151 32.772 0.737
# # 2  0.135 0.343 0.257 4.652 0.029 0.646 -0.117 0.096 5.136 0.047 0.711 39.823 5.541 33.525 0.734
# # 3  0.146 0.332 0.244 4.831 0.031 0.640 -0.127 0.094 4.677 0.049 0.665 38.150 5.063 32.417 0.728
# # 4  0.141 0.351 0.257 4.677 0.031 0.619 -0.118 0.095 4.843 0.049 0.720 39.655 5.256 33.360 0.742
# # 5  0.145 0.358 0.262 4.862 0.032 0.623 -0.127 0.095 4.531 0.049 0.692 38.293 4.914 32.497 0.736
# # 6  0.142 0.363 0.271 4.751 0.031 0.611 -0.122 0.095 4.631 0.048 0.704 39.055 5.028 32.885 0.748
# # 60 0.142 0.351 0.256 4.684 0.030 0.608 -0.119 0.095 4.700 0.047 0.703 39.214 5.098 33.048 0.732
# # 8  0.151 0.337 0.258 4.927 0.033 0.517 -0.126 0.092 3.735 0.047 0.701 39.044 4.108 33.247 0.730
# # 10 0.094 0.365 0.311 4.196 0.026 0.596 -0.101 0.098 5.463 0.044 0.741 41.554 5.904 34.016 0.754
# # 12 0.145 0.349 0.265 4.687 0.031 0.581 -0.119 0.095 4.489 0.047 0.707 39.340 4.886 33.163 0.734
# # 50 0.153 0.346 0.257 5.140 0.033 0.623 -0.136 0.094 4.222 0.049 0.674 37.678 4.579 32.418 0.729
# # 9  0.148 0.356 0.267 4.743 0.031 0.600 -0.122 0.095 4.525 0.048 0.705 38.822 4.920 32.718 0.747
#
#
# data_MR_filtered |>
#     dplyr::filter(.data$ID != "8") -> d
# fit_torpor_no8 <- torpor::tor_fit(Ta = d$Ta, M = d$kJ_h)
# torpor::tor_plot(fit_torpor_no8)
#  plot_MR_fit(fit_torpor_no8, d)
#
# data_MR_filtered |>
#     dplyr::filter(.data$ID != "10") -> d
# fit_torpor_no10 <- torpor::tor_fit(Ta = d$Ta, M = d$kJ_h)
# torpor::tor_plot(fit_torpor_no10)
#

