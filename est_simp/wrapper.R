## Estimation for HOPE/ASPIRE trial

est_ole <- function(HA_data) {
  browser()
  hope_l0 <- est_hope_baseline(HA_data)
  aspr_l0 <- est_aspr_baseline(HA_data)
  ps0 <- est_ps0(HA_data) #ASPIRE placebo risk
  ph1 <- est_ph1(HA_data) #HOPE active-arm risk
  aspr_cens <- est_aspr_cens(HA_data)
  hope_ps <- est_hope_ps(HA_data)
  aspr_ps <- est_aspr_ps(HA_data)
  Qfun <- est_Qfun(HA_data, hope_ps, aspr_cens)
  req_params <- list(
    "hope_l0" = hope_l0, "aspr_l0" = aspr_l0, "ps0" = ps0,
    "ph1" = ph1,  "aspr_cens" = aspr_cens, "hope_ps" = hope_ps,
    "aspr_ps" = aspr_ps, "Qfun" = Qfun)
  calc_ic <- est_den_ic(HA_data, req_params)
  hope_l0 <- HA_data %>% filter(trial == "hope") %>% pull(l_0)
  param_0step <- mean(ps0(hope_l0) * ph1(hope_l0) / (1 - Qfun(hope_l0)))
  param_ic <- apply(calc_ic, 1, sum)
  param_1step <- param_0step + mean(param_ic)
  param_se <- sqrt(var(param_ic))
  return(list(
    param_est = param_1step,
              param_se = param_se,
              ic_mat = calc_ic,
              obs_dat = HA_data
              ))
}

