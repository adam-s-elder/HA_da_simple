## Estimation for HOPE/ASPIRE trial

est_ose <- function(HA_data) {
  hope_l0 <- est_hope_baseline(HA_data)
  aspr_l0 <- est_aspr_baseline(HA_data)
  hope_ps <- est_hope_ps(HA_data)
  Qfun_info <- est_cf_surv(HA_data, hope_ps)
  Qfun <- surv_pred_fun(Qfun_info)
  aspr_placebo_surv <- est_obs_surv(
    fit_data = HA_data %>% filter(trial == "aspr", arm == 0),
    othr_data = HA_data %>% filter(trial == "hope" | arm == 1)
      )
  hope_obs_surv <- est_obs_surv(
    fit_data = HA_data %>% filter(trial == "hope"),
    othr_data = HA_data %>% filter(trial == "aspr")
      )
  req_params <- list(
    "hope_l0" = hope_l0, "aspr_l0" = aspr_l0,
    "Qfun" = Qfun, "aspr_cf_active_surv" = Qfun_info,
    "aspr_placebo_surv" = aspr_placebo_surv,
    "hope_surv" = hope_obs_surv
    )
  calc_ic <- est_den_ic(HA_data, req_params)
  prop_r <- sum(HA_data$trial == "hope") / sum(HA_data$trial == "aspr")
  numerator_ic <- hope_obs_surv$obs %>% dplyr::select(pid, marg_ic) %>%
    mutate(marg_ic = marg_ic * (1 / prop_r))
  num_ic_vals <- HA_data %>% filter(trial == "aspr") %>%
    dplyr::select(pid) %>% left_join(numerator_ic, by = "pid")
  num_ic_vals$marg_ic[is.na(num_ic_vals$marg_ic)] <- 0
  ic_mat <- cbind(calc_ic$ic, num_ic_vals$marg_ic)
  num_est <- 1 - hope_obs_surv$param_est
  denom <- calc_ic$est
  grad <- matrix(c(1 / denom, -1 * num_est / (denom ** 2)), nrow = 2)
  est_ic <- ic_mat %*% grad
  psi_hat <- num_est / denom
  psi_hat_ose <- num_est / denom - mean(est_ic[, 1])
  psi_se <- sqrt(mean(est_ic[, 1] ** 2) / nrow(est_ic))
  ret_val <- list(
    est = psi_hat, ose = psi_hat_ose,
    ic = est_ic, psi_se = psi_se,
    ic_num = num_ic_vals$marg_ic,
    ic_denom = calc_ic$ic,
    ic_denom_mat = calc_ic$ic_mat
  )
  return(ret_val)
}

