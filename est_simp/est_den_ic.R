## Estimate the denominator IC

est_den_ic <- function(hope_aspire_data, param_list) {
  aspire_df <- hope_aspire_data %>% filter(trial == "aspr")
  hope_df <- hope_aspire_data %>% filter(trial == "hope")
  prop_r <- nrow(hope_df) / nrow(aspire_df)
  ic_df <- aspire_df %>% select(pid)
  hope_surv <- param_list$hope_surv
  qfun <- param_list$Qfun
  aspr_placebo_surv <- param_list$aspr_placebo_surv
  aspr_active_surv <- param_list$aspr_cf_active_surv
  hope_base_dens <- param_list$hope_l0
  aspr_base_dens <- param_list$aspr_l0
  aspr_active_surv <- param_list$aspr_cf_active_surv
  aspr_pl_surv <- surv_pred_fun(aspr_placebo_surv)
  hope_surv_pred <- surv_pred_fun(hope_surv)
  risk_ratio_pred <- function(l_0) {
    l0_df <- data.frame(l_0 = l_0)
    return(
      (1 - aspr_pl_surv(l0_df)) * (1 - hope_surv_pred(l0_df)) /
        ((1 - qfun(l0_df)) ** 2)
    )
  }
  browser()
  # Part 1 (Phi_1) ----------------------------------------------------------
  hope_l <- hope_surv$obs %>%
    mutate(q = qfun(data.frame(l_0 = l_0)),
           aspr_pl_surv = aspr_pl_surv(data.frame(l_0 = l_0)),
           ic_1 = (1/prop_r) * ic * (1 - aspr_pl_surv) / (1 - q))
  ic_df <- left_join(ic_df, hope_l %>% select(pid, ic_1), by = "pid")
  # Part 2 (Phi_2) ----------------------------------------------------------
  phi2_df <- aspr_placebo_surv$obs
  l0_df <- phi2_df %>% select(l_0)
  phi2_df$dens_ratio <- hope_base_dens(l0_df) / aspr_base_dens(l0_df)
  phi2_df$hope_surv <- hope_surv_pred(l0_df)
  phi2_df$q <- Qfun(l0_df)
  ## TODO: CHECK WHAT SHOULD BE DONE HERE!!!!!
  rand_ratio <- 0.5 ## THIS PIECE
  phi2_df <- phi2_df %>%
    mutate(ic_2 = (rand_ratio ** (-1)) * ic *
             ((1 - hope_surv) / (1 - q)) * dens_ratio)
  ic_df <- left_join(ic_df, phi2_df %>% select(pid, ic_2), by = "pid")
  # Part 3 (Phi_3) ----------------------------------------------------------
  ## TODO: CHECK WHAT SHOULD BE DONE HERE!!!!!
  phi3_df <- hope_surv$obs %>%
    mutate(ratio = risk_ratio_pred(l_0),
           dif = est - qfun(data.frame(l_0 = l_0)),
           ic_3 = ratio * dif * (1 / prop_r))
  ic_df <- left_join(ic_df, phi3_df %>% select(pid, ic_3), by = "pid")
  # Part 4 (Phi_4) ----------------------------------------------------------
  phi4_df <- aspr_active_surv$marg_aspr_df
  l0_df <- phi4_df %>% select(l_0)
  phi4_df$dens_ratio <- hope_base_dens(l0_df) / aspr_base_dens(l0_df)
  # %>%
  phi4_df <- phi4_df %>% mutate(
    ratio = risk_ratio_pred(l_0),
    ic_4 = ratio * ic * dens_ratio * (1 / rand_ratio)
    )
  # TODO: This term still needs to be mean centered (since it is conditioned)
  # on a and l, but we also have terms outside of the survival ic that
  # include adherence.
  ic_df <- left_join(ic_df, phi4_df %>% select(pid, ic_4), by = "pid")
  # Part 5 (Phi_5) ----------------------------------------------------------
  hope_surv
  phi5_df <- hope_surv$obs
  l0_df <- data.frame(l_0 = phi5_df$l_0)
  phi5_df$param_ratio <-
    (1 - aspr_pl_surv(l0_df)) * (1 - hope_surv_pred(l0_df)) /
      (1 - qfun(l0_df))
  phi5_df$ic_5 <- (1 / prop_r) *
    (phi5_df$param_ratio - mean(phi5_df$param_ratio))
  ic_df <- left_join(ic_df, phi5_df %>% select(pid, ic_5), by = "pid")
  all_ic <- apply(ic_df, 1, sum, na.rm = TRUE)
  return(all_ic)
}


