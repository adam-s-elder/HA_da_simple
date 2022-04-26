## Estimate the denominator IC

est_den_ic <- function(hope_aspire_data, param_list) {
  browser()
  hope_dat <- hope_aspire_data %>% filter(trial == "hope") %>%
    mutate(y = event_time < 5)
  aspire_dat <- hope_aspire_data %>% filter(trial == "aspr")
  # Part 1 (Phi_1) ----------------------------------------------------------
  distr <- expand.grid(y = c(0, 1), l_0 = unique(hope_aspire_data$l_0))
  distr$phi1 <- distr$y * param_list$ps0(distr) /
    (1 - param_list$Qfun(distr))
  distr$prob <- param_list$ph1(distr) ** distr$y *
    (1 - param_list$ph1(distr)) ** (1 - distr$y)
  centering <- distr %>% group_by(l_0) %>%
    summarise(expec_phi1 = sum(prob * phi1)) %>% ungroup()
  distr <- left_join(distr, centering, by = c("l_0"))
  piece1 <- left_join(hope_dat, distr, by = c("l_0", "y")) %>%
    mutate(phi1_cent = phi1 - expec_phi1) %>% pull(phi1_cent)
  # Part 2 (Phi_2) ----------------------------------------------------------
  ## For now, we will assume that p(D = 0|l_0) = 0.5.  However,
  ## for simplicity (for now) we ignore the fact that half(ish) of
  ## the HOPE participants where in the placebo arm of the ASPIRE trial
  distr$phi2 <- (distr$y * param_list$ph1(distr) /
    (1 - param_list$Qfun(distr))) * (param_list$hope_l0(distr) /
    param_list$aspr_l0(distr))
  centering_phi2 <- distr %>% group_by(l_0) %>%
    summarise(expec = sum(prob * phi2)) %>% ungroup()
  piece1 <- left_join(hope_dat, centering_phi2, by = "l_0") %>%
    mutate(phi1_cent = phi2 - expec) %>% pull(phi1_cent)



}


