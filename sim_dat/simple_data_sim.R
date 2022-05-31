### Simulated Data HA project
library(tidyverse)

gen_trial_dat <- function(ss, adhr_mat, l_obs = NULL,
                          pids = NULL, h_arm = 1, adh_eff = 1) {
  if (is.null(l_obs)) {
    trial <- "aspr"
    l_0 <- sample(c("A", "B", "C", "D", "E"), size = ss, replace = TRUE,
                  prob = 1:5)
    arm <- rbinom(n = ss, size = 1, prob = 0.5)
    pid <- 1:ss
  }else{
    trial <- "hope"
    l_0 <- l_obs
    arm <- rep(h_arm, length(l_0))
    pid = pids
  }
  prbs_df <- left_join(data.frame(l_0), adhr_mat, by = "l_0")
  prbs_df$pr_a <- prbs_df$pr_a * arm
  a_mat <- prbs_df$pr_a %>% map(.f = function(x) {
    a_0 <- rbinom(1, size = 1, prob = x)
    a_1 <- rbinom(1, size = 1, prob = mean(c(x, a_0)))
    a_2 <- rbinom(1, size = 1, prob = mean(c(x, a_0, a_1)))
    a_3 <- rbinom(1, size = 1, prob = mean(c(x, a_0, a_1, a_2)))
    return(c(a_0, a_1, a_2, a_3))
  }) %>% do.call(what = rbind)
  colnames(a_mat) <- paste0("a.", 0:3)
  lam_val <- 25 + prbs_df$base_y + adh_eff * apply(a_mat, 1, sum) * 2
  tt <- rexp(length(l_0), rate = 1/lam_val) %>% pmin(5) %>% ceiling()
  lam_cens <- 25 + prbs_df$base_cens
  ct <- rexp(length(l_0), rate = 1/lam_cens) %>% pmin(4) %>% ceiling()
  risk_plb <- mean(tt[arm == 0] <= 4)
  risk_act <- mean(tt[arm == 1] <= 4)
  tt[ct < tt] <- NA
  ct[tt <= ct] <- NA
  y <- pmin(tt, ct, na.rm = TRUE)
  for (r_idx in seq_along(y)) {
    if (y[r_idx] < 4) a_mat[r_idx, (y[r_idx] + 1):4] <- NA
  }
  return(list(
    data = data.frame(
    pid = pid, l_0 = l_0, a_mat, arm = arm,
    event_time = tt, cens_time = ct, trial = trial
  ), placebo_risk = risk_plb, active_risk = risk_act)
  )
}

make_both_trials <- function(samp_size, adhr_effect, trial_params) {
  aspire_info <- gen_trial_dat(ss = samp_size, adh_eff = adhr_effect,
                              adhr_mat = trial_params$aspr_param)
  aspire_dat <- aspire_info$data
  aspr_finish <- which(aspire_dat$cens_time >= 4)
  hope_l0 <- aspire_dat$l_0[aspr_finish]
  h_drp <- 0.85 * as.numeric(0.75 + hope_l0 %in% c("B", "E") * 0.25)
  hope_keep <- which(rbinom(n = length(hope_l0), size = 1, prob = h_drp) == 1)
  hope_l0 <- hope_l0[hope_keep]
  hope_pids <- aspire_dat$pid[aspr_finish[hope_keep]]
  hope_info <- gen_trial_dat(
    ss = NA, adhr_mat = trial_params$hope_param, adh_eff = adhr_effect,
    l_obs = hope_l0, pids = hope_pids, h_arm = 1
  )
  hope_plb_info <- gen_trial_dat(
    ss = NA, adhr_mat = trial_params$hope_param, adh_eff = adhr_effect,
    l_obs = hope_l0, pids = hope_pids, h_arm = 0
  )
  hope_dat <- hope_info$data
  return(list(
    data = bind_rows(aspire_dat, hope_dat),
    emp_param = list(
      aspire = aspire_info[c("placebo_risk", "active_risk")],
      hope = hope_info[c("placebo_risk", "active_risk")],
      hope_cf = hope_plb_info[c("placebo_risk", "active_risk")])
    )
  )
}
