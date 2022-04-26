### Simulated Data HA project
library(tidyverse)

prbs_asp <- data.frame(
  l_0 = c("A", "B", "C", "D", "E"),
  pr_a = c(0.7, 0.1, 0.8, 0.3, 0.8),
  base_y = c(1, 0, -1, -2, 1),
  base_cens = c(3, 0, 1, -2, -2))

prbs_hope <- data.frame(
  l_0 = c("A", "B", "C", "D", "E"),
  pr_a = c(0.5, 0.4, 0.7, 0.5, 0.6),
  base_y = c(1, 0, -1, -2, 1),
  base_cens = c(3, 0, 1, -2, -2))

gen_trial_dat <- function(ss, adhr_mat, l_obs = NULL) {
  if (is.null(l_obs)) {
    trial <- "aspr"
    l_0 <- sample(c("A", "B", "C", "D", "E"), size = ss, replace = TRUE,
                  prob = 1:5)
    arm <- rbinom(n = ss, size = 1, prob = 0.5)
  }else{
    trial <- "hope"
    l_0 <- l_obs
    arm <- rep(1, length(l_0))
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
  lam_val <- 30 + prbs_df$base_y - apply(a_mat, 1, sum) * 2
  tt <- rexp(length(l_0), rate = 1/lam_val) %>% pmin(5) %>% ceiling()
  lam_cens <- 30 + prbs_df$base_cens
  ct <- rexp(length(l_0), rate = 1/lam_cens) %>% pmin(5) %>% ceiling()
  tt[ct < tt] <- NA
  ct[tt < ct] <- NA
  return(data.frame(
    l_0 = l_0, a = a_mat, arm = arm, event_time = tt,
    cens_time = ct, trial = trial
  ))
}

make_both_trials <- function(samp_size, trial_params) {
  aspire_dat <- gen_trial_dat(ss = samp_size,
                              adhr_mat = trial_params$aspr_param)
  hope_l0 <- aspire_dat$l_0[aspire_dat$event_time %in% 5]
  h_drp <- 0.8 * as.numeric(0.5 + hope_l0 %in% c("B", "E")/2)
  hope_l0 <- hope_l0[rbinom(n = length(hope_l0), size = 1, prob = h_drp) == 1]
  hope_dat <- gen_trial_dat(ss = NA, adhr_mat = trial_params$hope_param,
                        l_obs = hope_l0)
  return(bind_rows(aspire_dat, hope_dat))
}

