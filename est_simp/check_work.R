## Checking work script
library(tidyverse)
source("../sim_dat/simple_data_sim.R")
these_files <- setdiff(list.files(), "check_work.R")
src <- these_files %>% map(.f = function(x) source(x))

### creating dataset
prbs_asp <- data.frame(
  l_0 = c("A", "B", "C", "D", "E"),
  pr_a = c(0.15, 0.1, 0.2, 0.2, 0.3),
  base_y = c(10, -10, -10, -10, -10),
  base_cens = c(3, 0, 5, -6, -5))

prbs_hope <- data.frame(
  l_0 = c("A", "B", "C", "D", "E"),
  pr_a = c(0.8, 0.9, 0.7, 0.6, 0.85),
  base_y = c(10, -10, -10, -10, -10), # c(8, 0, -2, -5, 4),
  base_cens = c(3, 0, 1, -2, -2))

trial_dat <- make_both_trials(
  samp_size = 1050, trial_params = list("aspr_param" = prbs_asp,
                                       "hope_param" = prbs_hope)
)

### Eye test for being in the active arm:
trial_dat$data %>% mutate(event = !is.na(event_time)) %>%
  group_by(arm) %>% summarise(mean(event))

param_ests <- trial_dat$emp_param
## ASPIRE RR
param_ests$aspire$active_risk / param_ests$aspire$placebo_risk
## HOPE RR
param_ests$hope$active_risk / param_ests$hope_cf$placebo_risk

### baseline proportions
lk <- est_hope_baseline(trial_dat)
lk2 <- est_aspr_baseline(trial_dat)

table(lk(trial_dat %>% dplyr::select(l_0)))
table(lk2(trial_dat %>% dplyr::select(l_0)))
table(lk(trial_dat %>% dplyr::select(l_0)) /
        lk2(trial_dat %>% dplyr::select(l_0)))

### Propensity scores
ps_hope <- est_hope_ps(trial_dat)
ps_aspr <- est_aspr_ps(trial_dat)
hope_pss <- ps_hope(trial_dat %>% dplyr::select(l_0, a.0, a.1, a.2, a.3))
aspr_pss <- ps_aspr(trial_dat %>% dplyr::select(l_0, a.0, a.1, a.2, a.3))
hist(log(hope_pss))
hist(log(aspr_pss))
hist(log(hope_pss / aspr_pss))

### Survival probabilities
aspr_cens <- est_aspr_cens(trial_dat)
ph1 <- est_ph1(trial_dat)
ps0 <- est_ps0(trial_dat)

hist(ph1(trial_dat %>% dplyr::select(l_0)))
hist(ps0(trial_dat %>% dplyr::select(l_0)))
hist(aspr_cens(trial_dat %>% dplyr::select(l_0)))



Qfun_info <- est_cf_surv(trial_dat, hope_prop_score = ps_hope)
hope_fit <- est_obs_surv(trial_dat %>% filter(trial == "hope"),
                         othr_data = trial_dat %>% filter(trial == "aspr"))
Qfun <- surv_pred_fun(Qfun_info)
look <- est_ose(trial_dat$data)

Qfun(trial_dat %>% dplyr::select(l_0)) %>% hist()

### Estimating the IC
est_ole(trial_dat)
