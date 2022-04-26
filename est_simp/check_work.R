## Checking work script
library(tidyverse)
library(sl3)
source("../sim_dat/simple_data_sim.R")
these_files <- setdiff(list.files(), "check_work.R")
src <- these_files %>% map(.f = function(x) source(x))

### creating dataset
trial_dat <- make_both_trials(
  samp_size = 1050, trial_params = list("aspr_param" = prbs_asp,
                                       "hope_param" = prbs_hope)
)

### baseline proportions
lk <- est_hope_baseline(trial_dat)
lk2 <- est_aspr_baseline(trial_dat)

table(lk(trial_dat %>% select(l_0)))
table(lk2(trial_dat %>% select(l_0)))
table(lk(trial_dat %>% select(l_0))/
        lk2(trial_dat %>% select(l_0)))

### Propensity scores
ps_hope <- est_hope_ps(trial_dat)
ps_aspr <- est_aspr_ps(trial_dat)
hope_pss <- ps_hope(trial_dat %>% select(l_0, a.1, a.2, a.3, a.4))
aspr_pss <- ps_aspr(trial_dat %>% select(l_0, a.1, a.2, a.3, a.4))
hist(log(hope_pss))
hist(log(aspr_pss))
hist(log(hope_pss / aspr_pss))

### Survival probabilities
aspr_cens <- est_aspr_cens(trial_dat)
ph1 <- est_ph1(trial_dat)
ps0 <- est_ps0(trial_dat)

hist(ph1(trial_dat %>% select(l_0)))
hist(ps0(trial_dat %>% select(l_0)))
hist(aspr_cens(trial_dat %>% select(l_0)))

### Qfunction
Qfun <- est_Qfun(trial_dat, hope_prop_score = ps_hope,
                 aspr_cens_fun = aspr_cens)
Qfun(trial_dat %>% select(l_0)) %>% hist()

### Estimating the IC
est_ole(trial_dat)
