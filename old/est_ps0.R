## Estimate the probability of an event in the ASPIRE placebo arm

est_ps0 <- function(hope_aspire_data) {
  this_dat <- hope_aspire_data %>% filter(trial == "aspr", arm == 0)
  obs_time <- this_dat$event_time
  cens_ind <- rep(1, length(obs_time))
  cens_ind[is.na(obs_time)] <- 0
  obs_time[is.na(obs_time)] <- this_dat$cens_time[is.na(obs_time)]
  surv_obj <- survival::Surv(obs_time, cens_ind, type = "right")
  survdf <- data.frame(time = surv_obj, l_0 = this_dat$l_0)
  reg_obj <- survival::coxph(time ~ l_0, data = survdf)
  ndt <- data.frame(l_0 = unique(this_dat$l_0))
  fit_obj <- survival::survfit(reg_obj, newdata = ndt)
  wntd_sum <- summary(fit_obj, times = 4)
  pred_df <- data.frame(l_0 = ndt$l_0, cdf = 1 - wntd_sum$surv[1, ])
  pred_fun <- function(l0){
    left_join(l0, pred_df, by = setdiff(colnames(pred_df), "cdf")) %>%
      pull(cdf)
  }
  return(pred_fun)
}
