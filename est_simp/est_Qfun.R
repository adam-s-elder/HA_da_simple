## Estimate the probability of the counter factual outcome under HOPE adherence.

est_Qfun <- function(hope_aspire_data, hope_prop_score, aspr_cens_fun) {
  this_dat <- hope_aspire_data %>% filter(trial == "aspr", arm == 1)
  y <- as.numeric(this_dat$event_time < 5)
  delt <- as.numeric(this_dat$cens_time >= 5)
  y[delt == 0] <- 1 #If delta = 0, censoring happened, so indicator will be 0
  delt[y == 1] <- 0 #If y = 1, event happened so indicator will be zer0
  (1 - y) * delt
  ip <- 1 / aspr_cens_fun(this_dat %>% select(l_0))
  this_dat$psudo_outcome <- (1 - y) * delt * ip
  this_dat$ip <- ip
  ### TODO: Make sure to check this!!!!OMG CHECK THIS FOR THE LOVE
  ### OF ALL THAT IS GOOD AND HOLY. PLEASE CHECK THIS.
  pred_mod <- glm(psudo_outcome ~ a.1  + a.2 + a.3 + a.4 + l_0,
                  data = this_dat, weights = this_dat$ip,
                  family = "gaussian")
  all_pos <- expand.grid(l_0 = unique(this_dat$l_0),
                         a.1 = c(0, 1), a.2 = c(0, 1),
                         a.3 = c(0, 1), a.4 = c(0, 1))
  all_pos$expect <- predict(pred_mod, all_pos, type = "response")
  all_pos$weight <- hope_prop_score(all_pos %>% select(-expect))
  all_pos <- all_pos %>% group_by(l_0) %>%
    summarise(tot_prob = sum(weight),
              expec = sum(expect * weight))
  pred_df <- all_pos %>% select(l_0, surv = expec)
  pred_fun <- function(l0){
    left_join(l0, pred_df, by = setdiff(colnames(pred_df), "surv")) %>%
      pull(surv)
  }
  return(pred_fun)
}
