

# survdf <- matrix(c(45,  31, 42,  28, 34,  18, 54, 0, 811, 0),
#                  byrow = TRUE, nrow = 2)
#
# ad_sp_df <-
#   constr_adhr_time_cens(trial_data = trial_dat, adhr_pat = c(0, 1, 0, 1))
#
# survfit2 <- CFsurvival(
#   time = ad_sp_df$time, event = ad_sp_df$event,
#   treat = rep(1, nrow(ad_sp_df)),
#   confounders = lo_df, nuisance.options = nuisance_opts,
#   contrasts = NULL, conf.band = FALSE, fit.treat = 1
# )
# event.SL.library <- cens.SL.library <- lapply(
#   c("survSL.km", "survSL.coxph"), function(alg) {
#   c(alg, "All")
# })
# a <- survSuperLearner::survSuperLearner(
#   time = time_event$time, event = time_event$event,
#   X = baseline_covs, newX = NULL, new.times = 4,
#   event.SL.library = event.SL.library,
#   cens.SL.library = cens.SL.library
# )


# y <- as.numeric(this_dat$event_time < 5)
#
# delt <- as.numeric(this_dat$cens_time >= 5)
# y[delt == 0] <- 1 #If delta = 0, censoring happened, so indicator will be 0
# delt[y == 1] <- 0 #If y = 1, event happened so indicator will be zer0
# (1 - y) * delt
# ip <- 1 / aspr_cens_fun(this_dat %>% select(l_0))
# this_dat$psudo_outcome <- (1 - y) * delt * ip
# this_dat$ip <- ip
# ### TODO: Make sure to check this!!!!OMG CHECK THIS FOR THE LOVE
# ### OF ALL THAT IS GOOD AND HOLY. PLEASE CHECK THIS.
# pred_mod <- glm(psudo_outcome ~ a.0  + a.1 + a.2 + a.3 + l_0,
#                 data = this_dat, weights = this_dat$ip,
#                 family = "gaussian")
# all_pos <- expand.grid(l_0 = unique(this_dat$l_0),
#                        a.0 = c(0, 1), a.1 = c(0, 1),
#                        a.2 = c(0, 1), a.3 = c(0, 1))
# all_pos$expect <- predict(pred_mod, all_pos, type = "response")
# all_pos$weight <- hope_prop_score(all_pos %>% select(-expect))
# all_pos <- all_pos %>% group_by(l_0) %>%
#   summarise(tot_prob = sum(weight),
#             expec = sum(expect * weight))
# pred_df <- all_pos %>% select(l_0, surv = expec)