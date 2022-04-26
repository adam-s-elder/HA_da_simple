## Estimate the distribution of the propensity score in ASPIRE

est_aspr_ps <- function(hope_aspire_data) {
  this_dat <- hope_aspire_data %>% filter(trial == "aspr", arm == 1)
  a1mod <- glm(a.1 ~ l_0, data = this_dat, family = "binomial")
  a2mod <- glm(a.2 ~ l_0 + a.1, data = this_dat, family = "binomial")
  a3mod <- glm(a.3 ~ l_0 + a.1 + a.2, data = this_dat, family = "binomial")
  a4mod <- glm(a.4 ~ l_0 + a.1 + a.2 + a.3,
               data = this_dat, family = "binomial")
  all_pos <- expand.grid(l_0 = unique(this_dat$l_0),
                         a.1 = c(0, 1), a.2 = c(0, 1),
                         a.3 = c(0, 1), a.4 = c(0, 1))
  all_pos$pr1 <- predict(a1mod, all_pos, type = "response")
  all_pos$pr2 <- predict(a2mod, all_pos, type = "response")
  all_pos$pr3 <- predict(a3mod, all_pos, type = "response")
  all_pos$pr4 <- predict(a4mod, all_pos, type = "response")
  all_pos$jnt_pr <-
    all_pos$pr1 ** all_pos$a.1 * (1 - all_pos$pr1) ** (1 - all_pos$a.1) *
    all_pos$pr2 ** all_pos$a.2 * (1 - all_pos$pr2) ** (1 - all_pos$a.2) *
    all_pos$pr3 ** all_pos$a.3 * (1 - all_pos$pr3) ** (1 - all_pos$a.3) *
    all_pos$pr4 ** all_pos$a.4 * (1 - all_pos$pr4) ** (1 - all_pos$a.4)
  jnt_pred <- all_pos %>% select(l_0, a.1, a.2, a.3, a.4, jnt_pr)
  pred_fun <- function(l0_and_a){
    left_join(l0_and_a, jnt_pred, by = colnames(l0_and_a)) %>%
      pull(jnt_pr)
  }
  return(pred_fun)
}
