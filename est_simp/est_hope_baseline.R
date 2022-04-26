## Estimate the distribution of baseline characteristics in HOPE

est_hope_baseline <- function(hope_aspire_data) {
  obs_l0 <- hope_aspire_data %>% filter(trial == "hope") %>% group_by(l_0) %>%
    summarise(n = n()) %>% ungroup() %>%
    mutate(prop = n / sum(n)) %>% select(l_0, prop)
  pred_fun <- function(l0){
    left_join(l0, obs_l0, by = "l_0") %>%
      pull(prop)
  }
  return(pred_fun)
}
