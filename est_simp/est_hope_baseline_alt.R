est_hope_l0 <- function(hope_aspire_data) {
  browser()
  hope_dat <- hope_aspire_data %>% filter(trial == "hope") %>%
    mutate(y = event_time < 5)
  task <- sl3::sl3_Task$new(
    data = hope_dat,
    covariates = "l_0",
    outcome = "y",
    drop_missing_outcome = TRUE

  )
  glm_learner <- sl3::Lrnr_glm$new()
  learner_stack <- Stack$new(glm_learner)
  stack_fit <- learner_stack$train(task)
  preds <- stack_fit$predict()


  pred_fun <- function(l0){
    left_join(l0, obs_l0, by = "l_0") %>%
      pull(prop)
  }
  return(pred_fun)
}
