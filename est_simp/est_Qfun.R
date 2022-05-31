## Estimate the probability of the counter factual outcome under HOPE adherence.
# library(CFsurvival)

surv_pred_fun <- function(surv_preds) {
  if (!is.null(surv_preds$marg_aspr_df)) {
    comb_dat <-
      bind_rows(
        surv_preds$marg_aspr_df %>% dplyr::select(pid, l_0, est),
        surv_preds$marg_hope_df %>% dplyr::select(pid, l_0, est = pred)
      ) %>% group_by(l_0) %>% summarise(est = unique(est)) %>%
      ungroup()
  } else {
    comb_dat <-
      bind_rows(
        surv_preds$obs %>% dplyr::select(pid, l_0, est),
        surv_preds$new %>% dplyr::select(pid, l_0, est = pred)
      ) %>% group_by(l_0) %>% summarise(est = unique(est)) %>%
      ungroup()
  }
  pred_fun <- function(l0){
    left_join(l0, comb_dat, by = setdiff(colnames(comb_dat), "est")) %>%
      pull(est)
  }
  return(pred_fun)
}

est_cf_surv <- function(hope_aspire_data, hope_prop_score) {
  aspr <- hope_aspire_data %>% filter(trial == "aspr", arm == 1)
  hope <- hope_aspire_data %>% filter((trial == "hope") | (arm == 0))
  hope_base <- hope %>%
    dplyr::select(l_0) %>% mutate(l_0 = as.factor(l_0))
  aspr_base <- aspr %>% dplyr::select(l_0) %>%
    mutate(l_0 = as.factor(l_0))
  all_adhr <- rep(list(c(0, 1)), 4)
  names(all_adhr) <- paste0("cf.a.", 0:3)
  all_adhr <- all_adhr %>% do.call(what = expand.grid)
  adhr_sub <- all_adhr[sample(1:nrow(all_adhr), size = 4, replace = TRUE), ]
  many_fits <- 1:nrow(adhr_sub) %>%
    purrr::map(.f = function(x) {
      ad_patr <- adhr_sub[x, ]
      list(fit = fix_adh_es_ic(
        trial_data = aspr, adherence_pattern = ad_patr,
        baseline_covs = aspr_base,
        oos_baseline_covs = hope_base),
        adhr = ad_patr)
    })
  many_fits <- many_fits %>% map(.f = function(x) {
    adhr <- x$adhr
    rownames(adhr) <- NULL
    obs <- bind_cols("id" = 1:nrow(aspr), aspr, x$fit$obs, adhr)
    obs$prop <- obs %>%
      dplyr::select(l_0, a.0 = cf.a.0, a.1 = cf.a.1,
             a.2 = cf.a.2, a.3 = cf.a.3) %>% hope_prop_score()
    new <- bind_cols("id" = 1:nrow(hope), hope, x$fit$new, adhr)
    new$prop <- new %>%
      dplyr::select(l_0, a.0 = cf.a.0, a.1 = cf.a.1,
             a.2 = cf.a.2, a.3 = cf.a.3) %>% hope_prop_score()
    return(list(obs = obs, new = new))
  })
  obs <- many_fits %>% map(.f = function(x) x$obs) %>%
    do.call(what = bind_rows)
  obs_ave <- obs %>% group_by(id) %>%
    mutate(prop = prop / sum(prop)) %>%
    summarise(id = unique(id), ic = sum(prop * ic),
              est = sum(prop * est)) %>%
    ungroup() %>% arrange(id)
  new <- many_fits %>% map(.f = function(x) x$new) %>%
    do.call(what = bind_rows)
  new_ave <- new %>% group_by(id) %>%
    mutate(prop = prop / sum(prop)) %>%
    summarise(id = unique(id), pred = sum(pred * prop)) %>%
    ungroup() %>% arrange(id)

  marg_hope_df <- cbind(new_ave, hope)
  marg_aspr_df <- cbind(obs_ave, aspr)
  return(list(
    hope_df = new, aspr_df = obs,
    marg_hope_df = marg_hope_df,
    marg_aspr_df = marg_aspr_df
   ))
}

est_obs_surv <- function(fit_data, othr_data) {
  othr_base <- othr_data %>%
    dplyr::select(l_0) %>% mutate(l_0 = as.factor(l_0))
  fit_base <- fit_data %>% dplyr::select(l_0) %>%
    mutate(l_0 = as.factor(l_0))
  fit <- fix_adh_es_ic(
        trial_data = fit_data,
        adherence_pattern = NULL,
        baseline_covs = fit_base,
        oos_baseline_covs = othr_base)
  return(list(
    obs = bind_cols(fit_data, fit$obs),
    new = bind_cols(othr_data, fit$new),
    param_est = fit$param_est
    ))
}
