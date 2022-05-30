## Fit Survival functions

fix_adh_es_ic <- function(trial_data, adherence_pattern,
                          baseline_covs,
                          oos_baseline_covs) {
  time_event <- constr_adhr_time_cens(
    trial_data = trial_data,
    adhr_pat = adherence_pattern
  )
  nuisance_opts <- CFsurvival.nuisance.options(
    cross.fit = FALSE, V = ifelse(TRUE, 10, 1), folds = NULL,
    eval.times = NULL, event.SL.library = lapply(c(
      "survSL.km", "survSL.rfsrc"),
      function(alg) c(alg, "All")),
    event.pred.0 = NULL, event.pred.1 = NULL,
    cens.SL.library = lapply(c(
      "survSL.km", "survSL.expreg"),
      function(alg) c(alg, "All")),
    cens.pred.0 = NULL, cens.pred.1 = NULL,
    survSL.control = list(initWeightAlg = "survSL.rfsrc"),
    survSL.cvControl = list(V = 10), save.nuis.fits = TRUE,
    prop.SL.library = lapply(c("SL.mean"), function(alg) c(alg, "All")),
    prop.pred = NULL
  )
  survfit <- CFsurvival(
    time = time_event$time, event = time_event$event,
    treat = rep(1, nrow(time_event)),
    confounders = baseline_covs,
    new.confounders = oos_baseline_covs,
    nuisance.options = nuisance_opts,
    contrasts = NULL, conf.band = FALSE, fit.treat = 1
  )
  return(rip_survfit(survfit))
}

constr_adhr_time_cens <- function(trial_data, adhr_pat = NULL) {
  adhr_dat <- trial_data[, grep("a\\.", colnames(trial_data))]
  adhr_dat <- adhr_dat[, order(colnames(adhr_dat))]
  if (!is.null(adhr_pat)) {
    trial_data$cens_time_adhr <- apply(adhr_dat, MARGIN = 1, function(x) {
      min(which(c(x != adhr_pat, TRUE)))
    })
  } else {
    # If adhr_pat is null, we don't induce censoring from adherence mismatches.
    trial_data$cens_time_adhr <- 100
  }
  trial_data$cens_time_comb <- pmin(trial_data$cens_time,
                                    trial_data$cens_time_adhr, na.rm = TRUE)
  cens <- ((trial_data$event_time > trial_data$cens_time_comb) |
             is.na(trial_data$event_time)) %>% as.numeric()
  event <- 1 - cens
  time <- pmin(trial_data$event_time, trial_data$cens_time_comb, na.rm = TRUE)
  return(data.frame("event" = event, "time" = time))
}

rip_survfit <- function(CF_surv_obj) {
  sdf <- CF_surv_obj$surv.df
  e_pred <- CF_surv_obj$nuisance$event.pred.1$obs
  e_pred <- e_pred[, ncol(e_pred)]
  marg_ic <- CF_surv_obj$IF.vals.1
  marg_ic <- marg_ic[, ncol(marg_ic)]
  ovrl <- last(sdf$surv)
  cond_ic <- marg_ic - (e_pred - ovrl)
  table(CF_surv_obj$data[, c("time", "event")])
  ret_df <-
    matrix(c("est" = e_pred, "ic" = cond_ic),
           ncol = 2, byrow = FALSE)
  colnames(ret_df) <- c("est", "ic")
  e_pred_new <- CF_surv_obj$nuisance$event.pred.1$new
  e_pred_new <- e_pred_new[, ncol(e_pred_new), drop = FALSE]
  colnames(e_pred_new) <- "pred"
  return(list("obs" = ret_df, "new" = e_pred_new))
}

