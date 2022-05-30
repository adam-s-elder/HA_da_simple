
library(ltmle)
make_nodes <- function(event_times, censor_times) {
  event_df <- data.frame("y.0" = rep(0, length(event_times)))
  censr_df <- data.frame(
    "c.0" = factor(
      rep("uncensored", length(censor_times)),
      levels = c("uncensored", "censored"),
      labels = c("uncensored", "censored")
    )
  )
  max_time <- max(event_times, na.rm = TRUE)
  for (time_idx in 1:max_time) {
    cens_vec <- ifelse(
      is.na(censor_times),
      ifelse(
        ## Event time = 1, so censoring at time 1 is not obvserved
        event_times >= time_idx, "uncensored", NA
      ),
      ifelse(
        censor_times >= time_idx, "uncensored", "censored"
      )
    )
    censr_df[[paste0("c.", time_idx)]] <- factor(
      cens_vec, levels = c("uncensored", "censored"),
      labels = c("uncensored", "censored")
    )
    event_df[[paste0("y.", time_idx)]] <- ifelse(
      is.na(event_times),
      ifelse(
        ## Event time = 1, so censoring at time 1 is not ovserved
        censor_times >= time_idx, 0, NA
      ),
      ifelse(
        event_times <= time_idx, 1, 0
      )
    )
  }
  all_df <- bind_cols("y" = event_times, "c" = censor_times,
                      event_df, censr_df)
  return(all_df)
}

nodes <- make_nodes(trial_dat$event_time, trial_dat$cens_time)
trial_dat_wide <- bind_cols(trial_dat, nodes)
cl_names <- colnames(trial_dat_wide)
anodes <- cl_names[grep("a\\.", cl_names)]
ynodes <- cl_names[grep("y\\.", cl_names)]
cnodes <- cl_names[grep("c\\.", cl_names)]
nodes <- c(cnodes, anodes, ynodes)
strreverse <- function(x){
  strsplit(x, NULL) %>% lapply(rev) %>% sapply(paste, collapse="")
}
nodes <- nodes[order(strreverse(gsub("a", "z", nodes)))]

subdat <- trial_dat_wide[,nodes]
subdat$y.5[subdat$y.4 %in% 0] <- rbinom(n = sum(subdat$y.4 %in% 0), size = 1,
                                        prob = 0.04)

first_success <- ltmle(data = subdat,
                       Anodes = anodes, Ynodes = ynodes, Cnodes = cnodes,
                       abar = c(0, 0, 1, 1), survivalOutcome = TRUE
)
summary(first_success)
