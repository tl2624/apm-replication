# --- Load required libraries ---
library(parallel)
library(dplyr)
library(magrittr)
library(apm)

# --- Load dataset ---
load(file = "data/schell_et_al_sim_data.RData")

# --- Study period ---
years <- 1994:2008
treated_year <- 2008
val_years <- 1999:2007

# --- Preprocess data ---
data <- schell_et_al_sim_data %>%
  filter(Year %in% years) %>%
  select(State, Year, Crude.Rate) %>%
  rename(state = State, year = Year, crude_rate = Crude.Rate) %>%
  mutate(state = as.character(state))

# --- Treated and control states ---
N_treat <- 5
set.seed(11242017, kind = "L'Ecuyer-CMRG")
treated_states <- sample(unique(data$state), N_treat)
control_states <- setdiff(unique(data$state), treated_states)

data <- data %>%
  mutate(group = ifelse(state %in% treated_states, 1, 0),
         treat = ifelse(state %in% treated_states & year == treated_year, 1, 0))

# --- Models ---
mods <- apm_mod(formula_list = crude_rate ~ 1,
                family = "gaussian",
                lag = 0:1,
                diff_k = 0:1,
                log = FALSE,
                time_trend = 0:1,
                fixef = FALSE)[-5]

# --- Population-level estimate ---
fits <- apm_pre(models = mods,
                data = data,
                group_var = "group",
                time_var = "year",
                val_times = val_years,
                unit_var = "state",
                nsim = 10,
                cl = n_cores)
opt_mod <- rownames(summary(fits, order = "errors"))[1]
ests <- apm_est(fits = fits,
                post_time = treated_year,
                M = 1,
                R = 10,
                all_models = TRUE,
                cl = n_cores)

pop_est_ATT <- ests$atts[opt_mod, "ATT"]
pop_est_ATT_LB <- ests$atts[opt_mod, "LB"]
pop_est_ATT_UB <- ests$atts[opt_mod, "UB"]
alpha <- 0.05

# --- Simulation parameters ---
n_sims <- 1000
n_treats <- c(1, 3, 15, 35, 50, 500, 2000)
n_conts <- c(8, 24, 120, 280, 400, 4000, 16000)
n_draws <- 1000
n_boots <- 1000
j_values <- seq_along(n_treats)

# --- Storage objects ---
BMA_ATT_ests <- var_BMA_ATT_ests <- CI_cov_95 <- mod_post_probs <- vector("list", length(n_treats))
BMA_var_boot <- BMA_var_model <- vector("list", length(n_treats))
model_ATT_ests <- vector("list", length(n_treats))
names(BMA_ATT_ests) <- names(var_BMA_ATT_ests) <- names(CI_cov_95) <-
  names(mod_post_probs) <- names(BMA_var_boot) <- names(BMA_var_model) <-
  names(model_ATT_ests) <- paste0("n_treat_", n_treats)

for (j in seq_along(n_treats)) {
  BMA_ATT_ests[[j]] <- matrix(NA, n_sims, 3, dimnames = list(NULL, c("ATT", "LB", "UB")))
  var_BMA_ATT_ests[[j]] <- matrix(NA, n_sims, 3, dimnames = list(NULL, c("ATT", "LB", "UB")))
  CI_cov_95[[j]] <- matrix(FALSE, n_sims, 3, dimnames = list(NULL, c("ATT", "LB", "UB")))
  mod_post_probs[[j]] <- matrix(NA, n_sims, length(mods), dimnames = list(NULL, names(mods)))
  BMA_var_boot[[j]] <- matrix(NA, n_sims, 3, dimnames = list(NULL, c("ATT", "LB", "UB")))
  BMA_var_model[[j]] <- matrix(NA, n_sims, 3, dimnames = list(NULL, c("ATT", "LB", "UB")))
  model_ATT_ests[[j]] <- vector("list", n_sims)
}

# --- Simulation grid ---
sim_grid <- expand.grid(j = j_values, i = seq_len(n_sims))
sim_grid <- sim_grid[order(sim_grid$j, sim_grid$i), ]

# --- Start cluster ---
set.seed(09291992, kind = "L'Ecuyer-CMRG")
cl <- makeCluster(n_cores)
clusterExport(cl, varlist = c("data", "treated_states", "control_states", "mods", "val_years", "treated_year",
                              "n_draws", "n_boots", "alpha", "pop_est_ATT", "pop_est_ATT_LB", "pop_est_ATT_UB",
                              "sim_grid", "n_treats", "n_conts"))
clusterEvalQ(cl, { library(dplyr); library(apm) })

# --- Worker function ---
sim_worker <- function(k) {
  
  j <- sim_grid$j[k]
  i <- sim_grid$i[k]
  
  samp_treat <- sample(treated_states, n_treats[j], replace = TRUE)
  samp_control <- sample(control_states, n_conts[j], replace = TRUE)
  
  samp_data <- bind_rows(
    lapply(seq_along(samp_treat), function(k) data.frame(id = k, data[data$state == samp_treat[k], ])),
    lapply(seq_along(samp_control), function(k) data.frame(id = n_treats[j] + k, data[data$state == samp_control[k], ]))
  )
  
  model_subset <- if (j == length(n_treats)) {
    named_mods <- mods[top_2_mods]
    names(named_mods) <- names(mods)[top_2_mods]
    named_mods
  } else {
    mods
  }
  
  fits <- apm_pre(models = model_subset,
                  data = samp_data,
                  group_var = "group",
                  time_var = "year",
                  val_times = val_years,
                  unit_var = "id",
                  nsim = n_draws)
  est <- apm_est(fits = fits,
                 post_time = treated_year,
                 M = 1,
                 R = n_boots,
                 all_models = TRUE)
  
  list(
    j = j,
    i = i,
    BMA_ATT = est$BMA_att,
    BMA_var = est$BMA_var,
    BMA_var_b = est$BMA_var_b,
    BMA_var_m = est$BMA_var_m,
    model_atts = est$atts,
    cov_ATT = est$BMA_att["ATT"] - qnorm(1 - alpha / 2) * sqrt(est$BMA_var["ATT"]) <= pop_est_ATT &
      pop_est_ATT <= est$BMA_att["ATT"] + qnorm(1 - alpha / 2) * sqrt(est$BMA_var["ATT"]),
    cov_LB = est$BMA_att["LB"] - qnorm(1 - alpha / 2) * sqrt(est$BMA_var["LB"]) <= pop_est_ATT_LB &
      pop_est_ATT_LB <= est$BMA_att["LB"] + qnorm(1 - alpha / 2) * sqrt(est$BMA_var["LB"]),
    cov_UB = est$BMA_att["UB"] - qnorm(1 - alpha / 2) * sqrt(est$BMA_var["UB"]) <= pop_est_ATT_UB &
      pop_est_ATT_UB <= est$BMA_att["UB"] + qnorm(1 - alpha / 2) * sqrt(est$BMA_var["UB"]),
    BMA_weights = fits$BMA_weights
  )
}

# --- Run j = 1 to 6 ---
results_list <- vector("list", length(n_treats))
names(results_list) <- paste0("j_", seq_along(n_treats))
for (j in seq_len(length(n_treats) - 1)) {
  results_list[[j]] <- parLapply(cl, which(sim_grid$j == j), sim_worker)
}

# --- Top 2 models based on j = 6 ---
post_probs_j_6 <- do.call(what = rbind,
                          args = lapply(X = results_list[["j_6"]],
                                        FUN = function(x) x$BMA_weights))
colnames(post_probs_j_6) <- names(mods)
avg_post_probs_j_6 <- colMeans(post_probs_j_6)
top_2_model_names <- names(sort(avg_post_probs_j_6, decreasing = TRUE)[1:2])
top_2_mods <- match(top_2_model_names, names(mods))
clusterExport(cl, "top_2_mods")

# --- Initialize mod_post_probs[[7]] based on top 2 models ---
mod_post_probs[[7]] <- matrix(
  NA_real_,
  nrow = n_sims,
  ncol = length(top_2_mods),
  dimnames = list(NULL, names(mods[top_2_mods]))
)

# --- Run j = 7 with top 2 models ---
results_list[["j_7"]] <- parLapply(cl, which(sim_grid$j == 7), sim_worker)

# --- Combine results ---
for (j in seq_along(n_treats)) {
  results_j <- results_list[[paste0("j_", j)]]
  for (res in results_j) {
    i <- res$i
    BMA_ATT_ests[[j]][i, ] <- res$BMA_att
    var_BMA_ATT_ests[[j]][i, ] <- res$BMA_var
    BMA_var_boot[[j]][i, ] <- res$BMA_var_b
    BMA_var_model[[j]][i, ] <- res$BMA_var_m
    model_ATT_ests[[j]][[i]] <- res$model_atts
    CI_cov_95[[j]][i, ] <- c(res$cov_ATT, res$cov_LB, res$cov_UB)
    mod_post_probs[[j]][i, ] <- res$BMA_weights
  }
}

# --- Pad mod_post_probs for j_7 ---
full_model_names <- names(mods)
j_7_mat <- mod_post_probs[["n_treat_2000"]]
padded_j_7 <- matrix(data = 0,
                     nrow = nrow(j_7_mat),
                     ncol = length(full_model_names),
                     dimnames = list(rownames(j_7_mat), full_model_names))
padded_j_7[, colnames(j_7_mat)] <- j_7_mat
mod_post_probs[["n_treat_2000"]] <- padded_j_7

# --- Save and shutdown ---
stopCluster(cl)
save(BMA_ATT_ests, var_BMA_ATT_ests, CI_cov_95, mod_post_probs,
     BMA_var_boot, BMA_var_model, model_ATT_ests,
     file = "code_and_output/simulation/sim_res.RData")