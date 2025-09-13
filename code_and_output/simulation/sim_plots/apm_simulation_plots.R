library(dplyr)
library(magrittr)
library(ggplot2)
library(grid)
library(tidyr)
library(apm)
library(patchwork)
library(ggExtra)

# --- Load dataset ---
load(file = "data/schell_et_al_sim_data.RData")

# --- Define study period and evaluation years ---
years <- 1994:2008
treated_year <- 2008
val_years <- 1999:2007

# --- Preprocess data ---
data <- schell_et_al_sim_data %>%
  filter(Year %in% years) %>%
  select(State, Year, Crude.Rate) %>%
  rename(state = State, year = Year, crude_rate = Crude.Rate) %>%
  mutate(state = as.character(state))

# --- Define treated and control states ---
N_treat <- 5
set.seed(seed = 11242017, kind = "L'Ecuyer-CMRG")
treated_states <- sample(x = unique(data$state), size = N_treat)
control_states <- setdiff(x = unique(data$state), y = treated_states)

# Add group and treatment indicators
data <- data %>%
  mutate(group = ifelse(test = state %in% treated_states, yes = 1, no = 0),
         treat = ifelse(test = state %in% treated_states & year == treated_year, yes = 1, no = 0))

# --- Define candidate APM models ---
mods <- apm_mod(
  formula_list = crude_rate ~ 1,
  family = "gaussian",
  lag = 0:1,
  diff_k = 0:1,
  log = FALSE,
  time_trend = 0:1,
  fixef = FALSE
)
mods <- mods[-5]  # Remove 5th model

# --- Estimate population ATT using full data ---
fits <- apm_pre(
  models = mods,
  data = data,
  group_var = "group",
  time_var = "year",
  unit_var = "state",
  val_times = val_years,
  nsim = 10,
  cl = n_cores
)

# Select best-performing model
opt_mod <- row.names(x = summary(object = fits, order = "errors"))[1]

# Estimate ATT and get population values for simulation evaluation
ests <- apm_est(
  fits = fits,
  post_time = treated_year,
  M = 1,
  R = 10,
  all_models = TRUE,
  cl = n_cores
)
pop_est_ATT <- ests$atts[opt_mod, "ATT"]
pop_est_ATT_LB <- ests$atts[opt_mod, "LB"]
pop_est_ATT_UB <- ests$atts[opt_mod, "UB"]

# --- Simulation parameters ---
n_treats <- c(1, 3, 15, 35, 50, 500, 2000)
n_conts <- c(8, 24, 120, 280, 400, 4000, 16000)

# --- Load simulation results ---
load(file = "code_and_output/simulation/sim_res.RData")

# --- Define global sample size and model labels once ---

# Sample size labels (with line breaks for all plots)
sample_size_labels <- paste0(n_treats, " treated,\n", n_conts, " control")

# Model labels: capitalize first letter for display
model_names_in_probs <- colnames(mod_post_probs[[1]])
model_labels <- sub("^([a-z])", "\\U\\1", model_names_in_probs, perl = TRUE)
names(model_labels) <- model_names_in_probs  # map back to original names

# Expected Posterior Probability Plot -------------------------------------

# Construct posterior probability dataframe with 95% quantile intervals
post_prob_sim_data <- do.call(
  what = rbind,
  args = lapply(X = seq_along(mod_post_probs), FUN = function(j) {
    probs_mat <- mod_post_probs[[j]]
    means <- colMeans(x = probs_mat)
    
    # Compute quantiles for each model across simulation draws
    quantiles <- t(apply(X = probs_mat, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)))
    
    data.frame(
      sample_size = rep(paste0(n_treats[j], " treated, ", n_conts[j], " control"), length(means)),
      model = names(means),
      stat = as.numeric(means),
      lower = quantiles[, 1],
      upper = quantiles[, 2]
    )
  })
)

# Use pre-defined sample size labels
post_prob_sim_data$sample_size <- factor(
  x = gsub(", ", ",\n", post_prob_sim_data$sample_size),
  levels = sample_size_labels
)

# Set model factor levels and labels using predefined model_labels
post_prob_sim_data$model <- factor(
  x = post_prob_sim_data$model,
  levels = names(model_labels),
  labels = model_labels
)

ggplot(post_prob_sim_data, aes(x = model, y = stat)) +
  geom_col(fill = "grey30", width = 0.85) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.2,
    color = "grey70",
    linewidth = 0.6
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
  facet_wrap(~sample_size, nrow = 1, strip.position = "top") +
  labs(
    title = "Expected Posterior Probability of Candidate Models",
    x = "Model",
    y = "Probability"
  ) +
  scale_x_discrete(labels = model_labels) +
  scale_y_continuous(
    limits = c(0, 1.05),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 10)),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7, margin = margin(t = 5, b = 5)),
    axis.text.x = element_text(size = 5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    panel.grid.major = element_line(linewidth = 0.5, linetype = "solid", color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    plot.margin = unit(c(1.5, 1, 1.2, 2), "lines"),
    legend.position = "none"
  )

# Save plot
ggsave(
  filename = "code_and_output/simulation/sim_plots/mod_prob_sim_res_plot.pdf",
  plot = last_plot(),
  device = cairo_pdf,
  width = 7,
  height = 4,
  units = "in",
  dpi = 300,
  bg = "white"
)

# --- Determine index of optimal model (used in population fit) ---
opt_mod <- row.names(x = summary(object = fits, order = "errors"))[1]
opt_mod_index <- match(opt_mod, names(mods))  # Get its index position

# Build model selection proportion dataframe
model_selection_df <- data.frame(
  SampleSize = factor(sample_size_labels, levels = sample_size_labels),
  Proportion = sapply(mod_post_probs, function(mat) {
    mean(apply(mat, 1, function(row) which.max(row) == opt_mod_index))
  })
)

# Plot
ggplot(model_selection_df, aes(x = SampleSize, y = Proportion)) +
  geom_col(fill = "gray40", color = "black") +
  geom_text(aes(label = round(Proportion, 2)), vjust = -0.5, size = 3) +
  ylim(0, 1) +
  labs(
    title = "Proportion of Simulations in which Optimal Model Has Greatest Posterior Probability",
    x = NULL,
    y = "Proportion"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save to file
ggsave(
  filename = "code_and_output/simulation/sim_plots/model_selection_optimal.pdf",
  width = 7,
  height = 4,
  device = cairo_pdf,
  dpi = 300
)

# Absolute percent bias figure ----------------------------------------------

# Compute absolute percent bias
abs_perc_bias_df <- do.call(rbind, lapply(seq_along(BMA_ATT_ests), function(j) {
  ests <- colMeans(BMA_ATT_ests[[j]], na.rm = TRUE)
  
  # Define population targets
  targets <- c(ATT = pop_est_ATT, LB = pop_est_ATT_LB, UB = pop_est_ATT_UB)
  
  # Extract corresponding estimates
  estimates <- c(ATT = ests["ATT"], LB = ests["LB"], UB = ests["UB"])
  
  data.frame(
    sample_size = sample_size_labels[j],
    type = c("ATT", "Lower bound", "Upper bound"),
    abs_bias = abs(estimates - targets),  # new absolute bias column
    abs_perc_bias = abs((estimates - targets) / abs(targets)) * 100  # existing percent bias
  )
}))

# Set factor levels
abs_perc_bias_df$sample_size <- factor(abs_perc_bias_df$sample_size, levels = sample_size_labels)
abs_perc_bias_df$type <- factor(abs_perc_bias_df$type, levels = c("ATT", "Lower bound", "Upper bound"))

ggplot(abs_perc_bias_df, aes(x = type, y = abs_perc_bias, fill = type)) +
  geom_col(width = 0.9, color = "black") +
  geom_text(aes(label = round(abs_perc_bias, digits = 0)), vjust = -0.5, size = 2) +
  facet_wrap(vars(sample_size), nrow = 1, strip.position = "top") +
  scale_fill_manual(values = c("gray30", "gray60", "gray85"), guide = "none") +
  labs(title = "Absolute Percent Bias", x = NULL, y = "Percent") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7, margin = margin(t = 5, b = 5)),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.spacing = unit(1, "lines"),
    legend.position = "none"
  )

ggsave(
  filename = "code_and_output/simulation/sim_plots/abs_percent_bias_plot.pdf",
  device = cairo_pdf,
  width = 7.5,
  height = 4.2,
  dpi = 300,
  bg = "white"
)

# 95 % confidence interval coverage ---------------------------------------

# Compute coverage proportions
coverage_df <- do.call(rbind, lapply(seq_along(CI_cov_95), function(j) {
  means <- colMeans(CI_cov_95[[j]], na.rm = TRUE)
  data.frame(
    sample_size = sample_size_labels[j],
    stat = c(means["ATT"], means["LB"], means["UB"]),
    type = c("ATT", "Lower bound", "Upper bound")
  )
}))

coverage_df$sample_size <- factor(coverage_df$sample_size, levels = sample_size_labels)
coverage_df$type <- factor(coverage_df$type, levels = c("ATT", "Lower bound", "Upper bound"))

ggplot(coverage_df, aes(x = type, y = stat, fill = type)) +
  geom_col(width = 0.9, color = "black") +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black", linewidth = 0.6) +
  facet_wrap(vars(sample_size), nrow = 1, strip.position = "top") +
  scale_fill_manual(values = c("gray30", "gray60", "gray85"), guide = "none") +
  labs(title = "Coverage of 95% Confidence Bounds", x = NULL, y = "Proportion") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7, margin = margin(t = 5, b = 5)),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    panel.spacing = unit(1, "lines"),
    legend.position = "none"
  )

ggsave(
  filename = "code_and_output/simulation/sim_plots/95_perc_CI_cov_plot.pdf",
  device = cairo_pdf,
  width = 7.5,
  height = 4.3,
  dpi = 300,
  bg = "white"
)

# Var ratio ---------------------------------------------------------------

# Compute expected vs true variance ratio
var_ratio_df <- do.call(rbind, lapply(seq_along(var_BMA_ATT_ests), function(j) {
  exp_var <- colMeans(var_BMA_ATT_ests[[j]], na.rm = TRUE)
  true_var <- apply(BMA_ATT_ests[[j]], 2, function(col) mean((col - mean(col))^2, na.rm = TRUE))
  
  data.frame(
    sample_size = sample_size_labels[j],
    stat = exp_var / true_var,
    type = c("ATT", "Lower bound", "Upper bound")
  )
}))

# Set factor levels for consistent ordering
var_ratio_df$type <- factor(var_ratio_df$type, levels = c("ATT", "Lower bound", "Upper bound"))
var_ratio_df$sample_size <- factor(var_ratio_df$sample_size, levels = sample_size_labels)

# Determine max y-axis value
y_max <- ceiling(max(var_ratio_df$stat, na.rm = TRUE))

# Create variance ratio plot
ggplot(var_ratio_df, aes(x = type, y = stat, fill = type)) +
  geom_col(width = 0.9, color = "black") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.6) +
  facet_wrap(vars(sample_size), nrow = 1, strip.position = "top") +
  scale_fill_manual(values = c("gray30", "gray60", "gray85"), guide = "none") +
  labs(
    title = "Expected Estimated Variance Relative to Variance Across Simulations",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, y_max)) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 7, margin = margin(t = 5, b = 5)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.major = element_line(linewidth = 0.5, color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.spacing = unit(1, "lines"),
    plot.margin = unit(c(1.5, 1, 1.2, 2), "lines")
  )

# Save plot
ggsave(
  filename = "code_and_output/simulation/sim_plots/variance_ratio_plot.pdf",
  plot = last_plot(),
  device = cairo_pdf,
  width = 7.2,
  height = 4.2,
  units = "in",
  dpi = 300,
  bg = "white"
)