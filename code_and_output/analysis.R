# Setup -------------------------------------------------------------------

## Load required libraries
library(parallel)
library(apm)
library(dplyr)

# Set a reproducible random seed for parallel computing
# Using "L'Ecuyer-CMRG", a parallel-safe random number generator (RNG),
# ensures consistent and independent random streams across multiple cores.
# This is particularly useful when running simulations or bootstrapping in parallel.
set.seed(11242017, "L'Ecuyer-CMRG")

# Define Study Periods ---------------------------------------------------

## Validation years: 1999 - 2007 (used for model validation)
val_years <- 1999:2007

## Treated year: 2008 (post-policy/intervention year)
treated_year  <- 2008

# Model Specification ----------------------------------------------------

## Define set of models using apm_mod function
models <- apm_mod(
  formula_list = crude_rate ~ 1, # Model formula with outcome (crude_rate) and intercept as predictor
  family = "gaussian", # Specifies linear model with Gaussian distribution
  lag = 0:1, # Includes both current and one-year lagged outcome as potential predictors
  diff_k = 0:1, # Uses lagged outcome zero times and one time as offset for differencing models
  log = c(TRUE, FALSE), # Includes models both with and without log-transformed outcome
  time_trend = 0:2, # Includes models with no time trend, linear time trend, and linear time trend + quadratic time trend
  fixef = TRUE # Includes unit fixed effects for all models
)

# Model Estimation -------------------------------------------------------

## Perform pre-treatment model fitting with validation
fits <- apm_pre(
  models = models, # Uses the previously defined models
  data = ptpdata, # Dataset containing observations
  group_var = "group", # Variable name indicating to be treated and comparison groups
  time_var = "year", # Name of time variable
  unit_var = "state", # Name of unit variable (e.g., states in the dataset)
  val_times = val_years, # Validation years (1999-2007)
  nsim = 10^5, # Number of draws from multivariate gaussian for Bayesian Model Averaging (BMA) weights
  cl = n_cores # n_cores is defined in master.R
)

## Save output of apm_pre() for future analysis and reproducibility
save(fits, file = "code_and_output/fits.RData")

# Estimation of Treatment Effect -----------------------------------------

## Compute treatment effect estimates for the treated period
ests <- apm_est(
  fits = fits, # Uses pre-treatment fitted models
  post_time = treated_year, # Treated period (2008)
  M = 1, # Sensitivity parameter for set identification
  R = 10^5, # Number of bootstrap replications
  all_models = TRUE, # Fits all models, not only those with >0 BMA weights
  cl = n_cores # n_cores is defined in master.R
)

## Save output of apm_est() for future analysis and reproducibility
save(ests, file = "code_and_output/ests.RData")

# Model Performance Evaluation ------------------------------------------

## Calculate absolute differences in average prediction errors for all models and validation periods
abs_val_errors <- abs(fits$pred_error_diffs)

## Identify optimal models based on absolute differences in average prediction errors
mod_max_errs <- apply(X = abs_val_errors,
                      MARGIN = 2,
                      FUN = max)
names(fits$models)[order(mod_max_errs)]

## Identify the last pre-treatment year by finding the maximum year 
## where the treated group (Missouri) has not yet received the treatment (treat == 0)
## Convert the year to a character string using `paste()`
last_pre_treat_year <- paste(max(ptpdata$year[ptpdata$group == 1 & ptpdata$treat == 0]))

## Arrange models in ascending order based on the absolute error for the last pre-treatment year (2007)
names(fits$models)[order(abs_val_errors[last_pre_treat_year, ])]

## Arrange models in ascending order based on average error over all validation periods
mod_avg_errs <- apply(X = abs_val_errors,
                      MARGIN = 2,
                      FUN = mean)
names(fits$models)[order(mod_avg_errs)]

## Compute standard deviation of point estimates across models
round(x = sqrt(mean((ests$atts[,"ATT"] - mean(ests$atts[,"ATT"]))^2)),
      digits = 2)

## Compute point estimates for most and least robust models
round(x = ests$att[which(rownames(ests$atts) == names(fits$models)[which.min(mod_max_errs)]), "ATT"],
      digits = 2) ## Most robust
round(x = ests$att[which(rownames(ests$atts) == names(fits$models)[which.max(mod_max_errs)]), "ATT"],
      digits = 2) # Least robust

# Compare BMA model with sample's most robust model -----------------------

## Compute bounds of the most robust model with M = 1
round(x = ests$att[which(rownames(ests$atts) == names(fits$models)[which.min(mod_max_errs)]),],
      digits = 2)

## Compute BMA estimates
round(x = ests$BMA_att,
      digits = 2)

## Posterior probabilities of each model
round(x = fits$BMA_weights, digits = 2)

# Sample estimation and inference -----------------------------------------

## Compute estimated standard error
## BMA_var_b is bootstrap uncertainty holding model uncertainty fixed
## BMA_var_m is model uncertainty variance with fixed sampling uncertainty
## BMA_var is sum of BMA_var_b and BMA_var_m
round(x = sqrt(ests$BMA_var["ATT"]),
      digits = 2)

## Define alpha level for confidence intervals
alpha <- 0.05

## Compute 95% confidence interval for ATT
round(x = c(ests$BMA_att["ATT"] - qnorm(p = 1 - alpha/2) * sqrt(ests$BMA_var["ATT"]),
            ests$BMA_att["ATT"] + qnorm(p = 1 - alpha/2) * sqrt(ests$BMA_var["ATT"])),
      digits = 2)

## Observed homicide rate in 2007 (just before the repeal) in Missouri
ptpdata$crude_rate[ptpdata$group == 1 & ptpdata$year == last_pre_treat_year]

## Compute percent increase in crude rate for Missouri in 2007
round(x = 100 * (ests$BMA_att["ATT"]/ptpdata$crude_rate[ptpdata$state == "Missouri" & ptpdata$year == 2007]),
      digits = 0)

## Compute changepoint value of M (no sampling uncertainty)
round(x = robustness_bound(ests, level = 0),
      digits = 2)

## Compute changepoint Ms (no sampling uncertainty) for each model
round(x = summary(ests$atts[,"ATT"] / mod_max_errs),
      digits = 2)

## Compute changepoint value of M (Lower Bound of 95% CI)
round(x = robustness_bound(ests, level = 0.95),
      digits = 2)
