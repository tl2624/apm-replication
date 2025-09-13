# Setup -------------------------------------------------------------------

## Clear R environment
rm(list = ls())

# Use up to 120 cores or all but one, whichever is smaller
n_cores <- if(parallel::detectCores() > 120) { 120 } else { parallel::detectCores() - 1 }

# Install packages --------------------------------------------------------

source("install_packages.R")

# Create data -------------------------------------------------------------

source("data/data_construction.R")

# Code to create Figure 1 in manuscript -----------------------------------

source("code_and_output/fig_1_code.R")

# Code for primary analysis in manuscript ---------------------------------

source("code_and_output/analysis.R")

## Code for figures 2 - 4 require outputs of "code_and_output/analysis.R"

# Code to create Figure 2 in manuscript -----------------------------------

source("code_and_output/fig_2_code.R")

# Code to create Figure 3 in manuscript -----------------------------------

source("code_and_output/fig_3_code.R")

# Code to create Figure 4 in manuscript -----------------------------------

source("code_and_output/fig_4_code.R")

# Code for simulation in supplement ---------------------------------------

source("code_and_output/simulation/apm_simulation.R")

# Code for simulation plots in supplement ---------------------------------

source("code_and_output/simulation/sim_plots/apm_simulation_plots.R")

# Reproducibility Best Practices ------------------------------------------

## Print session info to console
cat("\n--- SESSION INFO ---\n")
print(sessionInfo())

## OPTIONAL: Save session info to a text file for submission
sink("sessionInfo.txt")
cat("--- SESSION INFO ---\n")
print(sessionInfo())
sink()
