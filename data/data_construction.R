# Setup -------------------------------------------------------------------

# Load required packages for data manipulation
library(dplyr)
library(magrittr)

## Data originally downloaded from Hasegawa et al. (2019), p. 376
## Source: http://links.lww.com/EDE/B501
## These datasets contain mortality-related statistics from various U.S. states.

# Data Import --------------------------------------------------------------

## Load pre-treatment (1992-1998) and treatment (1999-2007) datasets
## Data is stored in tab-separated files (.txt), which require the `sep = "\t"` argument.
data_92_98 <- read.delim(file = "data/raw_data/all_states_pre_period.txt",
                         header = TRUE,   # Ensures column headers are read correctly
                         sep = "\t",      # Specifies tab-delimited data format
                         dec = ".")       # Assumes decimal separator is a period

data_99_07 <- read.delim(file = "data/raw_data/all_states_before_after_periods.txt",
                         header = TRUE,
                         sep = "\t",
                         dec = ".") 

# Define Treated and Control States ----------------------------------------

## Define Missouri as the treated state for analysis
treated_state <- "Missouri"

## Specify the neighboring states that serve as the control group
control_states <- c("Arkansas",
                    "Illinois",
                    "Iowa",
                    "Kansas",
                    "Kentucky",
                    "Nebraska",
                    "Oklahoma",
                    "Tennessee")

## Merge the treated state with the control states into a single vector
states <- union(x = treated_state, y = control_states)

## Define post-treatment period (2008-2016)
post_treat_years <- 2008:2016

# Data Preparation ---------------------------------------------------------

## Combine datasets from both time periods and filter relevant states and years
## This ensures only relevant data is retained for the study
ptpdata <- rbind(data_92_98, data_99_07) %>%   # Combine both datasets row-wise
  filter(State %in% states & Year >= 1994) %>%  # Keep only selected states and data from 1994 onwards
  select(State, Year, Deaths, Crude.Rate, Age.Adjusted.Rate) %>%  # Retain only necessary columns
  rename(state = State,             # Standardize column names for easier manipulation
         year = Year,
         deaths = Deaths,
         crude_rate = Crude.Rate,
         age_adj_rate = Age.Adjusted.Rate)

## Data Cleaning and Variable Creation --------------------------------------

## Convert variables, remove unwanted characters, and create treatment indicators
ptpdata <- mutate(ptpdata,
                  group = ifelse(state == "Missouri", 1, 0),  # Create binary indicator for treatment group
                  
                  ## Remove non-numeric characters (e.g., missing values represented as text)
                  crude_rate = gsub(pattern = "[^0-9.-]", replacement = "", x = crude_rate),
                  crude_rate = as.numeric(crude_rate),  # Convert to numeric format
                  
                  age_adj_rate = gsub(pattern = "[^0-9.-]", replacement = "", x = age_adj_rate),
                  age_adj_rate = as.numeric(age_adj_rate),
                  
                  ## Convert state column into a factor to maintain consistent ordering
                  state = factor(state, levels = states),
                  
                  ## Create binary indicator for treatment period (1 if treated post-2008, 0 otherwise)
                  treat = ifelse(state == "Missouri" & year %in% post_treat_years, 1, 0)) %>%
  
  arrange(state, year, group)  # Ensure data is properly ordered for analysis

# Recode Post-Treatment Year (2008) ---------------------------------------

## Compute average mortality rates during the post-treatment period for treated and control groups
treated_post_treat_means <- filter(ptpdata, group == 1) %>%
  group_by(state) %>%
  summarize(post_treat_mean = mean(crude_rate[year %in% post_treat_years], na.rm = TRUE))  # Handle potential missing values

control_post_treat_means <- filter(ptpdata, group == 0) %>%
  group_by(state) %>%
  summarize(post_treat_mean = mean(crude_rate[year %in% post_treat_years], na.rm = TRUE))

## Recode the 2008 crude rate for Missouri using the computed post-treatment mean
ptpdata <- mutate(ptpdata,
                  crude_rate = ifelse(state == "Missouri" & year == 2008,
                                      treated_post_treat_means$post_treat_mean[1],
                                      crude_rate)) %>%
  filter(year <= 2008)  # Exclude years beyond 2008 as per study requirements

# Export Cleaned Data -----------------------------------------------------

## Save the processed dataset as a tab-separated file for further analysis
write.table(ptpdata,
            file = "data/ptpdata.tab",
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE)

## Save the dataset in R binary format (.RData) for quick loading in future sessions
save(ptpdata, file = "data/ptpdata.RData")