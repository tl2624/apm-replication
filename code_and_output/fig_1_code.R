# Setup -------------------------------------------------------------------

## Load necessary libraries for data manipulation
library(dplyr)
library(magrittr)
library(ggplot2)

## Load pre-processed data (`ptpdata.RData`) which contains mortality rates 
## for Missouri (treated) and neighboring states (control)
load(file = "data/ptpdata.RData")

# Data Preparation --------------------------------------------------------

## Aggregate data to compute the **mean crude rate** for treated (Missouri) 
## and control (neighboring states) groups, **grouped by year**.
desc_plot_states_data <- group_by(.data = ptpdata, year, group) %>%
  summarize(mean_crude_rate = mean(crude_rate),  # Calculate average crude homicide rate per year
            .groups = "keep") %>%                # Keep grouping structure
  mutate(group = factor(x = group,               # Convert `group` into a labeled factor
                        levels = c(0, 1), 
                        labels = c("Control (Neighboring states)",
                                   "Treated (Missouri)")))

# Visualization -----------------------------------------------------------

## Create a **line plot** comparing the **average crude homicide rate** in treated vs. control states
desc_plot_treated_control <- ggplot(data = desc_plot_states_data,
                                    mapping = aes(x = year, 
                                                  y = mean_crude_rate, 
                                                  group = group)) +
  theme_bw() +  # Apply a clean black-and-white theme
  
  ## Add points and lines for each group (treated vs. control)
  geom_point(mapping = aes(color = group)) +  # Scatter points
  geom_line(mapping = aes(color = group)) +   # Connect points with lines
  
  ## Customize x-axis: Label years sequentially, marking 2008+ differently
  scale_x_continuous(breaks = sort(unique(desc_plot_states_data$year)), 
                     labels = c(head(x = sort(unique(desc_plot_states_data$year)), 
                                     n = -1), "2008 +")) +
  
  ## Set y-axis limits and tick marks for better readability
  scale_y_continuous(limits = c(0, 9), 
                     breaks = c(0, 2.5, 5, 7.5, 9)) +
  
  ## Rotate x-axis labels for better visibility and remove legend title
  theme(axis.text.x = element_text(angle = 90, size = 8), 
        legend.title = element_blank()) +
  
  ## Add axis labels and customize legend
  labs(x = "Year", 
       y = "Average Gun Homicides (rate per 100k)", 
       color = "Group") +
  
  ## Define custom colors for treated (Missouri) and control groups
  scale_color_manual(values = c("gray", "black")) +
  
  ## Add a vertical **dotted line** at 2007 to mark the **treatment year**
  geom_vline(xintercept = 2007, 
             linetype = "dotted")  

## Display the plot in the RStudio viewer or plotting window
desc_plot_treated_control

# Save Plot ---------------------------------------------------------------

## Save the generated plot as a **high-resolution PDF file**
ggsave(plot = desc_plot_treated_control,       # Save the `desc_plot_treated_control` plot
       filename = "code_and_output/desc_plot_treated_control.pdf",  # Output location
       width = 6,    # Set width (in inches)
       height = 4,   # Set height (in inches)
       units = "in", # Specify measurement units
       dpi = 600)    # Ensure high resolution for publication-quality output