# Setup -------------------------------------------------------------------

library(apm)
library(ggplot2)

## Load previously saved model fits
load(file = "code_and_output/fits.RData")

# Model Prediction Error Visualization -----------------------------------

## Generate a bar plot of absolute prediction errors across models and validation periods
mod_pred_errors_barplot <- plot(fits,
                                type = "error", # Plots absolute difference in average prediction errors for all models across validation periods
                                ncol = 6) +
  ## Adjust theme settings for improved readability
  theme(axis.text.x = element_text(angle = 90, # Rotate x-axis labels for better visibility
                                   size = 5), # Reduce x-axis label size
        axis.text.y = element_text(size = 5), # Reduce y-axis label size
        strip.text.x = element_text(size = 4.25)) # Adjust facet strip label size

# Save Plot ---------------------------------------------------------------

## Save the plot as a high-resolution PDF file for documentation and reporting
ggsave(plot = mod_pred_errors_barplot,
       filename = "code_and_output/mod_pred_errors_barplot.pdf", # Output file path
       width = 6, # Width of the saved plot in inches
       height = 4, # Height of the saved plot in inches
       units = "in", # Specify measurement units
       dpi = 600) # Set resolution for high-quality output