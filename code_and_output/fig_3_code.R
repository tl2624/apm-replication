# Setup -------------------------------------------------------------------

## Load required libraries
library(ggplot2)
library(ggrepel)

## Load previously saved model fits
load(file = "code_and_output/ests.RData")

## Define a function `.colMax()` to compute the **maximum value in each column** of a matrix or data frame.
## - `x`: Input matrix or data frame.
## - `na.rm = TRUE`: Removes missing values (`NA`) before calculating the maximum.
.colMax <- function(x, na.rm = TRUE) {
  apply(x, 2L, max, na.rm = na.rm)  # Apply `max()` function column-wise (`2L` refers to columns)
}


# Model Maximum Error, Robustness Visualization -----------------------------------

max_abs_pred_error_diffs <- .colMax(abs(ests[["pred_error_diffs"]]))
est <- ests[["atts"]][, 1L]

labels <- rownames(ests[["atts"]])

plot_data <- data.frame(estimate = est,
                        pred_error = max_abs_pred_error_diffs[names(est)],
                        label = labels,
                        weights = ests[["BMA_weights"]],
                        best = seq_along(est) == which.min(max_abs_pred_error_diffs[names(est)]))

p <- ggplot(plot_data,
            aes(x = .data$pred_error,
                y = .data$estimate)) +
  geom_point(aes(color = .data$best,
                 size = .data$weights),
             alpha = .8) +
  scale_color_manual(values = c("TRUE" = "black", "FALSE" = "gray")) +
  guides(color = "none", size = "none") +
  labs(x = "Maximum absolute pre-treatment difference in average prediction errors",
       y = "Estimate") +
  theme_bw()

ests_max_errs_plot <- p + geom_text_repel(mapping = aes(label = .data$label),
                                          box.padding = 1,
                                          point.padding = 1,
                                          min.segment.length = .2,
                                          size = 4)

# Save Plot ---------------------------------------------------------------

## Save the plot as a high-resolution PDF file for documentation and reporting
ggsave(plot = ests_max_errs_plot,
       filename = "code_and_output/ests_max_errs_plot.pdf", # Output file path
       width = 6, # Width of the saved plot in inches
       height = 4, # Height of the saved plot in inches
       units = "in", # Specify measurement units
       dpi = 600) # Set resolution for high-quality output