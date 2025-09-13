# Setup -------------------------------------------------------------------

## Load required libraries
library(dplyr)
library(ggplot2)

## Load previously saved model fits and ests
load(file = "code_and_output/fits.RData")
load(file = "code_and_output/ests.RData")

# Prepare Posterior Probability Data -------------------------------------

## Create a data frame containing Bayesian Model Averaging (BMA) weights
post_data <- data.frame(prob = ests$BMA_weights, # Posterior probabilities of models
                        model = "") # Initialize model names as empty

## Identify models with non-zero probability
nonzero_models <- names(fits$models)[which(post_data$prob > 0)]

## Assign model names to those with non-zero probability
post_data$model[which(post_data$prob > 0)] <- nonzero_models

## Capitalize first letter of each model name
model_labels <- sub("^([a-z])", "\\U\\1", nonzero_models, perl = TRUE)
names(model_labels) <- nonzero_models  # Map capitalized labels to original names

# Visualization of Posterior Distributions -------------------------------

## Create a bar plot showing posterior probabilities of models
post_dist_plot <- ggplot(data = filter(.data = post_data, model != ""), # Filter out empty model names
                         mapping = aes(x = model, y = prob)) +
  geom_bar(stat = "identity") +  # Create bar plot
  theme_bw() +                   # Apply a clean theme
  labs(x = "Prediction model",
       y = "Posterior probability") +
  scale_x_discrete(labels = model_labels) +  # Apply capitalized labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Posterior Distribution Plot ---------------------------------------

## Save the plot as a high-resolution PDF file
ggsave(plot = post_dist_plot,
       filename = "code_and_output/post_dist_plot.pdf",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600,
       bg = "white")