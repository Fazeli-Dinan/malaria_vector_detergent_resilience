# =============================================================================
# Script: composite_resilience_index.R
# Purpose: Calculate and visualize Composite Resilience Index (CRI) 
#          from life-table parameters across three treatment groups
# Author: [Your Name]
# Date: 2026-09-07
# Corresponding to: Figure [X] in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)
library(grid)

# =============================================================================
# 2. Life-table raw data (mean values from Table 1)
# =============================================================================
raw_data <- data.frame(
  Pollution = c("Control", "1.5 ppm", "8.5 ppm"),
  rm = c(0.21796, 0.18449, 0.22184),
  lambda = c(1.24354, 1.20261, 1.24837),
  R0 = c(63.19, 34.86, 61.76),
  T = c(19.02, 19.25, 18.59),
  GRR = c(151.64, 70.76, 132.35),
  Fecundity = c(169.27, 113.67, 171.56)
)

# =============================================================================
# 3. Standardize data using Z-score (scale function)
# =============================================================================
scaled_data <- as.data.frame(scale(raw_data[, 2:7]))  # Only numeric columns

# =============================================================================
# 4. Calculate Composite Resilience Index (mean of Z-scores)
# =============================================================================
raw_data$CompositeResilience_Z <- round(rowMeans(scaled_data), 2)

# =============================================================================
# 5. Prepare data for plotting
# =============================================================================
resilience_data <- data.frame(
  Detergent_ppm = c(0, 1.5, 8.5),
  CompositeResilience_Z = raw_data$CompositeResilience_Z
)

# =============================================================================
# 6. Create trajectory plot with arrow
# =============================================================================
p <- ggplot(resilience_data, aes(x = Detergent_ppm, y = CompositeResilience_Z)) +
  geom_path(
    arrow = arrow(type = "closed", length = unit(0.2, "inches")),
    color = "black", size = 1.0
  ) +
  geom_point(size = 5, color = "red") +
  geom_text(aes(label = round(CompositeResilience_Z, 2)), vjust = -1.5, size = 5) +
  scale_y_continuous(limits = c(-1.2, 1)) +
  labs(
    title = "Trajectory of Composite Resilience Index",
    x = "Detergent concentration (ppm)",
    y = "Composite Resilience Index (Z-score)"
  ) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1.2),
    axis.text = element_text(color = "black", size = 20),
    axis.title = element_text(color = "black", size = 20),
    plot.title = element_text(color = "black", size = 18, face = "bold", hjust = 0.5)
  )

# =============================================================================
# 7. Display the plot
# =============================================================================
print(p)

# =============================================================================
# 8. Save the plot as JPEG
# =============================================================================
output_dir <- "./outputs/figures/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(filename = paste0(output_dir, "composite_resilience_index.jpeg"),
       plot = p, width = 8, height = 6, dpi = 300)

# =============================================================================
# 9. Print the resilience values
# =============================================================================
cat("\nComposite Resilience Index (Z-score) values:\n")
print(raw_data[, c("Pollution", "CompositeResilience_Z")])

# =============================================================================
# End of script
# =============================================================================