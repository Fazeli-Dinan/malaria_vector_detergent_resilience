# =============================================================================
# Script: malaria_sigmoid_transmission.R
# Purpose: Model malaria transmission probability as a sigmoid function of 
#          mosquito longevity for five Plasmodium species
# Author: [Your Name]
# Date: 2026-09-07
# Corresponding to: Figure [X] in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)

# =============================================================================
# 2. Sigmoid function for transmission probability
#    Returns probability between 0 and 1 based on mosquito longevity
# =============================================================================
sigmoid <- function(x, min_day, max_day) {
  center <- (min_day + max_day) / 2
  steepness <- 1.5 / (max_day - min_day)
  1 / (1 + exp(-steepness * (x - center)))
}

# =============================================================================
# 3. Define Plasmodium species and their development periods
# =============================================================================
parasites <- data.frame(
  Parasite = c("P. falciparum", "P. vivax", "P. malariae", "P. ovale", "P. knowlesi"),
  MinDay = c(10, 8, 15, 9, 9),
  MaxDay = c(12, 10, 20, 10, 12),
  Color = c("red", "blue", "darkgreen", "purple", "orange")
)

# =============================================================================
# 4. Generate probability data for each parasite across longevity range
# =============================================================================
x <- seq(0, 35, length.out = 500)

prob_data <- do.call(rbind, lapply(1:nrow(parasites), function(i) {
  data.frame(
    Longevity = x,
    Probability = sigmoid(x, parasites$MinDay[i], parasites$MaxDay[i]),
    Parasite = parasites$Parasite[i],
    Color = parasites$Color[i]
  )
}))

# =============================================================================
# 5. Define mosquito longevity under different water conditions
# =============================================================================
longevity_lines <- data.frame(
  Condition = c("Control", "1.5 ppm", "8.5 ppm"),
  Days = c(26.35, 27.06, 29.16),
  LineColor = c("blue", "black", "red"),
  LineSize = c(2.2, 1.8, 1.8)
)

# =============================================================================
# 6. Create the plot
# =============================================================================
g <- ggplot(prob_data, aes(x = Longevity, y = Probability, color = Parasite)) +
  # Sigmoid curves for each parasite
  geom_line(size = 2.1) +
  
  # Vertical lines for mosquito longevity (with custom colors)
  geom_vline(data = longevity_lines,
             aes(xintercept = Days),
             linetype = "dashed",
             color = longevity_lines$LineColor,
             size = longevity_lines$LineSize,
             show.legend = FALSE) +
  
  # Labels for vertical lines (with custom colors)
  geom_text(data = longevity_lines,
            aes(x = Days, y = 0.05, label = Condition),
            angle = 90, vjust = -0.5, hjust = 0,
            size = 6,
            color = longevity_lines$LineColor,
            show.legend = FALSE) +
  
  # Custom colors and italic labels for parasites
  scale_color_manual(
    values = setNames(parasites$Color, parasites$Parasite),
    labels = lapply(parasites$Parasite, function(name) bquote(italic(.(name))))
  ) +
  
  # Labels and titles
  labs(title = "Probability of Malaria Transmission Based on Mosquito Longevity",
       x = "Mosquito Longevity (days)",
       y = "Transmission Probability",
       color = "Parasite Type") +
  
  # Theme settings
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 20, color = "black"),
    axis.text = element_text(size = 20, color = "black"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# =============================================================================
# 7. Display the plot
# =============================================================================
print(g)

# =============================================================================
# 8. Save the plot
# =============================================================================
output_dir <- "./outputs/figures/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(filename = paste0(output_dir, "malaria_sigmoid_transmission.png"),
       plot = g, width = 10, height = 6, dpi = 300)

cat("\nFigure saved as:", paste0(output_dir, "malaria_sigmoid_transmission.png"), "\n")

# =============================================================================
# 9. Print summary of transmission probabilities at observed longevity values
# =============================================================================
cat("\nTransmission probabilities at observed mosquito longevity:\n")
for (i in 1:nrow(longevity_lines)) {
  cat("\n", longevity_lines$Condition[i], "(", longevity_lines$Days[i], "days):\n")
  for (j in 1:nrow(parasites)) {
    prob <- sigmoid(longevity_lines$Days[i], parasites$MinDay[j], parasites$MaxDay[j])
    cat("  ", parasites$Parasite[j], ":", round(prob, 3), "\n")
  }
}

# =============================================================================
# End of script
# =============================================================================