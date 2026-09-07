# =============================================================================
# Script: malaria_transmission_potential.R
# Purpose: Compare mosquito longevity with Plasmodium development periods
#          across three water treatment groups using faceted bar-plot
# Author: [Your Name]
# Date: 2026-09-07
# Corresponding to: Figure [X] in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)

# =============================================================================
# 2. Define data: Female mosquito longevity across treatments
# =============================================================================
longevity_data <- data.frame(
  WaterType = factor(c("Control", "1.5 ppm", "8.5 ppm"),
                     levels = c("Control", "1.5 ppm", "8.5 ppm")),
  Longevity = c(26.35, 27.06, 29.16)
)

# =============================================================================
# 3. Define data: Plasmodium development periods (min-max days)
# =============================================================================
malaria_data <- data.frame(
  Parasite = c("P. falciparum", "P. vivax", "P. malariae", "P. ovale", "P. knowlesi"),
  MinDays = c(10, 8, 15, 9, 9),
  MaxDays = c(12, 10, 20, 10, 12),
  FillColor = c("mistyrose", "lightblue", "lightgreen", "plum", "moccasin"),
  BorderColor = c("red", "blue", "darkgreen", "purple", "orange")
)

# =============================================================================
# 4. Format parasite names for italic display in facet labels
# =============================================================================
malaria_data$Parasite <- paste0("italic('", malaria_data$Parasite, "')")

# =============================================================================
# 5. Define custom color palettes
# =============================================================================
water_colors <- c("Control" = "darkgreen", "1.5 ppm" = "orange", "8.5 ppm" = "skyblue")
parasite_fill <- setNames(malaria_data$FillColor, malaria_data$Parasite)
parasite_border <- setNames(malaria_data$BorderColor, malaria_data$Parasite)

# =============================================================================
# 6. Expand malaria data for each water type (to overlay rectangles on all facets)
# =============================================================================
expanded_data <- do.call(rbind, lapply(levels(longevity_data$WaterType), function(wt) {
  malaria_data$WaterType <- wt
  malaria_data
}))

# =============================================================================
# 7. Create faceted plot
# =============================================================================
p <- ggplot() +
  # Bar plot for mosquito longevity
  geom_col(data = longevity_data,
           aes(x = WaterType, y = Longevity, fill = WaterType),
           width = 0.6, alpha = 0.8) +
  # Rectangles for parasite development ranges
  geom_rect(data = expanded_data,
            aes(xmin = 0.5, xmax = 3.5, ymin = MinDays, ymax = MaxDays,
                fill = Parasite, color = Parasite),
            inherit.aes = FALSE, alpha = 0.3, linewidth = 1) +
  # Custom color scales
  scale_fill_manual(values = c(water_colors, parasite_fill)) +
  scale_color_manual(values = parasite_border) +
  # Facet by parasite species
  facet_wrap(~Parasite, labeller = label_parsed) +
  # Labels and titles
  labs(title = "Mosquito Longevity vs Parasite Development Range by Species",
       x = "Water Condition",
       y = "Days",
       fill = "Water Type / Parasite",
       color = "Parasite Border") +
  # Theme settings
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 13, color = "black", face = "plain")
  )

# =============================================================================
# 8. Display the plot
# =============================================================================
print(p)

# =============================================================================
# 9. Save the plot as JPEG
# =============================================================================
output_dir <- "./outputs/figures/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(filename = paste0(output_dir, "malaria_transmission_potential.jpeg"),
       plot = p, width = 10, height = 6, dpi = 300)

cat("\nFigure saved as:", paste0(output_dir, "malaria_transmission_potential.jpeg"), "\n")

# =============================================================================
# End of script
# =============================================================================