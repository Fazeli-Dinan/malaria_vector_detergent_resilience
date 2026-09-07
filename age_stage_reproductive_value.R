# =============================================================================
# Script: age_stage_reproductive_value.R
# Purpose: Plot age-stage specific reproductive value (vxj) for An. stephensi
#          across three treatment groups (Control, 1.5 ppm, 8.5 ppm)
# Author: [Your Name]
# Date: 2026-09-07
# Corresponding to: Figure [X] in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggh4x)

# =============================================================================
# 2. Define reproductive value data for each treatment
# =============================================================================

# 2.1 Control group
control_rep <- data.frame(
  Age = 0:37,
  Preadult = c(1.24, 1.90, 2.42, 3.07, 3.85, 4.82, 6.16, 8.09, 10.56, 13.40,
               18.58, 26.45, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0),
  Female = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             29.45, 36.63, 45.55, 59.85, 74.42, 69.78, 61.44, 59.89, 84.41, 72.63,
             68.64, 28.67, 38.33, 29.94, 37.23, 19.15, 23.82, 20.20, 17.05, 6.96,
             11.75, 9.32, 8.11, 1.11, 1.94, 4.02, 7.5, 0),
  Male = rep(0, 38)
)
control_rep$treatment <- "Control"

# 2.2 1.5 ppm group
ppm1.5_rep <- data.frame(
  Age = 0:40,
  Preadult = c(1.20, 1.69, 2.07, 2.51, 3.04, 3.75, 4.62, 5.71, 7.59, 9.59,
               12.13, 14.35, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Female = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             24.81, 29.84, 35.89, 43.16, 53.06, 58.86, 55.07, 29.77, 36.63, 37.26,
             38.19, 23.59, 28.37, 36.90, 31.39, 14.92, 17.94, 22.85, 22.39, 13.13,
             17.89, 20.22, 23.41, 1.44, 1.73, 2.27, 3, 0, 0, 0, 0),
  Male = rep(0, 41)
)
ppm1.5_rep$treatment <- "1.5 ppm"

# 2.3 8.5 ppm group
ppm8.5_rep <- data.frame(
  Age = 0:40,
  Preadult = c(1.25, 1.77, 2.23, 2.80, 3.58, 4.51, 5.76, 7.63, 10.23, 14.07,
               17.00, 21.71, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Female = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             31.88, 39.79, 49.68, 62.02, 77.42, 71.86, 55.55, 43.07, 55.92, 47.49,
             38.58, 33.88, 42.29, 37.68, 22.70, 16.93, 21.13, 14.31, 15.00, 15.95,
             23.37, 23.04, 13.97, 9.31, 11.63, 9.44, 3.93, 0, 0, 0, 0),
  Male = rep(0, 41)
)
ppm8.5_rep$treatment <- "8.5 ppm"

# =============================================================================
# 3. Combine all data and convert to long format
# =============================================================================
all_rep <- bind_rows(control_rep, ppm1.5_rep, ppm8.5_rep)
all_rep$treatment <- factor(all_rep$treatment, levels = c("Control", "1.5 ppm", "8.5 ppm"))

long_rep <- all_rep %>%
  pivot_longer(cols = Preadult:Male, names_to = "Stage", values_to = "ReproductiveValue")

# =============================================================================
# 4. Create the plot
# =============================================================================
p_rep <- ggplot(long_rep, aes(x = Age, y = ReproductiveValue, color = Stage)) +
  geom_line(size = 0.7) +
  geom_point(size = 2.5) +
  facet_wrap(~treatment, nrow = 1, scales = "fixed") +
  scale_color_manual(values = c(
    "Preadult" = "black",
    "Female" = "red",
    "Male" = "blue"
  )) +
  scale_x_continuous(
    breaks = seq(0, 40, by = 10),
    minor_breaks = seq(0, 40, by = 2),
    expand = c(0, 0),
    guide = guide_axis_minor()
  ) +
  scale_y_continuous(
    limits = c(0, 90),
    breaks = seq(0, 85, by = 10),
    minor_breaks = seq(0, 85, by = 2),
    expand = c(0, 0),
    guide = guide_axis_minor()
  ) +
  labs(
    title = "Age-stage Reproductive Value under Different Treatments",
    x = "Age (days)",
    y = "Reproductive Value",
    color = "Stage"
  ) +
  coord_fixed(ratio = 30 / 60) +
  theme_bw(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#e6f2ff", color = NA),
    strip.text = element_text(face = "bold", size = 13),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# =============================================================================
# 5. Display the plot
# =============================================================================
print(p_rep)

# =============================================================================
# 6. Save the plot
# =============================================================================
output_dir <- "./outputs/figures/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(filename = paste0(output_dir, "age_stage_reproductive_value.jpeg"),
       plot = p_rep, width = 12, height = 6, dpi = 300, units = "in", device = "jpeg")

cat("\nFigure saved as:", paste0(output_dir, "age_stage_reproductive_value.jpeg"), "\n")

# =============================================================================
# End of script
# =============================================================================