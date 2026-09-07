# =============================================================================
# Script: age_stage_life_expectancy.R
# Purpose: Plot age-stage specific life expectancy (exj) for An. stephensi
#          across three treatment groups (Control, 1.5 ppm, 8.5 ppm)
# Author: [Your Name]
# Date: 2026-09-07
# Corresponding to: Figure 3 in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggh4x)

# =============================================================================
# 2. Define life expectancy data for each treatment
# =============================================================================

# 2.1 Control group
control_life <- data.frame(
  Age = 0:37,
  Preadult = c(17.84, 20.70, 20.20, 19.53, 18.69, 17.84, 17.29, 17.22, 17.02, 16.34,
               15.50, 14.71, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0),
  Female = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             16.36, 15.36, 14.36, 14.11, 13.11, 12.11, 11.11, 10.51, 10.78, 9.78,
             9.19, 8.19, 7.73, 6.73, 6.54, 6.06, 5.06, 4.33, 4, 3.95,
             4, 3.23, 2.42, 2.43, 2, 1.67, 1, 0),
  Male = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           14.02, 13.02, 12.02, 11.02, 10.02, 9.46, 9.13, 8.35, 8.5, 7.5,
           6.5, 5.5, 4.65, 4.04, 3.54, 3.21, 3.23, 4.14, 3.14, 2.5,
           1.5, 1, 0, 0, 0, 0, 0, 0)
)
control_life$treatment <- "Control"

# 2.2 1.5 ppm group
ppm1.5_life <- data.frame(
  Age = 0:40,
  Preadult = c(20.22, 22.52, 21.87, 21.03, 20.19, 19.67, 19.14, 18.62, 19.48, 19.41,
               18.35, 17.37, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Female = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             17.07, 16.07, 15.07, 14.07, 13.36, 12.36, 11.61, 10.61, 9.84, 8.84,
             9.11, 9.68, 8.68, 8.81, 8.79, 10.39, 9.39, 8.88, 7.88, 6.88,
             6.67, 6.07, 5.46, 4.83, 3.83, 3.09, 2.3, 1.86, 2, 1, 0),
  Male = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           19.58, 18.58, 17.58, 16.58, 15.58, 14.58, 13.58, 12.83, 12.3, 11.3,
           10.3, 9.69, 9.07, 9.27, 8.95, 8.65, 8.13, 7.13, 7.54, 7.39,
           6.39, 5.39, 4.39, 3.71, 3, 2.92, 2.78, 2, 1, 0, 0)
)
ppm1.5_life$treatment <- "1.5 ppm"

# 2.3 8.5 ppm group
ppm8.5_life <- data.frame(
  Age = 0:40,
  Preadult = c(20.14, 21.75, 20.91, 20.06, 19.51, 18.66, 18.09, 18.12, 18.39, 19.16,
               18.12, 17.15, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  Female = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             19.17, 18.17, 17.17, 16.17, 15.17, 14.43, 13.43, 12.67, 12.14, 11.60,
             11.57, 11.63, 10.63, 10.69, 9.97, 8.97, 7.97, 7.18, 6.36, 6.56,
             6.52, 6.05, 5.3, 5.38, 4.38, 3.38, 2.71, 1.85, 1.57, 1, 0),
  Male = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
           16.93, 15.93, 14.93, 13.93, 12.93, 11.93, 10.93, 10.16, 9.38, 8.38,
           7.38, 6.38, 7.06, 10.21, 9.21, 8.21, 7.61, 7, 6, 5.67,
           5, 4.67, 4, 4.13, 3.13, 2.43, 2, 1, 0, 0, 0)
)
ppm8.5_life$treatment <- "8.5 ppm"

# =============================================================================
# 3. Combine all data and convert to long format
# =============================================================================
all_life <- bind_rows(control_life, ppm1.5_life, ppm8.5_life)
all_life$treatment <- factor(all_life$treatment, levels = c("Control", "1.5 ppm", "8.5 ppm"))

long_life <- all_life %>%
  pivot_longer(cols = Preadult:Male, names_to = "Stage", values_to = "LifeExpectancy")

# =============================================================================
# 4. Create the plot
# =============================================================================
p_life <- ggplot(long_life, aes(x = Age, y = LifeExpectancy, color = Stage)) +
  geom_line(size = 0.7) +
  geom_point(size = 2.5) +
  facet_wrap(~treatment, nrow = 1, scales = "fixed") +
  scale_color_manual(values = c("Preadult" = "black", "Female" = "red", "Male" = "blue")) +
  scale_x_continuous(
    breaks = seq(0, 40, by = 10),
    minor_breaks = seq(0, 40, by = 2),
    expand = c(0, 0),
    guide = guide_axis_minor()
  ) +
  scale_y_continuous(
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    minor_breaks = seq(0, 25, by = 1),
    expand = c(0, 0),
    guide = guide_axis_minor()
  ) +
  labs(
    title = "Age-stage Life Expectancy under Different Treatments",
    x = "Age (days)",
    y = "Life Expectancy",
    color = "Stage"
  ) +
  coord_fixed(ratio = 60 / 35) +
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
print(p_life)

# =============================================================================
# 6. Save the plot
# =============================================================================
output_dir <- "./outputs/figures/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(filename = paste0(output_dir, "age_stage_life_expectancy.jpeg"),
       plot = p_life, width = 12, height = 6, dpi = 300, units = "in", device = "jpeg")

cat("\nFigure saved as:", paste0(output_dir, "age_stage_life_expectancy.jpeg"), "\n")

# =============================================================================
# End of script
# =============================================================================