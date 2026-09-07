# =============================================================================
# Script: life_table_parameters.R
# Purpose: Plot age-specific survival (lx), fecundity (mx), lx*mx, 
#          and cumulative lx*mx across three treatment groups
# Author: [Your Name]
# Date: 2026-09-07
# Corresponding to: Figure 1 in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)
library(dplyr)
library(ggh4x)

# =============================================================================
# 2. Function to calculate derived parameters
# =============================================================================
calc_params <- function(df) {
  df$lxmx <- df$lx * df$mx
  df$Cumu_lxmx <- cumsum(df$lxmx)
  df$Cumu_exp_r_lxmx <- cumsum(exp(-0.184 * (df$Age + 1)) * df$lxmx)
  df$curtailed_r <- df$lxmx * exp(-0.184 * (df$Age + 1))
  df$r_ratio <- df$curtailed_r / max(df$curtailed_r, na.rm = TRUE)
  return(df)
}

# =============================================================================
# 3. Define life table data for each treatment
# =============================================================================

# 3.1 Control group
control <- data.frame(
  Age = 0:40,
  lx = c(1, 0.813333, 0.793333, 0.78, 0.773333, 0.766667, 0.746667, 0.706667, 0.673333, 0.66,
         0.66, 0.66, 0.66, 0.64, 0.64, 0.626667, 0.606667, 0.586667, 0.513333, 0.513333,
         0.5, 0.5, 0.473333, 0.453333, 0.393333, 0.34, 0.3, 0.246667, 0.213333, 0.166667,
         0.133333, 0.106667, 0.08, 0.046667, 0.033333, 0.02, 0.013333, 0, 0, 0, 0),
  mx = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 10.104167, 11.489362, 8.791209, 0, 15.194805, 11.623377,
         26.133333, 0, 8.028169, 2.205882, 13.728814, 0, 6.111111, 7.108108, 10, 0,
         3.35, 2.6875, 7.583333, 0, 0, 0, 7.5, 0, 0, 0, 0),
  Treatment = "Control"
)

# 3.2 1.5 ppm group
ppm_1_5 <- data.frame(
  Age = 0:40,
  lx = c(1, 0.853333, 0.84, 0.833333, 0.826667, 0.806667, 0.786667, 0.766667, 0.693333, 0.66,
         0.66, 0.66, 0.66, 0.66, 0.653333, 0.653333, 0.646667, 0.64, 0.62, 0.62,
         0.58, 0.526667, 0.513333, 0.446667, 0.406667, 0.346667, 0.333333, 0.326667, 0.286667, 0.266667,
         0.253333, 0.246667, 0.24, 0.22, 0.206667, 0.16, 0.126667, 0.1, 0.073333, 0.02, 0),
  mx = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 1.887755, 6.469388, 13.752577, 0, 2.612903, 4.591398,
         9.252874, 0, 0.662338, 5.522388, 8.688525, 0, 0, 1.469388, 4.534884, 0,
         0.868421, 0.810811, 8.055556, 0, 0, 0, 1.578947, 0, 0, 0, 0),
  Treatment = "1.5 ppm"
)

# 3.3 8.5 ppm group
ppm_8_5 <- data.frame(
  Age = 0:40,
  lx = c(1, 0.88, 0.873333, 0.866667, 0.846667, 0.84, 0.82, 0.773333, 0.72, 0.653333,
         0.653333, 0.653333, 0.653333, 0.653333, 0.653333, 0.646667, 0.646667, 0.633333, 0.613333, 0.6,
         0.573333, 0.546667, 0.48, 0.366667, 0.36, 0.36, 0.353333, 0.34, 0.333333, 0.28,
         0.246667, 0.22, 0.206667, 0.16, 0.16, 0.153333, 0.126667, 0.12, 0.046667, 0.026667, 0),
  mx = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 11.530612, 14.948454, 11.85567, 0, 10.543478, 10.222222,
         7.116279, 0, 8.402778, 13.090909, 5.925926, 0, 6.603774, 1.764706, 3.000000, 0,
         4.054054, 7.878788, 5.16129, 0, 2.708333, 4.652174, 2.894737, 0, 0, 0, 0),
  Treatment = "8.5 ppm"
)

# =============================================================================
# 4. Calculate derived parameters
# =============================================================================
control <- calc_params(control)
ppm_1_5 <- calc_params(ppm_1_5)
ppm_8_5 <- calc_params(ppm_8_5)

# =============================================================================
# 5. Combine data and set factor levels
# =============================================================================
combined <- bind_rows(control, ppm_1_5, ppm_8_5)
combined$Treatment <- factor(combined$Treatment, levels = c("Control", "1.5 ppm", "8.5 ppm"))

# =============================================================================
# 6. Calculate scaling factor for secondary axis
# =============================================================================
scale_factor <- max(combined$lxmx, na.rm = TRUE) / max(combined$Cumu_lxmx, na.rm = TRUE)
combined <- combined %>%
  mutate(Cumu_scaled = Cumu_lxmx * scale_factor)

# =============================================================================
# 7. Create the plot
# =============================================================================
p <- ggplot(combined, aes(x = Age)) +
  # Lines for life table parameters
  geom_line(aes(y = lx, color = "Survival (lx)"), size = 0.8) +
  geom_line(aes(y = mx, color = "Fecundity (mx)"), size = 0.8) +
  geom_line(aes(y = lxmx, color = "lx * mx"), size = 0.8) +
  geom_line(aes(y = Cumu_scaled, color = "Cumulative lx*mx"), size = 0.8, linetype = "dashed") +
  
  # Facet by treatment
  facet_wrap(~Treatment, nrow = 1) +
  
  # X-axis settings
  scale_x_continuous(
    breaks = seq(0, 40, by = 10),
    minor_breaks = seq(0, 40, by = 2),
    guide = ggh4x::guide_axis_minor()
  ) +
  
  # Y-axis settings with secondary axis
  scale_y_continuous(
    limits = c(0, 30),
    breaks = seq(0, 30, by = 5),
    minor_breaks = seq(0, 30, by = 1),
    guide = ggh4x::guide_axis_minor(),
    name = expression(italic(l)[x] * ", " * italic(m)[x] * ", " * italic(l)[x] * italic(m)[x]),
    sec.axis = sec_axis(
      trans = ~ . / scale_factor,
      breaks = c(0, 50, 100),
      labels = c(0, 50, 100),
      name = expression("Cumulative " * italic(l)[x] * italic(m)[x])
    )
  ) +
  
  # Color settings with custom labels
  scale_color_manual(
    values = c(
      "Survival (lx)" = "blue",
      "Fecundity (mx)" = "red",
      "lx * mx" = "purple",
      "Cumulative lx*mx" = "darkgreen"
    ),
    labels = c(
      expression("Survival (" * italic(l)[x] * ")"),
      expression("Fecundity (" * italic(m)[x] * ")"),
      expression(italic(l)[x] * " * " * italic(m)[x]),
      expression("Cumulative " * italic(l)[x] * "*" * italic(m)[x])
    ),
    breaks = c("Survival (lx)", "Fecundity (mx)", "lx * mx", "Cumulative lx*mx")
  ) +
  
  # Labels and titles
  labs(
    title = "Life Table Parameters across Treatment Groups",
    x = "Age (days)",
    color = "Parameter"
  ) +
  
  # Theme settings
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top",
    legend.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(color = "black")
  ) +
  coord_fixed(ratio = 1)

# =============================================================================
# 8. Display the plot
# =============================================================================
print(p)

# =============================================================================
# 9. Save the plot
# =============================================================================
output_dir <- "./outputs/figures/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(filename = paste0(output_dir, "life_table_parameters.jpeg"),
       plot = p, width = 12, height = 6, dpi = 300, units = "in", device = "jpeg")

cat("\nFigure saved as:", paste0(output_dir, "life_table_parameters.jpeg"), "\n")

# =============================================================================
# End of script
# =============================================================================