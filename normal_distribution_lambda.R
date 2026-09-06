# =============================================================================
# Script: normal_distribution_lambda.R
# Purpose: Generate normal distribution plots for finite rate of increase (lambda)
#          across three treatment groups (Control, 1.5 ppm, 8.5 ppm)
# Author: [Your Name]
# Date: [Current Date]
# Corresponding to: Figure 6b in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)
library(reshape2)
library(gridExtra)

# =============================================================================
# 2. Define bootstrap statistics for lambda
#    SD estimated from percentile range: (97.5% - 2.5%) / 4
# =============================================================================
mean_control <- 1.2431017393867
sd_control <- (1.25763397649001 - 1.2265843268984) / 4

mean_ppm_1_5 <- 1.20211770078316
sd_ppm_1_5 <- (1.21769429667999 - 1.18429036639592) / 4

mean_ppm_8_5 <- 1.2479108465136
sd_ppm_8_5 <- (1.262691199877 - 1.23107835007347) / 4

# =============================================================================
# 3. Generate x-axis ranges for each group (mean ± 4*SD)
# =============================================================================
x_control <- seq(mean_control - 4*sd_control, mean_control + 4*sd_control, length.out = 1000)
x_ppm_1_5 <- seq(mean_ppm_1_5 - 4*sd_ppm_1_5, mean_ppm_1_5 + 4*sd_ppm_1_5, length.out = 1000)
x_ppm_8_5 <- seq(mean_ppm_8_5 - 4*sd_ppm_8_5, mean_ppm_8_5 + 4*sd_ppm_8_5, length.out = 1000)

# =============================================================================
# 4. Generate normal distribution curves
# =============================================================================
curve_control <- data.frame(x = x_control, Density = dnorm(x_control, mean_control, sd_control))
curve_ppm_1_5 <- data.frame(x = x_ppm_1_5, Density = dnorm(x_ppm_1_5, mean_ppm_1_5, sd_ppm_1_5))
curve_ppm_8_5 <- data.frame(x = x_ppm_8_5, Density = dnorm(x_ppm_8_5, mean_ppm_8_5, sd_ppm_8_5))

# =============================================================================
# 5. Percentile data (2.5%, 50%, 97.5%, Original)
# =============================================================================
percentiles <- data.frame(
  Group = rep(c("Control", "PPM_1_5", "PPM_8_5"), each = 4),
  Label = rep(c("2.5%", "50%", "97.5%", "Original"), 3),
  Rate = c(
    1.2265843268984, 1.24345203752847, 1.25763397649001, 1.24353529798188,
    1.18429036639592, 1.20251009076877, 1.21769429667999, 1.20261002805049,
    1.23107835007347, 1.24831381951824, 1.262691199877, 1.24837216951958
  )
)

# =============================================================================
# 6. Theme settings
# =============================================================================
label_theme <- theme(
  plot.title = element_text(size = 18),
  axis.title = element_text(size = 16),
  axis.text = element_text(size = 14),
  legend.title = element_text(size = 15),
  legend.text = element_text(size = 13)
)

# =============================================================================
# 7. Create individual plots
# =============================================================================

# 7.1 Control group
p1 <- ggplot(curve_control, aes(x = x, y = Density)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = percentiles$Rate[percentiles$Group == "Control"], linetype = "dashed") +
  geom_point(data = subset(percentiles, Group == "Control"),
             aes(x = Rate, y = dnorm(Rate, mean_control, sd_control), shape = Label), size = 3) +
  labs(title = expression("Control (0 ppm) – Finite Rate (" * italic(lambda) * ")"),
       x = expression(italic(lambda)), y = "Probability Density") +
  theme_minimal() + label_theme

# 7.2 1.5 ppm group
p2 <- ggplot(curve_ppm_1_5, aes(x = x, y = Density)) +
  geom_line(color = "red") +
  geom_vline(xintercept = percentiles$Rate[percentiles$Group == "PPM_1_5"], linetype = "dashed") +
  geom_point(data = subset(percentiles, Group == "PPM_1_5"),
             aes(x = Rate, y = dnorm(Rate, mean_ppm_1_5, sd_ppm_1_5), shape = Label), size = 3) +
  labs(title = expression("1.5 ppm – Finite Rate (" * italic(lambda) * ")"),
       x = expression(italic(lambda)), y = "Probability Density") +
  theme_minimal() + label_theme

# 7.3 8.5 ppm group
p3 <- ggplot(curve_ppm_8_5, aes(x = x, y = Density)) +
  geom_line(color = "green") +
  geom_vline(xintercept = percentiles$Rate[percentiles$Group == "PPM_8_5"], linetype = "dashed") +
  geom_point(data = subset(percentiles, Group == "PPM_8_5"),
             aes(x = Rate, y = dnorm(Rate, mean_ppm_8_5, sd_ppm_8_5), shape = Label), size = 3) +
  labs(title = expression("8.5 ppm – Finite Rate (" * italic(lambda) * ")"),
       x = expression(italic(lambda)), y = "Probability Density") +
  theme_minimal() + label_theme

# =============================================================================
# 8. Comparison plot with common x-axis
# =============================================================================
x_all <- seq(min(percentiles$Rate) - 0.01, max(percentiles$Rate) + 0.01, length.out = 1000)

curve_all <- data.frame(
  x = x_all,
  Control = dnorm(x_all, mean_control, sd_control),
  PPM_1_5 = dnorm(x_all, mean_ppm_1_5, sd_ppm_1_5),
  PPM_8_5 = dnorm(x_all, mean_ppm_8_5, sd_ppm_8_5)
)
curve_long <- melt(curve_all, id.vars = "x", variable.name = "Group", value.name = "Density")

p4 <- ggplot(curve_long, aes(x = x, y = Density, color = Group)) +
  geom_line(size = 1) +
  geom_vline(data = percentiles, aes(xintercept = Rate, color = Group), linetype = "dashed", alpha = 0.5) +
  geom_point(data = percentiles,
             aes(x = Rate,
                 y = dnorm(Rate,
                           mean = ifelse(Group == "Control", mean_control,
                                         ifelse(Group == "PPM_1_5", mean_ppm_1_5, mean_ppm_8_5)),
                           sd = ifelse(Group == "Control", sd_control,
                                       ifelse(Group == "PPM_1_5", sd_ppm_1_5, sd_ppm_8_5))),
                 color = Group,
                 shape = Label),
             size = 3) +
  scale_color_manual(values = c("Control" = "blue", "PPM_1_5" = "red", "PPM_8_5" = "green")) +
  scale_shape_manual(values = c("2.5%" = 15, "50%" = 16, "97.5%" = 17, "Original" = 18)) +
  labs(title = expression("Comparison of Finite Rate (" * italic(lambda) * ")"),
       x = expression(italic(lambda)), y = "Probability Density") +
  theme_minimal() + label_theme

# =============================================================================
# 9. Combine and save
# =============================================================================
combined_plot <- arrangeGrob(p1, p2, p3, p4, ncol = 2)

# Display in R environment
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save as JPEG with high quality
ggsave(filename = "finite_rate_panels.jpeg",
       plot = combined_plot,
       width = 12, height = 8, dpi = 300, units = "in")

cat("Figure saved as: finite_rate_panels.jpeg\n")