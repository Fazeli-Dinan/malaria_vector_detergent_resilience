# =============================================================================
# Script: normal_distribution_rm.R
# Purpose: Generate normal distribution plots for intrinsic rate of increase (rm)
#          across three treatment groups (Control, 1.5 ppm, 8.5 ppm)
# Author: [Your Name]
# Date: [Current Date]
# Corresponding to: Figure 6a in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(ggplot2)
library(reshape2)
library(gridExtra)

# =============================================================================
# 2. Define bootstrap statistics for rm
# =============================================================================
mean_control <- 0.217589241699638
sd_control <- 6.39220408189354E-03

mean_ppm_1_5 <- 0.184059410621524
sd_ppm_1_5 <- 7.12175793130301E-03

mean_ppm_8_5 <- 0.221449795462937
sd_ppm_8_5 <- 6.48824104121303E-03

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
  Group = rep(c("Control", "1.5 ppm", "8.5 ppm"), each = 4),
  Label = rep(c("2.5%", "50%", "97.5%", "Original"), 3),
  Rate = c(
    0.204233336448669, 0.217891412973404, 0.229232159256935, 0.217958369851112,
    0.169143748283386, 0.184411114454269, 0.196959149837494, 0.184494218230248,
    0.207890492677689, 0.221793696284294, 0.233245316147804, 0.221840438246727
  )
)

# Rename groups for consistency in comparison plot
percentiles$Group[percentiles$Group == "1.5 ppm"] <- "PPM_1_5"
percentiles$Group[percentiles$Group == "8.5 ppm"] <- "PPM_8_5"

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
  labs(title = "Control (0 ppm)", 
       x = expression("Intrinsic Rate of Increase (" * italic(r[m]) * ")"), 
       y = "Probability Density") +
  theme_minimal() + label_theme

# 7.2 1.5 ppm group
p2 <- ggplot(curve_ppm_1_5, aes(x = x, y = Density)) +
  geom_line(color = "red") +
  geom_vline(xintercept = percentiles$Rate[percentiles$Group == "PPM_1_5"], linetype = "dashed") +
  geom_point(data = subset(percentiles, Group == "PPM_1_5"),
             aes(x = Rate, y = dnorm(Rate, mean_ppm_1_5, sd_ppm_1_5), shape = Label), size = 3) +
  labs(title = "1.5 ppm", 
       x = expression("Intrinsic Rate of Increase (" * italic(r[m]) * ")"), 
       y = "Probability Density") +
  theme_minimal() + label_theme

# 7.3 8.5 ppm group
p3 <- ggplot(curve_ppm_8_5, aes(x = x, y = Density)) +
  geom_line(color = "green") +
  geom_vline(xintercept = percentiles$Rate[percentiles$Group == "PPM_8_5"], linetype = "dashed") +
  geom_point(data = subset(percentiles, Group == "PPM_8_5"),
             aes(x = Rate, y = dnorm(Rate, mean_ppm_8_5, sd_ppm_8_5), shape = Label), size = 3) +
  labs(title = "8.5 ppm", 
       x = expression("Intrinsic Rate of Increase (" * italic(r[m]) * ")"), 
       y = "Probability Density") +
  theme_minimal() + label_theme

# =============================================================================
# 8. Comparison plot with common x-axis
# =============================================================================
x_all <- seq(min(mean_ppm_1_5 - 4*sd_ppm_1_5, mean_control - 4*sd_control, mean_ppm_8_5 - 4*sd_ppm_8_5),
             max(mean_ppm_1_5 + 4*sd_ppm_1_5, mean_control + 4*sd_control, mean_ppm_8_5 + 4*sd_ppm_8_5),
             length.out = 1000)

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
  labs(title = "Comparison of All Groups", 
       x = expression("Intrinsic Rate of Increase (" * italic(r[m]) * ")"), 
       y = "Probability Density") +
  theme_minimal() + label_theme

# =============================================================================
# 9. Combine and save
# =============================================================================
combined_plot <- arrangeGrob(p1, p2, p3, p4, ncol = 2)

# Display in R environment
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save as JPEG with high quality
ggsave(filename = "intrinsic_rate_panels.jpeg",
       plot = combined_plot,
       width = 12, height = 8, dpi = 300, units = "in")

cat("Figure saved as: intrinsic_rate_panels.jpeg\n")