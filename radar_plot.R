# =============================================================================
# Script: radar_plot.R
# Purpose: Generate a radar (polar) plot of cumulative percent profiles 
#          for life-table parameters across three treatment groups
# Author: [Your Name]
# Date: 2026-09-07
# Corresponding to: Figure [X] in the manuscript
# =============================================================================

# =============================================================================
# 1. Load required libraries
# =============================================================================
library(plotly)
library(webshot)
library(htmlwidgets)

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
# 3. Standardize data using Z-score (manual function)
# =============================================================================
zscore <- function(x) (x - mean(x)) / sd(x)
z_data <- as.data.frame(lapply(raw_data[, -1], zscore))
z_data$Pollution <- raw_data$Pollution

# =============================================================================
# 4. Convert Z-scores to cumulative percentages (pnorm)
# =============================================================================
z_to_percent <- function(z) {
  pnorm(z) * 100
}
percent_data <- as.data.frame(lapply(z_data[, 1:6], z_to_percent))
percent_data$Pollution <- z_data$Pollution
parameters <- colnames(percent_data)[1:6]

# =============================================================================
# 5. Create radar plot with plotly
# =============================================================================
p <- plot_ly(
  type = 'scatterpolar',
  fill = 'toself',
  mode = "lines+markers"
) %>%
  add_trace(
    r = as.numeric(percent_data[percent_data$Pollution == "Control", 1:6]),
    theta = parameters,
    name = "Control",
    mode = "lines+markers",
    line = list(color = "blue", width = 0),
    marker = list(color = "blue", size = 20, symbol = "square")
  ) %>%
  add_trace(
    r = as.numeric(percent_data[percent_data$Pollution == "1.5 ppm", 1:6]),
    theta = parameters,
    name = "1.5 ppm",
    mode = "lines+markers",
    line = list(color = "green", width = 0),
    marker = list(color = "green", size = 20, symbol = "square")
  ) %>%
  add_trace(
    r = as.numeric(percent_data[percent_data$Pollution == "8.5 ppm", 1:6]),
    theta = parameters,
    name = "8.5 ppm",
    mode = "lines+markers",
    line = list(color = "red", width = 0),
    marker = list(color = "red", size = 20, symbol = "square")
  ) %>%
  layout(
    title = list(
      text = "Cumulative Percent Profile of Life Table Parameters across Detergent Levels",
      font = list(size = 40, family = "Arial", color = "black")
    ),
    margin = list(t = 180),
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(0, 100),
        tickmode = "array",
        tickvals = c(0, 25, 50, 75, 100),
        ticktext = c("0", "25", "50", "75", "100"),
        showline = TRUE,
        showticklabels = TRUE,
        tickangle = 0,
        tickfont = list(size = 40, family = "Arial"),
        gridcolor = "lightgray",
        gridwidth = 2
      ),
      angularaxis = list(
        showline = TRUE,
        linecolor = "black",
        linewidth = 4.5,
        tickfont = list(size = 60, family = "Arial")
      )
    ),
    legend = list(
      font = list(size = 50, color = "black"),
      orientation = "v",
      x = 0.3,
      y = -0.2,
      bgcolor = "white",
      bordercolor = "white",
      borderwidth = 2
    )
  )

# =============================================================================
# 6. Save as temporary HTML and convert to JPEG
# =============================================================================
output_dir <- "./outputs/figures/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save as HTML
saveWidget(p, paste0(output_dir, "radar_plot_temp.html"), selfcontained = TRUE)

# Convert HTML to JPEG with high quality
webshot(paste0(output_dir, "radar_plot_temp.html"),
        paste0(output_dir, "radar_plot.jpeg"),
        vwidth = 1600, vheight = 1600, zoom = 2)

# =============================================================================
# 7. Display the cumulative percentage data
# =============================================================================
cat("\nCumulative Percent Data:\n")
print(percent_data)

# =============================================================================
# 8. Display the plot in R environment
# =============================================================================
print(p)

# =============================================================================
# End of script
# =============================================================================