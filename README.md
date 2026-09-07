# Code for: Demographic Resilience of Anopheles stephensi to Detergent Pollution

This repository contains the R scripts used for generating normal distribution plots of bootstrap-derived demographic parameters in the manuscript:

**"Demographic Resilience of *Anopheles stephensi* to Detergent Pollution: Life-Table Responses and Implications for Urban Malaria Transmission"**

---

##  Repository Structure

```
.
├── README.md                           # This file
├── LICENSE                             # MIT License
└── scripts/
    ├── normal_distribution_rm.R
    ├── normal_distribution_lambda.R
    ├── normal_distribution_R0.R
    ├── composite_resilience_index.R
    ├── radar_plot.R
    ├── malaria_transmission_potential.R
    ├── malaria_sigmoid_transmission.R
    ├── age_stage_life_expectancy.R
    ├── life_table_parameters.R
    ├── age_stage_survival.R
    └── age_stage_reproductive_value.R       # Age-stage reproductive value (vxj)


---

##  Requirements

- **R** (version 4.0 or higher)
- Required R packages:
  - `ggplot2` (for plotting)
  - `reshape2` (for data manipulation)
  - `gridExtra` (for arranging multiple plots)

Install the required packages using:

```r
install.packages(c("ggplot2", "reshape2", "gridExtra"))
```

---

##  How to Run

1. Clone this repository:
   ```bash
   git clone https://github.com/Fazeli-Dinan/malaria_vector_detergent_resilience.git
   ```
2. Open R or RStudio and set the working directory to the repository root.
3. Navigate to the `scripts/` folder and run any of the scripts:
   ```r
   source("./scripts/normal_distribution_rm.R")
   ```
   Or run all three scripts sequentially to generate all figures.

---

##  Output

Each script generates a **JPEG image** containing four panels:

| Script | Output File | Corresponding Figure |
| :--- | :--- | :--- |
| `normal_distribution_rm.R` | `intrinsic_rate_panels.jpeg` | Figure 6a |
| `normal_distribution_lambda.R` | `finite_rate_panels.jpeg` | Figure 6b |
| `normal_distribution_R0.R` | `net_reproduction_rate_panels.jpeg` | Figure 6c |

Each panel includes:
- **Individual plots** for Control (blue), 1.5 ppm (red), and 8.5 ppm (green)
- **Comparison plot** showing all three distributions together
- **Percentile markers** (2.5%, 50%, 97.5%, and Original value)

---

##  Data Source

All summary statistics (means, standard deviations, and percentiles) used in these scripts were derived from **10,000 bootstrap resamples** of the original life-table data. The raw data supporting the findings of this study are available from the corresponding author upon reasonable request.

---

##  License

This code is licensed under the **MIT License**. See the `LICENSE` file for details.

---

##  Citation

If you use this code in your research, please cite the associated manuscript:

> [Authors]. (Year). Demographic Resilience of *Anopheles stephensi* to Detergent Pollution: Life-Table Responses and Implications for Urban Malaria Transmission. *Journal Name*, Volume(Issue), Pages. DOI: [DOI]

---

##  Contact

For questions or requests, please contact the corresponding author:

- ** Dr. Mahmoud Fazeli-Dinan**
- Associate professor of Entomology
- Email: fazelidinan@gmail.com
- Department of Medical Entomology, Mazandaran University of Medical Sciences

---

##  Zenodo DOI

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22629348.svg)](https://doi.org/10.5281/zenodo.22629348)

---

**Last updated:** [Current Date]
