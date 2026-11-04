# SNAD/SNACK Heatmap Paper

Stata and R code for recoding SNAD and SNACK study data, estimating associations between social and network measures and cognitive or mental-health outcomes, applying multiple-test correction, and producing heatmaps.

## Project structure

The repository is intended to be the `Code` folder. Keep the data and output folders beside it:

```
Parent folder/
├── Code/                              this repository
├── Data/
│   └── raw and intermediate/
└── Output/
    ├── Coefficients/
    ├── Figures/
    └── Tables/raw/
```

Create the `Data` and `Output` directories used by the scripts before running them. Data files are excluded from Git by `.gitignore` and should not be included here.

## Data

The recode script expects source files in `Data/raw and intermediate/`:

- `manual_fix_june10.dta`
- `snad-analysis-focal-r01match-20260701.dta`
- `SNAD-Analysis-focal-LONG-R01match-20260120.dta`
- `SNACK_W1_W2_W3.xlsx`
- `SNACK-wave1-long.dta`
- `250818_SNACK_W2_LONG-clean.dta`
- `NC-LONG-clean-250825.dta`

The scripts also refer to local absolute paths for the SNACK long-data files. Update those paths in `recodes_snadsnack_heatmap_paper.do` to match your machine before running it. Data are available from Siyun Peng and Anne Krendl upon request by a SNAD or SNACK lab team member.

## Requirements

- Stata intall `esttab` command
- R packages: `dplyr`, `tidyr`, `readr`, `ggplot2`, and `tibble`

Install the R packages once in R:

```r
install.packages(c("dplyr", "tidyr", "readr", "ggplot2", "tibble"))
```

The Stata scripts and the R script contain machine-specific `cd` paths. Update these paths to your local `Code` directory before running the scripts.


## Workflow

| Step | File | Input | Output |
|---|---|---|---|
| 1. Recode | `recodes_snadsnack_heatmap_paper.do` | SNAD/SNACK source files | Clean SNAD, SNACK, and combined datasets under `Data/` |
| 2. Analyze | `analysis_snadsnack_heatmap_paper.do` | `Data/combined_heatmap_paper.dta` | Coefficient CSVs, multiple-test-correction results, and a correlation workbook under `Output/` |
| 3. Plot | `visual_snadsnack_heatmap_paper.R` | `Output/Coefficients/cog_heatmap_coefs.csv` and `mh_heatmap_coefs.csv` | Cognitive and mental-health heatmap PNGs under `Output/Figures/` |

The analysis script loads the helper programs in `output_coefficients_program.do` and `multiple_test_correction_program.do` automatically.

## Output

The scripts create cleaned `.dta` datasets, coefficient and correction files, a correlation workbook, and two heatmap images. The analysis expects the corresponding output folders to exist; the R script creates the `Figures` folder if needed.
