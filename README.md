# Data, code, and paper for "Agreement Attraction in Turkish: The Case of Genitive Attractors"

## Folder Structure

```
.
├── LICENSE
├── README.md
├── code
│   ├── analysis
│   ├── fits
│   ├── models
│   └── scripts
├── data
│   ├── all_ud_tr.conllu
│   ├── exp_data.rds
│   ├── exp_form.rds
│   └── frequencies_exp1_and_lago.xlsx
├── dataraw
│   ├── experiment
│   └── lagoetal
├── full_analysis.R
└── pdf_render.R
```

## Run Order and Details

- To run all code, open [full_analysis.R](full_analysis.R) file and run everything inside. It may take some time due to 2 Bayesian GLM Models.
- Stand-alone scripts and data analysis can be found in R directory. Follow the order of the scripts to replicate our analysis.
  1. [packages](code/scripts/00.0-packages.R)
  2. [read_ibex](code/scripts/00.1-prepare.R)
  3. [eda](code/scripts/00.2-eda.R)
  4. [misc_scripts](code/scripts/misc.R)
  5. [data_wrangling](code/analysis/01_data_wrangling.R)
  6. [word_frequencies](code/analysis/02_word_freq.R)
  7. [descriptive_plots](code/analysis/03_descriptive_plots.R)
  8. [bayesian_models](code/analysis/04_bayesian_models.R)
  9. [model_plots](code/analysis/05_model_plots.R)
  10. [text_inputs](code/analysis/06_text_inputs.R)
- Stan models exported from the brms package can be found in [code/stan/](code/stan/) folder.
- Raw data from both Lago et al. (2019) and our experiment can be found in [dataraw/](dataraw/).

## Requirements

List of packages, softwares, fonts you need for replicating our paper and models:
* LaTeX
* R
* Stan
* and following R packages
  * languageR
  * tidyverse
  * gdata
  * MASS
  * magrittr
  * ggplot2
  * car
  * brms
  * xtable
  * rstan
  * knitr
  * dplyr
  * plyr
