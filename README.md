# Data, code, and paper for "Agreement Attraction in Turkish: The Case of Genitive Attractors"

## Run Order and Details

- First open [full_analysis.R](full_analysis.R) file and run everything inside. It may take some time due to multiple Bayesian GLM Models. 
- Then, run [pdf_render.R](pdf_render.R). It converts the Sweave file into a knitr file and then render a pdf out of knitr file.
- Stand-alone scripts and data analysis can be found in R directory.
  - The ordering between them as follows: [packages](R/scripts/00.0-packages.R), [read_ibex](R/scripts/00.1-prepare.R), [eda](R/scripts/00.2-eda.R), [misc_scripts](R/scripts/misc.R), [data_wrangling](R/analysis/01_data_wrangling.R), [word_frequencies](R/analysis/02_word_freq.R), [descriptive_plots](R/analysis/03_descriptive_plots.R), [bayesian_models](R/analysis/04_bayesian_models.R), [bayesfactors](R/analysis/bayesfactor.r), [model_plots](R/analysis/05_model_plots.R), [text_inputs](R/analysis/06_text_inputs.R)
- Stan models exported from the brms package can be found in [R/stan/](R/stan/) folder.
- Raw data from both Lago et al. (2019) and our experiment can be found in [dataraw/](dataraw/).
- Complete environment needed for the paper, experimental data, and the demographic information of the experiment can be found in [data/](data/)
- Edits should be done in [manuscript.Rnw](paper/glossa/manuscript.Rnw) file.
- Our [PDF](paper/glossa/manuscript.pdf) is also present in [paper/glossa/](paper/glossa/)
- For automated word count, we use [this solution](https://tex.stackexchange.com/a/239703) by [Omar Wasow](https://tex.stackexchange.com/users/34597/omar-wasow) using Texcount Perl script. In addition to this solution, we excluded unnumbered parts automatically.

## Requirements

List of packages, softwares, fonts you need for replicating our paper and models:
* LaTeX
* Brill Fonts
* Fira Sans Fonts
* R
* Rstudio (needed for setwd to an active document)
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