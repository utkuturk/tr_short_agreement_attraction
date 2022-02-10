# Analysis

## Set seed
set.seed(42)

## Set the path to the active document (full_analysis_and_pdf)
setwd(here::here())


## Set parameters for the model analysis
# parameters:
# By default, only run a model for 'acceptable responses
# in which the data from exp1 and lago et al is pooled
# Change the following values to True if you would like to have
# additional exp1 only bayesian model and coefs
# additional lago only bayesian model and coes
# model of the RTs

exp1only = F; lagoonly = F; rt = F

files = c("./R/scripts/00.0-packages.R",
            "./R/scripts/00.1-prepare.R",
            "./R/scripts/00.2-eda.R",
            "./R/scripts/misc.R", 
            "./R/analysis/01_data_wrangling.R", "./R/analysis/02_word_freq.R", 
            "./R/analysis/03_descriptive_plots.R","./R/analysis/04_bayesian_models.R",
            "./R/analysis/05_model_plots.R", "./R/analysis/06_text_inputs.R", 
            "./R/analysis/poss_vs_acc.R")

## source them all
for (f in files) {
  source(f)
}

## save the image
save.image(file='./data/complete_environment.RData')
#load(file='./data/complete_environment.RData')
