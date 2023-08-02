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

exp1only <- F
lagoonly <- F
rt <- F

files <- c(
  "./code/scripts/00.0-packages.R",
  "./code/scripts/00.1-prepare.R",
  "./code/scripts/00.2-eda.R",
  "./code/scripts/misc.R",
  "./code/analysis/01_data_wrangling.R", "./code/analysis/02_word_freq.R",
  "./code/analysis/03_descriptive_plots.R", "./code/analysis/04_bayesian_models.R",
  "./code/analysis/05_model_plots.R", "./code/analysis/06_text_inputs.R",
  "./code/analysis/poss_vs_acc.R"
)

## source them all
for (f in files) {
  source(f)
}

## save the image
save.image(file = "./data/complete_environment.RData")
# load(file='./data/complete_environment.RData')
