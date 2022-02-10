load(file='./data/complete_environment.RData')

# PDF render
## set working directory for the paper
setwd(
  paste0(
    here::here(),'/paper/lcn_revision/'
  )
)

# Convert it to knitr
knitr::Sweave2knitr("manuscript.Rnw")

# knit it to a pdf
knitr::knit2pdf(compiler = 'luatex', 'manuscript-knitr.Rnw')

# open the PDF
system2('open', args = 'manuscript-knitr.pdf', wait = FALSE)

