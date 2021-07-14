# PDF render
## set working directory for the paper
setwd(
  paste0(
    dirname(rstudioapi::getActiveDocumentContext()$path),'/paper/glossa/'
  )
)

# Convert it to knitr
knitr::Sweave2knitr("manuscript.Rnw")

# knit it to a pdf
knitr::knit2pdf(compiler = 'xelatex', 'manuscript-knitr.Rnw')

# open the PDF
system2('open', args = 'manuscript-knitr.pdf', wait = FALSE)

