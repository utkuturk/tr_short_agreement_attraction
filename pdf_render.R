# PDF render
## set working directory for the paper
setwd(
  paste0(
    here::here(),'/paper/glossa/'
  )
)
# Convert it to knitr
knitr::Sweave2knitr("manuscript.Rnw")


# knit it to a pdf
knitr::knit2pdf(compiler = 'xelatex', 'manuscript-knitr.Rnw') %>% 
  file.rename(.,'manuscript.pdf')

  
# delete aux
clean.knitr <- function() {
  if (.Platform$OS.type=="unix") {
    
    system(paste0("cd '", getwd(), 
                  "';rm *.gz;rm *.toc;rm *.log;rm *.glo;rm *.aux;rm *.ist;rm *-knitr.Rnw"))
  } else {
    warning("This function works only in UNIX systems")
  }
}

#clean.knitr()

# open the PDF
system2('open', args = 'manuscript.pdf', wait = FALSE)


