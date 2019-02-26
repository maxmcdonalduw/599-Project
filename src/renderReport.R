library(rstudioapi)
library(rmarkdown)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

output_dir <- "../results"
render("analysis.Rmd", output_dir = output_dir)
