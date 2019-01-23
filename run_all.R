## Run all

# Load libraries
library(knitr)
library(rmarkdown)

# Run scripts
source("analysis/query.R")
source("analysis/analyses.R")

# Generate files
render("rmds/manuscript.Rmd", output_format = "all", output_dir = "reports", envir = new.env())
render("rmds/figures.Rmd", output_file = "figures.pdf", output_dir = "reports", envir = new.env())
render("rmds/tables.Rmd", output_format = "all", output_dir = "reports", envir = new.env())
render("rmds/supplementary.Rmd", output_file = "reports/supplementary.pdf", output_dir = "reports", envir = new.env())

## Zotero citation key format
# [auth:lower]_[Title:lower:skipwords:select,1,1]_[year]