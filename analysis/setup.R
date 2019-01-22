# Setup

# Knitr options
knitr::opts_chunk$set(warning=FALSE, message=FALSE, echo = FALSE, dev = "pdf")

# Housekeeping
cat("\014")
set.seed(1)

# Load libraries
libs<-c("MASS", "mapdata", "tidyverse", "knitcitations", "bibtex", "grid",
        "FSA", "ggridges", "xtable", "sf", "ggmap", "marmap",
        "readxl", "gridExtra", "ellipse", "boot",  "TMB",
        "purrrlyr", "here")

lapply(libs, library, character.only = TRUE)

# Bibliography
cleanbib()
cite_options(citation_format = "pandoc", check.entries = FALSE)

# Plot theme
lim_theme <- theme(panel.border = element_rect(colour = "black", fill = "transparent"),
                   panel.background = element_blank(),
                   panel.grid = element_blank(),
                   legend.key = element_rect(colour = NA, fill = NA),
                   strip.background = element_rect(colour = "black"),  strip.text = element_text(hjust = 0.05),
                   legend.background = element_rect(colour = NA), legend.margin = margin(5, 5, 5, 5, unit = "pt"))
