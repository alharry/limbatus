## Setup script and load functions
source("analysis/setup.R")
source("analysis/functions.R")

## Load data
data <- read_rds("data/data_final_btp.rds") # C. limbatus
data_abt <- read_rds("data/data_final_abt.rds") # C. tilstoni Qld
data_all <- read_rds("data/data_final_both.rds") # Both species

## Run analyses
# Length weight analysis
source("analysis/len_weight.R")
# Length at age analysis
source("analysis/len_age.R")
# Length at maturity analysis
source("analysis/len_mat.R")
# Age at maturity analysis
source("analysis/age_mat.R")
# Clasper length analysis
source("analysis/len_clasper.R")
# Demographic analysis
source("analysis/demography.R")