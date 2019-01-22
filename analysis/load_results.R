# Run setup and function scripts
source(here::here("analysis", "setup.R"))
source(here::here("analysis", "functions.R"))

## Load data
data <- read_rds("../data/data_final_btp.rds") # C. limbatus
data_abt <- read_rds("../data/data_final_abt.rds") # C. tilstoni Qld
data_all <- read_rds("../data/data_final_both.rds") # Both species

## Load results of analyses
# len_weight
mod_lw <- read_rds("../data/len_weight/mod_lw.rds")
mod_lw2 <- read_rds("../data/len_weight/mod_lw2.rds")
anova_lw <- read_rds("../data/len_weight/anova_lw.rds")
lw_m <- read_rds("../data/len_weight/lw_m.rds")
lw_f <- read_rds("../data/len_weight/lw_f.rds")
lw_both <- read_rds("../data/len_weight/lw_both.rds")
lw_abt <- read_rds("../data/len_weight/lw_abt.rds")
lw_results <- read_rds("../data/len_weight/lw_results.rds")

# len_age
age_dat <- read_rds("../data/len_age/age_dat.rds")
age <- read_rds("../data/len_age/age.rds")
preds <- read_rds("../data/len_age/preds.rds")
raw_points <- read_rds("../data/len_age/raw_points.rds")
preds_both <- read_rds("../data/len_age/preds_both.rds")
len0 <- read_rds("../data/len_age/len0.rds")
len0_plot <- read_rds("../data/len_age/len0_plot.rds")
age2 <- read_rds("../data/len_age/age2.rds")
true_age <- read_rds("../data/len_age/true_age.rds")
nursery <- read_rds("../data/len_age/nursery.rds")
mon_pred <- read_rds("../data/len_age/mon_pred.rds")
rep <- read_rds("../data/len_age/rep.rds")
qld_age_len_mod_m_log <- read_rds("../data/len_age/qld_age_len_mod_m_log.rds")
qld_age_len_mod_f_log <- read_rds("../data/len_age/qld_age_len_mod_f_log.rds")
qld_age_len_mod_m_vb <- read_rds("../data/len_age/qld_age_len_mod_m_vb.rds")
qld_age_len_mod_f_vb <- read_rds("../data/len_age/qld_age_len_mod_f_vb.rds")
resids <- read_rds("../data/len_age/resids.rds")

# len_mat
mod_lm <- read_rds("../data/len_mat/mod_lm.rds")
anova_lm <- read_rds("../data/len_mat/anova_lm.rds")
lm_results <- read_rds("../data/len_mat/lm_results.rds")
lm_both <- read_rds("../data/len_mat/lm_both.rds")

# age_mat
anova_am <- read_rds("../data/age_mat/anova_am.rds")
am_results <- read_rds("../data/age_mat/am_results.rds")
am_both <- read_rds("../data/age_mat/am_both.rds")

# len_clasper
clasp_mod <- read_rds("../data/len_clasper/clasp_mod.rds")
plot_data_both <- read_rds("../data/len_clasper/plot_data_both.rds")

# demography
monte_carlo <- read_rds("../data/demog/monte_carlo.rds")
demog_summary <- read_rds("../data/demog/demog_summary.rds")
m_r_int <- read_rds("../data/demog/m_r_int.rds")
