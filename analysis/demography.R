# Demographic analysis

# Number of simulations
r = 1000

## Common blacktip parameters
growth_pars <- mvrnorm(r, mu = rep$par.fixed[1:5], rep$cov.fixed[1:5,1:5])
mat_pars <- mvrnorm(r, mu = coef(am_both$model), summary(am_both$model)$cov.unscaled)
lw_f_pars <- mvrnorm(r, mu = coef(lw_both$model), summary(lw_both$model)$cov.unscaled)

# Data from for parameters
par_lim <- data_frame(
  # Species 
  species = "C. limbatus",
  # Growth parameters                  
  growth_type = "VB", Linf_f = growth_pars[, 1], Linf_m = growth_pars[, 2], 
  L0_f = growth_pars[, 5], L0_m = growth_pars[, 5], K_f = growth_pars[, 3], K_m = growth_pars[, 4], 
  # Maternity parameters
  am_a = mat_pars[, 1], am_b = mat_pars[, 2], A50 = -am_a/am_b, A95 = log(19)/am_b - am_a/am_b, 
  Pmax = runif(r, 1/3, 1/2), matern_adjust = 1/Pmax,
  # Maturity parameters
  amat_a_f = mat_pars[, 1], amat_b_f = mat_pars[, 2], A50_f = -amat_a_f/amat_b_f, A95_f = log(19)/amat_b_f - amat_a_f/amat_b_f,
  amat_a_m = mat_pars[, 1], amat_b_m = mat_pars[, 2], A50_m = -amat_a_m/amat_b_m, A95_m = log(19)/amat_b_m - amat_a_f/amat_b_m,
  # Length weight parameters
  Wa = exp(lw_f_pars[,1]), Wb = lw_f_pars[,2], 
  # Fecundity parameters
  FecA = sample(c(6.6, 6.6, 6.7, 6, 6.8), r, replace = T), FecB = 0, sex_ratio = 1/2,
  # Selectivty and exploitation level (q)
  SelMin = 0, SelMax = Inf, q = 0,
  # Size based mortality (note multiplied by 10 for units in cm converted to mm)
  M_f = ((4.118 * K_f^0.73) * (Linf_f * 10)^(-0.33)),
  M_m = ((4.118 * K_m^0.73) * (Linf_m * 10)^(-0.33))) %>%
  mutate(M_f = M_f + rnorm(r, 0, mean(M_f)/5)) %>%
  mutate(M_m = M_m + rnorm(r, 0, mean(M_m)/5)) 

## Australian blacktip parameters (QLD)
growth_pars_m_vb <- tbl_df(mvrnorm(r, mu = coef(qld_age_len_mod_m_vb), summary(qld_age_len_mod_m_vb)$cov.unscaled))
growth_pars_f_vb <- tbl_df(mvrnorm(r, mu = coef(qld_age_len_mod_f_vb), summary(qld_age_len_mod_f_vb)$cov.unscaled))
mat_pars <- mvrnorm(r, mu = coef(amatern_abt$model), summary(amatern_abt$model)$cov.unscaled)
mat_pars_f <- mvrnorm(r, mu = coef(am_abt_f$model), summary(am_abt_f$model)$cov.unscaled)
mat_pars_m <- mvrnorm(r, mu = coef(am_abt_m$model), summary(am_abt_m$model)$cov.unscaled)
lw_f_pars <- mvrnorm(r, mu = coef(lw_abt$model), summary(lw_abt$model)$cov.unscaled)
f_mod <- with(filter(data_abt, Emb >0), lm(Emb~STL))
f_l_pars <- mvrnorm(r, mu = coef(f_mod), summary(f_mod)$cov.unscaled)
  
# Data from for parameters
par_til_qld <- data_frame(
  # Species 
  species = "C. tilstoni (QLD)",
  # Growth parameters                  
  growth_type = "LOG", 
  Linf_f = rnorm(r, summary(qld_age_len_mod_f_log)$coefficients[1,1], summary(qld_age_len_mod_f_log)$coefficients[1,2]),
  Linf_m = rnorm(r, summary(qld_age_len_mod_m_log)$coefficients[1,1], summary(qld_age_len_mod_m_log)$coefficients[1,2]), 
  L0_f = rnorm(r, summary(qld_age_len_mod_f_log)$coefficients[2,1], summary(qld_age_len_mod_f_log)$coefficients[2,2]),
  L0_m = rnorm(r, summary(qld_age_len_mod_m_log)$coefficients[2,1], summary(qld_age_len_mod_m_log)$coefficients[2,2]),
  K_f = rnorm(r, summary(qld_age_len_mod_f_log)$coefficients[3,1], summary(qld_age_len_mod_f_log)$coefficients[3,2]),
  K_m = rnorm(r, summary(qld_age_len_mod_m_log)$coefficients[3,1], summary(qld_age_len_mod_m_log)$coefficients[3,2]), 
  # Maternity parameters
  am_a = mat_pars[, 1], am_b = mat_pars[, 2], A50 = -am_a/am_b, A95 = log(19)/am_b - am_a/am_b, 
  Pmax = runif(r, 0.833, 1), matern_adjust = 0,
  # Maturity parameters
  amat_a_f = mat_pars_f[, 1], amat_b_f = mat_pars_f[, 2], A50_f = -amat_a_f/amat_b_f, A95_f = log(19)/amat_b_f - amat_a_f/amat_b_f,
  amat_a_m = mat_pars_m[, 1], amat_b_m = mat_pars_m[, 2], A50_m = -amat_a_m/amat_b_m, A95_m = log(19)/amat_b_m - amat_a_m/amat_b_m,
  # Length weight parameters
  Wa = exp(lw_f_pars[,1]), Wb = lw_f_pars[,2], 
  # Fecundity parameters
  FecA = f_l_pars[,1], FecB = f_l_pars[,2], sex_ratio = 1/2,
  # Selectivty and exploitation level (q)
  SelMin = 0, SelMax = Inf, q = 0,
  # Size based mortality (note multiplied by 10 for units in cm converted to mm)
  M_f = (4.118 * growth_pars_f_vb$abc3^0.73) * (growth_pars_f_vb$abc1 * 10)^(-0.33),
  M_m = (4.118 * growth_pars_m_vb$abc3^0.73) * (growth_pars_m_vb$abc1 * 10)^(-0.33)) %>%
  mutate(M_f = M_f + rnorm(r, 0, mean(M_f)/5)) %>%
  mutate(M_m = M_m + rnorm(r, 0, mean(M_m)/5))

## Australian blacktip parameters (NT)

# Data from for parameters
par_til_nt <- data_frame(
  # Species 
  species = "C. tilstoni (NT)",
  # Growth parameters                  
  growth_type = "VB1", 
  Linf_f = rnorm(r, 181.4, 181.4 * 0.05), # From Davenport and Stevens 1988, modal analysis with 5% CV
  Linf_m = rnorm(r, 156.8, 156.8 * 0.05), # From Davenport and Stevens 1988, modal analysis with 5% CV
  L0_f = rnorm(r, -2.1, 2.1 * 0.05), # From Davenport and Stevens 1988, modal analysis with 5% CV
  L0_m = rnorm(r, -1.9, 1.9 * 0.05), # From Davenport and Stevens 1988, modal analysis with 5% CV
  K_f = rnorm(r, 0.19, 0.19 * 0.05), # From Davenport and Stevens 1988, modal analysis with 5% CV
  K_m = rnorm(r, 0.25, 0.25 * 0.05), # From Davenport and Stevens 1988, modal analysis with 5% CV
  # Maturity parameters
  am_a = -999, am_b = -999, A50 = 5, A95 = 6, 
  am_var = A50 - rnorm(r, A50, A50/10),
  Pmax = 1, matern_adjust = 0,
  # Maturity parameters
  amat_a_f = -999, amat_b_f = -999, A50_f = 4, A95_f = 5,
  amat_a_m = -999, amat_b_m = -999, A50_m = 3, A95_m = 4,
  amat_f_var = A50_f - rnorm(r, A50_f, A50_f/10),
  amat_m_var = A50_m - rnorm(r, A50_m, A50_m/10),
  # Length weight parameters
  Wa = 4.75e-6, Wb = 3.06, 
  # Fecundity parameters
  FecA = rnorm(r, 3, 0.3), FecB = 0, sex_ratio = 0.462,
  # Selectivty and exploitation level (q)
  SelMin = 0, SelMax = Inf, q = 0,
  # Size based mortality (note multiplied by 10 for units in cm converted to mm)
  M_f = ((4.118 * K_f^0.73) * (Linf_f * 10)^(-0.33)),
  M_m = ((4.118 * K_m^0.73) * (Linf_m * 10)^(-0.33))) %>%
  mutate(M_f = M_f + rnorm(r, 0, mean(M_f)/5)) %>%
  mutate(M_m = M_m + rnorm(r, 0, mean(M_m)/5)) %>%
  # Add uncertainty to maturity
  mutate(A50 = A50 + am_var, A95 = A95 + am_var) %>%
  mutate(A50_f = A50_f + amat_f_var, A95_f = A95_f + amat_f_var) %>% 
  mutate(A50_m = A50_m + amat_m_var, A95_m = A95_m + amat_m_var)


## Run demographic analysis
# For each of the rows of simulated life history parameters generated above, the 
# code below runs elements of the demographic analysis.

par_df <- full_join(par_lim, par_til_qld) %>%  full_join(par_til_nt) %>% 
  filter(A50 > 3, A95 > 0) %>% 
  mutate(species = fct_relevel(species, "C. tilstoni (NT)", "C. tilstoni (QLD)", "C. limbatus")) %>%   
  mutate(ID = row_number()) %>% 
  group_by(ID) %>% nest(.key = "par") %>% 
  mutate(lotka = map(par, optimize_lotka)) %>% 
  unnest(par, lotka) %>% 
  rename(Lambda_f = Lambda) %>% 
  mutate(r_int = - Lambda_f - M_f) %>%
  mutate(Lambda_m = -r_int - M_m) %>%
  mutate(converge = round(objective, 2)) %>% select(-objective) %>% 
  group_by(ID) %>% nest(.key = "df") %>% 
  mutate(sad_integral_f = map_dbl(df, ~integrate(stable_age, 0, Inf, par = .x, sex = "f")$value)) %>% 
  mutate(sad_integral_m = map_dbl(df, ~integrate(stable_age, 0, Inf, par = .x, sex = "m")$value)) %>% 
  mutate(mean_age_f = map_dbl(df, ~integrate(mean_age_biom, 0, Inf, par = .x, sex ="f")$value)) %>% 
  mutate(mean_age_m = map_dbl(df, ~integrate(mean_age_biom, 0, Inf, par = .x, sex ="m")$value)) %>% 
  unnest() %>% 
  group_by(ID) %>% nest(.key = "df") %>% 
  mutate(var_age_f = map_dbl(df, ~integrate(var_age_biom, 0, Inf, par = .x, sex ="f")$value)) %>% 
  mutate(var_age_m = map_dbl(df, ~integrate(var_age_biom, 0, Inf, par = .x, sex ="m")$value)) %>% 
  mutate(r_0 = map_dbl(df, ~integrate(r_0, 0, Inf, par = .x)$value)) %>% 
  unnest() %>% 
  mutate(mean_age_f_n = -1/Lambda_f, var_age_f_n = 1/Lambda_f^2) %>%
  mutate(mean_age_m_n = -1/Lambda_m, var_age_m_n = 1/Lambda_m^2) %>% 
  group_by(ID) %>% nest(.key = "df") %>% 
  mutate(sad_f = map(df, ~ stable_age(par = .x, a = seq(0, 40, 0.2), sex = "f", include_age = TRUE))) %>% 
  mutate(sad_m = map(df, ~ stable_age(par = .x, a = seq(0, 40, 0.2), sex = "m", include_age = TRUE))) %>% 
  unnest(df)
  
# Save data

write_rds(par_df,"data/demog/monte_carlo.rds")

# Summarise
demog_summary <- group_by(par_df, species) %>% 
  summarise(r_int_mean = mean(r_int), r_int_sd = sd(r_int), 
  Lambda_f_mean = mean(Lambda_f), Lambda_f_sd = sd(Lambda_f),
  Lambda_m_mean = mean(Lambda_m), Lambda_m_sd = sd(Lambda_m),
  M_m_mean = mean(M_m), M_m_sd = sd(M_m),           
  M_f_mean = mean(M_f), M_f_sd = sd(M_f),
  age_m_mean_n = mean(mean_age_m_n), age_m_mean_n_sd = sd(mean_age_m_n),
  age_m_var_n = mean(var_age_m_n), age_m_var_n_sd = sd(var_age_m_n),
  age_f_mean_n = mean(mean_age_f_n), age_f_mean_n_sd = sd(mean_age_f_n),
  age_f_var_n = mean(var_age_f_n), age_f_var_n_sd = sd(var_age_f_n),
  age_m_mean = mean(mean_age_m), age_m_mean_sd = sd(mean_age_m),
  age_f_mean = mean(mean_age_f), age_f_mean_sd = sd(mean_age_f),
  age_m_var = mean(var_age_m), age_m_var_sd = sd(var_age_m),
  age_f_var = mean(var_age_f), age_f_var_sd = sd(var_age_f),
  r_0_mean = mean(r_0), r_0_sd = sd(r_0)) %>%
  mutate_if(is.numeric, signif, 2)

# Save summary
write_rds(demog_summary, "data/demog/demog_summary.rds")

# Now compare M and r_max against range of plausible vaules

# Limbatus
# Fixed parameters (assuming biennial pupping)
par_lim2 <- data_frame(
  # Species
  species = "C. limbatus",
  # Growth parameters
  growth_type = "VB",
  Linf_f = rep$par.fixed[1],
  L0_f = rep$par.fixed[5],
  K_f = rep$par.fixed[3],
  # Maturity parameters
  am_a = coef(am_both$model)[1], 
  am_b = coef(am_both$model)[2],
  A50 = -am_a/am_b,
  A95 = log(19)/am_b - am_a/am_b, 
  Pmax = 1/2, 
  matern_adjust = 1 / Pmax,
  # Length weight parameters
  Wa = coef(lw_both$model)[1],
  Wb = coef(lw_both$model)[2],
  # Fecundity parameters
  FecA = 6.6,
  FecB = 0,
  sex_ratio = 1 / 2
) 

# Fixed parameters (assuming triennial pupping)
par_lim2a <- data_frame(
  # Species
  species = "C. limbatus",
  # Growth parameters
  growth_type = "VB",
  Linf_f = rep$par.fixed[1],
  L0_f = rep$par.fixed[5],
  K_f = rep$par.fixed[3],
  # Maturity parameters
  am_a = coef(am_both$model)[1], 
  am_b = coef(am_both$model)[2],
  A50 = -am_a/am_b,
  A95 = log(19)/am_b - am_a/am_b, 
  Pmax = 1/3, 
  matern_adjust = 1 / Pmax,
  # Length weight parameters
  Wa = coef(lw_both$model)[1],
  Wb = coef(lw_both$model)[2],
  # Fecundity parameters
  FecA = 6.6,
  FecB = 0,
  sex_ratio = 1 / 2
)  


# Tilstoni
# Fixed parameters 
par_til_qld2 <- data_frame(
  # Species 
  species = "C. tilstoni (QLD)",
  # Growth parameters                  
  growth_type = "LOG", 
  Linf_f = summary(qld_age_len_mod_f_log)$coefficients[1,1],
  Linf_m = summary(qld_age_len_mod_m_log)$coefficients[1,1],
  L0_f = summary(qld_age_len_mod_f_log)$coefficients[2,1],
  L0_m = summary(qld_age_len_mod_m_log)$coefficients[2,1],
  K_f = summary(qld_age_len_mod_f_log)$coefficients[3,1], 
  K_m = summary(qld_age_len_mod_m_log)$coefficients[3,1],
  # Maturity parameters
  am_a = coef(amatern_abt$model)[1], am_b = coef(amatern_abt$model)[2], A50 = -am_a/am_b, A95 = log(19)/am_b - am_a/am_b, 
  Pmax = 1-(1-0.87)/2, matern_adjust = 0,
  # Length weight parameters
  Wa = exp(coef(lw_abt$model)[1]), Wb = coef(lw_abt$model)[2], 
  # Fecundity parameters
  FecA = coef(f_mod)[1], FecB = coef(f_mod)[2], sex_ratio = 1/2
)

# Tilstoni (NT)
# Fixed parameters
par_til_nt2 <- data_frame(
  # Species 
  species = "C. tilstoni (NT)",
  # Growth parameters                  
  growth_type = "VB1", 
  Linf_f = 181.4, 
  L0_f = -2.1,
  K_f = 0.19,
  # Maturity parameters
  am_a = -999, am_b = -999, A50 = 5, A95 = 6,
  Pmax = 1, matern_adjust = 0,
  # Length weight parameters
  Wa = 4.75e-6, Wb = 3.06, 
  # Fecundity parameters
  FecA = 3, FecB = 0, sex_ratio = 0.462
)

# Now show how r varies in response to m over 
# range of possible values
m_r_int<- full_join(par_lim2, par_lim2a) %>% 
  full_join(par_til_qld2) %>% 
  full_join(par_til_nt2) %>%
  by_row(..f = optimize_lotka, .collate = "cols") %>%  rename(Lambda_f = Lambda1) %>%
  group_by(species, Pmax) %>%
  right_join(expand(., M_f = c(0, -Lambda_f))) %>%
  mutate(r_int = -Lambda_f - M_f)


# Save summary
write_rds(m_r_int, "data/demog/m_r_int.rds")