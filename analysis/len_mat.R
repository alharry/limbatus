
# Logistic regression analysis of maturity
mod_lm <- glm(Mat ~ STL * Sex, family = "binomial", data = data)
mod_lm2 <- glm(Mat ~ STL, family = "binomial", data = data)

# Analysis of deviance to determine whether sex is statistically significant 
anova_lm <- anova(mod_lm2, mod_lm, test = "Chisq")

# Run the length at maturity function for C. limbatus
lm_m <- filter(data, Sex == "Male") %>% mat_function(whichvar = "len_mat")
lm_f <- filter(data, Sex == "Female") %>% mat_function(whichvar = "len_mat")
lm_both <- mat_function(data, whichvar = "len_mat", facet_lab = "(a) Length at maturity - both sexes")

# Run the length at maturity function for Qld C. tilstoni
lm_abt_m <- filter(data_abt, Sex == "Male") %>% mat_function(whichvar = "len_mat")
lm_abt_f <- filter(data_abt, Sex == "Female") %>% mat_function(whichvar = "len_mat")
lmatern_abt <-filter(data_abt, Sex == "Female") %>% mat_function(whichvar = "len_matern")

# Create a data frame to store maturity data for NT C. tilstoni (raw data not available)
lm_abt_nt <- data_frame(Species = rep("C. tilstoni (NT)", 3), 
                          Sex = c("Male", "Female", "Female (Matern.)"), L50 = c(115, 120, 130))

# Bind results together
lm_results <- as_data_frame(rbind(lm_m$results,lm_f$results,lm_both$results, lm_abt_m$results, lm_abt_f$results, lmatern_abt$results)) %>% signif(4) %>%
  mutate(Sex = c("Male", "Female", "Male + Female", "Male", "Female","Female (Matern.)")) %>%
  mutate(Species = c(rep("C. limbatus", 3), rep("C. tilstoni (QLD)",3))) %>% 
  dplyr::select(Species, Sex, everything()) %>%
  full_join(lm_abt_nt) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female", "Female (Matern.)", "Both")) %>%
  mutate(Species = fct_relevel(Species, "C. tilstoni (NT)", "C. tilstoni (QLD)", "C. limbatus")) %>%
  mutate(facet = "(c) Length at maturity comparison")

# Save data
write_rds(mod_lm, "data/len_mat/mod_lm.rds")
write_rds(anova_lm, "data/len_mat/anova_lm.rds")
write_rds(lm_results, "data/len_mat/lm_results.rds")
write_rds(lm_both, "data/len_mat/lm_both.rds")
