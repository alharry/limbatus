# Logistic regression analysis of maturity
mod_am <- glm(Mat ~ AgeAgree, family = "binomial", data = data)
mod_am2 <- glm(Mat ~ AgeAgree * Sex, family = "binomial", data = data)

# Analysis of deviance to determine whether sex is statistically significant 
anova_am <- anova(mod_am, mod_am2, test = "Chisq")

# Run maturity analysis function for C. limbbatus
# Males
am_m <- filter(data, Sex == "Male") %>% mat_function(which = "age_mat")
# Females
am_f <- filter(data, Sex == "Female") %>% mat_function(which = "age_mat")
# Both
am_both <- mat_function(data, which = "age_mat", facet_lab = "(b) Age at maturity - both sexes")

# Run maturity analysis function for Qld C. tilstoni
# Males
am_abt_m <- filter(data_abt, Sex == "Male") %>% mat_function(whichvar = "age_mat")
# Females
am_abt_f <- filter(data_abt, Sex == "Female") %>% mat_function(whichvar = "age_mat")
# Both
amatern_abt <-filter(data_abt, Sex == "Female") %>% mat_function(whichvar = "age_matern")

# Maturity parameters for NT C. tilstoni
am_abt_nt <- data_frame(Species = rep("C. tilstoni (NT)", 3), 
                        Sex = c("Male", "Female", "Female (Matern.)"), L50 = c(3.65, 4.07, 5.11))

# Tabulate results
am_results <- as_data_frame(rbind(am_m$results,am_f$results,am_both$results, am_abt_m$results, am_abt_f$results, amatern_abt$results)) %>% signif(4) %>%
  mutate(Sex = c("Male", "Female", "Male + Female", "Male", "Female","Female (Matern.)")) %>%
  mutate(Species = c(rep("C. limbatus", 3), rep("C. tilstoni (QLD)",3))) %>% 
  dplyr::select(Species, Sex, everything()) %>%
  full_join(am_abt_nt) %>%
  mutate(Sex = fct_relevel(Sex, "Male", "Female", "Female (Matern.)", "Both")) %>%
  mutate(Species = fct_relevel(Species, "C. tilstoni (NT)", "C. tilstoni (QLD)", "C. limbatus")) %>% 
  mutate(facet = "(d) Age at maturity comparison")

# Save results
write_rds(anova_am, "data/age_mat/anova_am.rds")
write_rds(am_results, "data/age_mat/am_results.rds")
write_rds(am_both, "data/age_mat/am_both.rds")
