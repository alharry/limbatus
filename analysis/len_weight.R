
# Power curve analysis of length weight relationship
mod_lw <- with(data, lm(log(Wgt) ~ log(STL) * Sex))
mod_lw2 <- with(data, lm(log(Wgt) ~ log(STL)))

# Analysis of varianace to determine whether sex is statistically significant 
anova_lw <- anova(mod_lw, mod_lw2)

# Run len_weight analysis for sexes seprately and combined to get 
# model output and plots
lw_m <- filter(data, Sex == "Male") %>% len_weight()
lw_f <- filter(data, Sex == "Female") %>% len_weight()
lw_both <- len_weight(mutate(data, facet = "(a) Female + Male"))

# Ren length weight analysis for Qld C. tilstoni
lw_abt<-filter(data_abt) %>% len_weight()

# Bind together results
lw_results <- as_data_frame(rbind(lw_m$results,lw_f$results,lw_both$results)) %>% signif(4) %>%
  mutate(Sex = c("Male", "Female", "Both")) %>% 
  select(Sex, everything())

# Save data
write_rds(mod_lw, "data/len_weight/mod_lw.rds")
write_rds(mod_lw2, "data/len_weight/mod_lw2.rds")
write_rds(anova_lw, "data/len_weight/anova_lw.rds")
write_rds(lw_m, "data/len_weight/lw_m.rds")
write_rds(lw_f, "data/len_weight/lw_f.rds")
write_rds(lw_both, "data/len_weight/lw_both.rds")
write_rds(lw_abt, "data/len_weight/lw_abt.rds")
write_rds(lw_results, "data/len_weight/lw_results.rds")

