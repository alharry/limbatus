# Nonlinear regression model of clasper length vs length 

clasp_mod <- nls(log(Clasp.length) ~ log(b + ((a - b) * (1 + exp(-log(19) * (STL - L50)/(L95 - L50)))^-1)), data, 
                 start = list(a = 110, b = 10, L50 = 180, L95 = 220))

# Get bootstrap confidence intervals
#CI <- nlsBoot(clasp_mod, 1000)

# Save useful parameters
#clasp.pars <- signif(data.frame(coef(clasp_mod), CI$bootCI), 4)

# Get confidence and prediction intervals (2nd order Taylor expansion using propagate package)
xrange <- with(filter(data, Sex %in% "Male"), seq(min(STL, na.rm = T), max(STL, na.rm = T), 5))
invisible(capture.output(CI <- propagate::predictNLS(clasp_mod, newdata = data.frame(STL = xrange), interval = c("confidence"), do.sim = FALSE, second.order=T)))
invisible(capture.output(PI <- propagate::predictNLS(clasp_mod, newdata = data.frame(STL = xrange), interval = c("prediction"), do.sim = FALSE, second.order = T)))

# Save data for plotting
plot_data<-data_frame(STL = xrange, Clasp.length = exp(CI$summary[, 1]),
                      c.lower = exp(CI$summary[, 5]), c.upper = exp(CI$summary[, 6]),
                      p.lower = exp(PI$summary[, 5]), p.upper = exp(PI$summary[,6])) %>%
  mutate(Species = "C. limbatus") %>% mutate(cl_facet = "(a) Clasper development")

# C. tilstoni parameters for comparison
b = 14.02; a = 95.46; L50 = 118.2; L95 = 130.6

# Save tilstoni data for plotting
plot_data_abt <- data_frame(STL = min(data_abt$STL[data_abt$Sex == "Male"],
                                      na.rm = T):max(data_abt$STL[data_abt$Sex == "Male"], na.rm = T)) %>% 
  mutate(Clasp.length = b + ((a - b) * (1 + exp(-log(19) * (STL - L50)/(L95 - L50)))^-1)) %>%
  mutate(Species = "C. tilstoni (QLD)")

# Observed clasper length at length for both species
plot_data_both <- full_join(plot_data, plot_data_abt) %>%
  mutate(facet = "(b) Species comparison ")

# Save data
write_rds(clasp_mod, "data/len_clasper/clasp_mod.rds")
write_rds(plot_data_both, "data/len_clasper/plot_data_both.rds")

