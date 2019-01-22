## Analysis of length at age data

## Prepare ageing data
age_dat <- select(data, STL, Month, Sex, AgeAgree, Reader1, Reader2) %>% na.omit()

## Starting parameters
max_f <- max(age_dat$AgeAgree[age_dat$Sex=="Female"], na.rm=T) 
max_m <- max(age_dat$AgeAgree[age_dat$Sex=="Male"], na.rm=T) 

## Maxtrix of age data
age <- select(age_dat, Reader1, Reader2) %>%
  mutate(Reader1 = ifelse(Reader1 == 0, .Machine$double.eps, Reader1)) %>%
  mutate(Reader2 = ifelse(Reader2 == 0, .Machine$double.eps, Reader2))
  
## Calculate age CV
CV <- apply(age, 1, cvfun)
CVe<-sqrt(sum(CV^2)/length(CV))

## L0 observations
len0 <- filter(data, Umb.Scar %in% c("y","p")) %>%
  select(STL)

## Prepare data
list_data <- list(read = 2, # Number of reads
  len = age_dat$STL, 
  len0 = len0$STL, 
  sex = as.numeric(as.factor(age_dat$Sex)) - 1, # Dummy variable for sex
  age = as.matrix(age), 
  CVe = CVe,
  pred_age_f = seq(0, max_f, 0.1), # Plot ages
  pred_age_m = seq(0, max_m, 0.1))

# Prepare parameters
parameters <- list(
  # Growth model
  Linf = rep(max(list_data$len), 2),
  K = c(0.17, 0.17),
  L0 = min(list_data$len),
  CV_L = 0.06,
  # Gamma distribution
  #logShape = log(4),
  #logScale = log(2),
  #logRate = log(0.2),
  # Age random effect
  age_re = rep(1, length(list_data$len))
)

## Compile TMB code and load libraries
compile("analysis/len_age.cpp")
dyn.load(dynlib("analysis/len_age"))

## Run model
obj <- MakeADFun(list_data, parameters, DLL ="len_age", random="age_re", silent = TRUE)
obj$fn()
obj$gr()
opt <- nlminb(obj$par, obj$fn, obj$gr)
rep <- sdreport(obj)


## Get expected values and uncertainty
preds<-data_frame(age = c(list_data$pred_age_f,list_data$pred_age_m),
                  len = rep$value[which(!names((rep$value)) %in% "resid")],
                  sd = rep$sd[which(!names((rep$value)) %in% "resid")], 
  sex = c(rep("(a) Female",length(list_data$pred_age_f)),rep("(b) Male",length(list_data$pred_age_m)))) %>%
  mutate(upper = len + 1.96 * sd, lower = len - 1.96 * sd) %>% mutate(pupper = len + 1.96 * sqrt(sd^2 + 
  (len*rep$par.fixed[6])^2)) %>% mutate(plower = len - 1.96 * sqrt(sd^2 +(len*rep$par.fixed[6])^2)) %>%
  mutate(species = "C. limbatus")

## Raw data points using agreed age
raw_points <- data_frame(age = age_dat$AgeAgree, len = list_data$len, sex = list_data$sex) %>%
  mutate(sex = fct_recode(as.factor(sex), `(a) Female` = "0", `(b) Male` = "1")) %>% 
  mutate(species = "C. limbatus")

raw_points_abt <- select(data_abt, age = AgeAgree, len = STL, sex = facet) %>% filter(!is.na(sex)) %>% 
  mutate(species = "C. tilstoni (QLD)")

raw_points <- full_join(raw_points, raw_points_abt) %>% 
  mutate(species = factor(species, levels = c("C. tilstoni (QLD)","C. limbatus")))


## Growth curves for Australian blacktips
qld_age_abt_f <- seq(0, 14.8,0.1)
qld_len_abt_f <- (173.9 * 64.48 * exp(0.2676 * qld_age_abt_f))/(173.9 + 64.48*(exp(0.2676 * qld_age_abt_f) - 1))
qld_age_abt_m <- seq(0, 12.8, 0.1)
qld_len_abt_m <- (147.8 * 62.91 * exp(0.3479 * qld_age_abt_m))/(147.8 + 62.91*(exp(0.3479 * qld_age_abt_m) - 1))
nt_age_abt_f <- seq(0, 12, 0.1)
nt_len_abt_f <- 181.4 * (1 - exp(- 0.19 * (nt_age_abt_f -- 2.1)))
nt_age_abt_m <- seq(0, 8, 0.1)
nt_len_abt_m <- 156.8 * (1 - exp(- 0.25 * (nt_age_abt_m -- 1.9)))

## Refit Australian blactip models to get covariance matrices
qld_age_len_mod_f_log <- nls(log(len) ~ log((abc1 * abc2 * exp(abc3 * age)) / (abc1 + abc2 * ((exp(abc3 * age) - 1)))),
                         data = filter(raw_points_abt, sex %in% "(a) Female"), 
                         start = list(abc1 = 180, abc2 = 60, abc3 = 0.08), 
                         algorithm = "port")

qld_age_len_mod_m_log <- nls(log(len) ~ log((abc1 * abc2 * exp(abc3 * age)) / (abc1 + abc2 * ((exp(abc3 * age) - 1)))),
                                  data = filter(raw_points_abt, sex %in% "(b) Male"), 
                                  start = list(abc1 = 170, abc2 = 60, abc3 = 0.2), 
                                  algorithm = "port")

# VB Constrained for getting Linf and K required in M estimates
qld_age_len_mod_m_vb <- nls(len ~ abc2+(abc1-abc2)*(1-exp(-abc3*age)),
                                  data = filter(raw_points_abt, sex %in% "(b) Male"), 
                                  start = list(abc1 = 140, abc2 = 61.63, abc3 = 0.1459), 
                                  upper = list(abc1 = 147.8, abc2 = 100.00, abc3 = 1.0),
                                  algorithm = "port")
qld_age_len_mod_f_vb <- nls(len ~ abc2+(abc1-abc2)*(1-exp(-abc3*age)),
                            data = filter(raw_points_abt, sex %in% "(a) Female"), 
                            start = list(abc1 = 169.8, abc2 = 61.63, abc3 = 0.1459),
                            upper = list(abc1 = 173.9, abc2 = 100.00, abc3 = 1.0),
                            algorithm = "port")

## Predicted tilstoni length at age for plotting 
preds_abt <- data_frame(age = c(qld_age_abt_f, qld_age_abt_m, nt_age_abt_f, nt_age_abt_m), 
  len = c(qld_len_abt_f, qld_len_abt_m, nt_len_abt_f, nt_len_abt_m), 
  sex = c(rep("(a) Female", length(qld_age_abt_f)), rep("(b) Male", length(qld_age_abt_m)),
  rep("(a) Female", length(nt_age_abt_f)), rep("(b) Male", length(nt_age_abt_m))),
  species = c(rep("C. tilstoni (QLD)", length(c(qld_age_abt_f, qld_age_abt_m))), 
  rep("C. tilstoni (NT)", length(c(nt_age_abt_f, nt_age_abt_m)))))

# Predicted length at age both species
preds_both <- full_join(preds, preds_abt) %>% mutate(facet = "(e) Species comparison")

## Length at birth for both species
len0_plot <- filter(data_all, Umb.Scar %in% c("y", "p")) %>% select(species = Species, STL = STL) %>%
  mutate(facet = "(c) Length at birth")

## Underlying age distribution
age2 <- gather(age, key = "Reader", value = "age") %>% mutate(facet = "(d) Age structure")

#true_age <- data_frame(age = seq(0, 25, 0.1), dens_age = dgamma(seq(0, 25, 0.1), 
#  shape = exp(opt$par[7]), scale = exp(opt$par[8]))) 

# Comparison of age with nursery bound sharks
nursery <- filter(data, Source %in% c("QLD: 2004 - 2007")) %>%
  mutate(Month2 = ifelse(Month %in% c(12), Month-11,Month+1)) %>% 
  select(Month2, Tag, STL, Umb.Scar) %>% 
  # Manually add in apparent cohorts
  mutate(coh = ifelse(Umb.Scar %in% c("y","p"), "0", "0+")) %>% 
  mutate(coh = ifelse(Month2 %in% c("1","5") & STL > 80 & STL <100, "1+", coh)) %>%
  mutate(coh = ifelse(Month2 %in% c("11","12") & STL > 102 & STL <120, "1+", coh)) %>% 
  mutate(coh = ifelse(Month2 %in% c("1","5") & STL >= 100, "2+", coh)) %>%
  mutate(coh = ifelse(Month2 %in% c("11","12") & STL >=120, "2+", coh)) %>%
  mutate(facet = "(f) Nursery length structure")

Linf = opt$par[2]
L0 = opt$par[5]
K = opt$par[4]

# Vertebral length at age for 0+ to 3+
mon_pred <- data_frame(age = seq(0,3, 1/12)) %>%
  mutate(Age = L0 + (Linf - L0) * (1 - exp(-K *age))) %>%
  mutate(Month2 = c(rep(seq(1, 12, 1),3),1)) %>%
  mutate(Cohort = as.factor(c(rep("0+",12),rep("1+",12),rep("2+",12),"3+"))) %>%
  filter(!Cohort%in%"3+")

# Residual plot
resids <- data_frame(len_pred = obj$report()$Lt,
 resid = summary(rep)[which(rownames(summary(rep)) %in% "resid")]) %>%
 mutate(facet = "(d) Residuals")

# Save results
write_rds(age_dat, "data/len_age/age_dat.rds")
write_rds(age, "data/len_age/age.rds")
write_rds(preds, "data/len_age/preds.rds")
write_rds(raw_points, "data/len_age/raw_points.rds")
write_rds(preds_both, "data/len_age/preds_both.rds")
write_rds(len0, "data/len_age/len0.rds")
write_rds(len0_plot, "data/len_age/len0_plot.rds")
write_rds(age2, "data/len_age/age2.rds")
#write_rds(true_age, "data/len_age/true_age.rds")
write_rds(nursery, "data/len_age/nursery.rds")
write_rds(mon_pred, "data/len_age/mon_pred.rds")
write_rds(rep, "data/len_age/rep.rds")
write_rds(qld_age_len_mod_m_log, "data/len_age/qld_age_len_mod_m_log.rds")
write_rds(qld_age_len_mod_f_log, "data/len_age/qld_age_len_mod_f_log.rds")
write_rds(qld_age_len_mod_m_vb, "data/len_age/qld_age_len_mod_m_vb.rds")
write_rds(qld_age_len_mod_f_vb, "data/len_age/qld_age_len_mod_f_vb.rds")
write_rds(resids, "data/len_age/resids.rds")
