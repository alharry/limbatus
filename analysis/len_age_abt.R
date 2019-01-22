## Analysis of length at age data

## Prepare ageing data
age_dat <- filter(data_abt, !Umb.Scar %in% c("y","p")) %>% # Remove neonates, included below now
  select(STL, Month, Sex, AgeAgree, Reader1, Reader2) %>% na.omit() %>%
  mutate(partial_age = AgeAgree - Reader2) %>% 
  mutate(adjustment = ifelse(partial_age < 0, -1, partial_age)) %>%
  mutate(adjustment = ifelse(partial_age > 1, 1, adjustment)) %>%
  mutate(partial_age = ifelse(partial_age < 0, partial_age + 1, partial_age)) %>%
  mutate(Reader1 = Reader1 + adjustment) %>% 
  mutate(Reader2 = Reader2 + adjustment) %>%
  mutate(Reader1 = pmax(0, Reader1), Reader2 = pmax(0, Reader2)) 
 
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
len0 <- filter(data_abt, Umb.Scar %in% c("y","p")) %>%
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
  K = c(0.2, 0.2),
  L0 = min(list_data$len),
  CV_L = 0.06,
  # Gamma distribution
  logShape = log(4),
  logScale = log(2),
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
