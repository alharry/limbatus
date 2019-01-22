# Supporting functions 

## length weight function

# Function for running analysis, plotting and extracting coefficients
len_weight <- function(data){
  
  data <- filter(data, !is.na(Wgt)) %>% filter(!is.na(STL))
  
  # Run GLM
  m <- glm(log(Wgt) ~ log(STL), family = gaussian(link = "identity"), data = data)
  
  # Critical value of t for 95% confidence intervals
  tcrit<-qt(1-0.05/2, m$df.residual) 
  
  # Get the mean and asymptotic confidence intervals over the range of data for plotting
  newdat<-data_frame(STL = seq(min(m$data$STL, na.rm = T), max(m$data$STL, na.rm = T))) %>%
    mutate(mu = predict(m, ., type = "link", se.fit = T)$fit) %>% 
    mutate(se = predict(m, ., type = "link", se.fit = T)$se.fit) %>% 
    mutate(upper = mu + se*tcrit) %>%
    mutate(lower = mu - se*tcrit) %>%
    mutate(p_upper = mu + sqrt(se^2 + summary(m)$dispersion) * tcrit) %>%
    mutate(p_lower = mu - sqrt(se^2 + summary(m)$dispersion) * tcrit) %>%
    mutate(mu = exp(mu), upper = exp(upper), lower = exp(lower)) %>%
    mutate(p_upper = exp(p_upper), p_lower = exp(p_lower))
  
  # Plot
  p <- ggplot(newdat, aes(x = STL, y = mu)) + 
    geom_ribbon(aes(ymin = p_lower, ymax = p_upper), fill = "grey80") +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey60") + 
    geom_line() +
    geom_point(data = data, aes(x = STL, y = Wgt), pch = 21, fill = "#41b6c4") + 
    xlab("Total length (mm)") + ylab("Weight (kg)") 
  
  # Results
  r <- data_frame(ln_a = coef(m)[1], SE_a = summary(m)$coef[1,2],
                  b = coef(m)[2], SE_b = summary(m)$coef[2,2],
                  sigma = sqrt(summary(m)$dispersion), n = length(data$STL),
                  min_STL = min(m$data$STL, na.rm = T),
                  max_STL = max(m$data$STL, na.rm = T),
                  min_Wgt = min(m$data$Wgt, na.rm = T),
                  max_Wgt = max(m$data$Wgt, na.rm = T))
  
  # Return results
  return(list(model = m, plot = p, results = r, pred = newdat))  
}

## Maturity function

# Function for running length at maturity analysis, plotting and extracting coefficients
mat_function <- function(data, whichvar = "len_mat", facet_lab = "NA"){
  
  data = switch(whichvar, 
         len_mat = select(data, x = STL, y = Mat),
         age_mat = select(data, x = AgeAgree, y = Mat),
         len_matern = select(data, x = STL, y= Matern),
         age_matern = select(data, x= AgeAgree, y= Matern))
  
  plot.int = switch(whichvar, 
                    len_mat = 1,
                    age_mat = 0.1,
                    len_matern = 1,
                    age_matern = 0.1)
  
  point.int = switch(whichvar, 
                    len_mat = 10,
                    age_mat = 2,
                    len_matern = 10,
                    age_matern = 2)
  
  
  # Run GLM
  m <- glm(y ~ x, data = data, family ="binomial")
  
  # Get mean and asymptotic wald-type confidence intervals over the range of data for plotting
  newdat<-data_frame(x = with(data, seq(min(x, na.rm = T), max(x, na.rm = T), plot.int))) %>%
    mutate(mu = predict(m, ., type = "link", se.fit = T)$fit) %>% 
    mutate(se = predict(m, ., type = "link", se.fit = T)$se.fit) %>% 
    mutate(upper = mu + se*1.96) %>%
    mutate(lower = mu - se*1.96) %>%
    mutate(mu = m$family$linkinv(mu), upper = m$family$linkinv(upper), lower = m$family$linkinv(lower))
  
  # L50 and L95
  L50 = dose.p(m, p = 0.5)
  L95 = dose.p(m, p = 0.95)
  
  # Bootstrapping
  R = 1000
  mat_bootstraps <- boot(data, statistic = logistic_boot, R, parallel = "snow")
  mat_ci <- logistic_confints(data, mat_bootstraps, m, plot.int)
  
  # Save bootstrap confidence intervals
  newdat <- left_join(newdat, mat_ci) %>% select(-fitted.maturity) %>% 
  mutate(facet = facet_lab)
  
  # Tabulate observed proportions by a length interval (5 cm)
  minx <- min(data$x, na.rm =T) 
  maxx <- max(data$x, na.rm =T)
  Props <- data_frame(x = seq(minx, maxx, point.int)+point.int/2)
  Props$y <- prop.table(table(data$y, cut(data$x, seq(minx, maxx+point.int, point.int))), margin = 2)[2,]
      
  # Plot
  p <- ggplot(newdat, aes(x = x, y = mu)) + 
    geom_ribbon(aes(ymin = lower_boot, ymax = upper_boot), fill = "grey60") + 
    geom_line() +
    geom_point(data = Props, aes(x = x, y = y)) + 
    xlab("") + ylab("")
  
  # Save useful results
  r <- data_frame(a = coef(m)[1], SE_a = summary(m)$coef[1,2], SE_a_boot = sd(mat_bootstraps$t[,1]),
                  b = coef(m)[2], SE_b = summary(m)$coef[2,2], SE_b_boot = sd(mat_bootstraps$t[,2]),
                  L50 = L50, SE_L50 = attributes(L50)$SE[1], SE_L50_boot = sd(mat_bootstraps$t[,3]),
                  L95 = L95, SE_L95 = attributes(L95)$SE[1], SE_L95_boot = sd(mat_bootstraps$t[,4]),
                  L50_lower = envelope(mat_bootstraps)$point[2,3], L50_upper = envelope(mat_bootstraps)$point[1,3],
                  n = sum(table(data$y)), N = table(data$y)[2]) %>% signif(4)

  # Return results
  return(list(model = m, plot = p, results = r, bootstraps = mat_bootstraps))
}

# Function for bootstrapping the data. This simply repeats the above analysis. 
# The parameters are also expressed in terms of L50 and L95. 
logistic_boot <- function(data, i){
  data <- data[i, ]
  mod <- glm(y ~ x, data = data, family ="binomial")
  L50 <- dose.p(mod, p = c(0.5))
  L95 <- dose.p(mod, p = c(0.95))
  Bootstats <- c(coefficients(mod), L50, L95)
}

logistic_confints <- function(data, boot.out, mod, plot.int){
  Boot.results <- boot.out
  x <- seq(min(data$x, na.rm = T), max(data$x, na.rm = T), plot.int)
  Model <- mod
  par1 <- Boot.results$t[, 1]
  par2 <- Boot.results$t[, 2]
  par1.vec <- rep(par1, each = length(x))
  par2.vec <- rep(par2, each = length(x))
  predicted.maturity <- 1/(1 + exp(-(par1.vec + par2.vec * x)))
  plotmatrix <- matrix(predicted.maturity, ncol = length(x), byrow = T)
  
  OUT <- as_data_frame(plyr::adply(plotmatrix, 2, quantile, c(.025, .975))) %>% 
    select(lower_boot = `2.5%`, upper_boot = `97.5%`) %>%
    mutate(x = x) %>% 
    mutate(fitted.maturity = 1/(1 + exp(-(coef(Model)[[1]] + coef(Model)[[2]] * x))))
  
  return(OUT)
}

# Function for calculating ageing CV
cvfun <- function(x) {
  sd(x, na.rm = T) / mean(x, na.rm = T)
}

## Demographic analysis

# The continuous time Lotka equation (stripped back, F, M held constant)
continuous_lotka <- function(a, Lambda, par, continuous = TRUE) {

  # Length at age function
  Length <- function(a) {
    if (par$growth_type %in% "VB") {
      par$L0_f + (par$Linf_f - par$L0_f) * (1 - exp(-par$K_f * a))
    } else if (par$growth_type %in% "LOG") {
      (par$Linf_f * par$L0_f) / (par$L0_f + (par$Linf_f - par$L0_f) * exp(-par$K_f * (a)))
    } else if (par$growth_type %in% "VB1") {
      # Had to use t0 version for NT blacktip
      par$Linf_f * (1 - exp(-par$K_f * (a - par$L0_f)))
    }
  }

  # Maturity at age function
  Maturity <- function(a) {
    # Right shift parameters by a fixed amount to account for delay before
    # first reproduction if not using maternity ogive
    A50 <- par$A50 + par$matern_adjust
    A95 <- par$A95 + par$matern_adjust
    (par$Pmax) / (1 + exp(-log(19) * ((a - A50) / (A95 - A50))))
  }

  # Fecundity at age function
  Fecundity <- function(a) {
    par$FecA + par$FecB * Length(a)
  }

  # Birth function
  Birth <- function(a) {
    pmax(0, Fecundity(a) * Maturity(a) * par$sex_ratio)
  }

  # Intrinsic rate of population decrease with age
  LAMBDA <- function(a) {
    rep(Lambda, length(a))
  }

  # Sum to optimize
  Birth(a) * exp(sapply(a, function(x) {
    integrate(LAMBDA, 0, x)$value
  }))
}
# Solve for Lambda
solve_lotka<-function(par, Lambda = 0){
  Integral<-integrate(continuous_lotka, 0, Inf, Lambda = Lambda, par = par, continuous = TRUE)$value
  dif<-abs(1-Integral)
  return(dif)
}

# Optimize for lambda
optimize_lotka <- function(par, lower = -2, upper = 0){
  optimize(solve_lotka, c(lower, upper), par = par) %>% tbl_df %>%
    rename(Lambda = minimum)}



stable_age <- function(a, par, sex = "f", type = "biomass", include_age = FALSE){
  
  # Length at age function
  Length<-function(a){
    if(sex %in% "f"){
      if(par$growth_type %in% "VB"){
        par$L0_f + (par$Linf_f - par$L0_f) * (1 - exp(-par$K_f * a))
      }else if(par$growth_type %in% "LOG"){
        (par$Linf_f*par$L0_f)/(par$L0_f+(par$Linf_f-par$L0_f)*exp(-par$K_f*(a)))
      } else if(par$growth_type %in% "VB1"){
        # Had to use t0 version for NT blacktip 
        par$Linf_f * (1 - exp(-par$K_f * (a - par$L0_f)))
      }} else if(sex %in% "m"){
        if(par$growth_type %in% "VB"){
          par$L0_m + (par$Linf_m - par$L0_m) * (1 - exp(-par$K_m * a))
        }else if(par$growth_type %in% "LOG"){
          (par$Linf_m*par$L0_m)/(par$L0_m+(par$Linf_m-par$L0_m)*exp(-par$K_m*(a)))
        } else if(par$growth_type %in% "VB1"){
          # Had to use t0 version for NT blacktip 
          par$Linf_m * (1 - exp(-par$K_m * (a - par$L0_m)))
        }}
    
  }
  
  Weight <- function(a){
    par$Wa * Length(a)^par$Wb
  }
  
  # Maturity at age function
  Maturity<-function(a){
    A50= if(sex %in% "f"){par$A50_f} else {par$A50_m}
    A95 = if(sex %in% "f"){par$A95_f} else {par$A95_m}
    1 / (1 + exp(-log(19) * ((a - A50)/(A95 - A50))))
  }
  
  # Maternity at age function
  Maternity<-function(a){
    # Right shift parameters by a fixed amount to account for delay before
    # first reproduction if not using maternity ogive
    A50 = par$A50 + par$matern_adjust
    A95 = par$A95 + par$matern_adjust
    (par$Pmax)/(1 + exp(-log(19) * ((a - A50)/(A95 - A50))))
  }
  
  LAMBDA<-function(a){
    if(sex %in% "f"){
      rep(par$Lambda_f, length(a))
    } else if (sex %in% "m"){
      rep(par$Lambda_m, length(a))
    }
  }
  
  if(include_age == FALSE){
  Weight(a) * exp(sapply(a, function(x){integrate(LAMBDA, 0, x)$value}))}
  else if(include_age == TRUE){
    data_frame(age = a, length = Length(a), maturity = Maturity(a), maternity = Maternity(a), 
               weight = Weight(a) * exp(sapply(a, function(x){integrate(LAMBDA, 0, x)$value})))
  }
}

# Function for calculating biomass-based mean age
mean_age_biom <- function(a, par, sex = "f") {
  denom <- integrate(stable_age, 0, Inf, par = par, sex = sex)$value
  sad <- stable_age(a, par = par, sex = sex) / denom

  return(a * sad)
}

# Function for calculating biomass-based variance of ages
var_age_biom <- function(a, par, sex = "f") {
  denom <- integrate(stable_age, 0, Inf, par = par, sex = sex)$value
  sad <- stable_age(a, par = par, sex = sex) / denom
  if (sex %in% "f") {
    mu <- par$mean_age_f
  } else if (sex %in% "m") {
    mu <- par$mean_age_m
  }

  return((a - mu)^2 * sad)
}

# Function for calulating reproductive output
r_0 <- function(a, par) {

  # Length at age function
  Length <- function(a) {
    if (par$growth_type %in% "VB") {
      par$L0_f + (par$Linf_f - par$L0_f) * (1 - exp(-par$K_f * a))
    } else if (par$growth_type %in% "LOG") {
      (par$Linf_f * par$L0_f) / (par$L0_f + (par$Linf_f - par$L0_f) * exp(-par$K_f * (a)))
    } else if (par$growth_type %in% "VB1") {
      # Had to use t0 version for NT blacktip
      par$Linf_f * (1 - exp(-par$K_f * (a - par$L0_f)))
    }
  }

  # Maternity at age function
  Maternity <- function(a) {
    # Right shift parameters by a fixed amount to account for delay before
    # first reproduction if not using maternity ogive
    A50 <- par$A50 + par$matern_adjust
    A95 <- par$A95 + par$matern_adjust
    (par$Pmax) / (1 + exp(-log(19) * ((a - A50) / (A95 - A50))))
  }

  # Fecundity at age function
  Fecundity <- function(a) {
    par$FecA + par$FecB * Length(a)
  }

  # Birth function
  Birth <- function(a) {
    pmax(0, Fecundity(a) * Maternity(a) * par$sex_ratio)
  }

  # Natural mortality
  Mortality <- function(a) {
    rep(-par$M_f, length(a))
  }

  # Product of age specific survivorship and fertility
  Birth(a) * exp(sapply(a, function(x) {
    integrate(Mortality, 0, x)$value
  }))
}