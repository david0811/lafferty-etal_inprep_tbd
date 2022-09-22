library(MASS)
library(tidyverse)
library(bayesplot)
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)
library(fixest)
library(rjags)
library(R2jags)
library(parallel)
library(coda)

##################################################
# Read all relevant data
##################################################

# gridmet state obs
df_county_obs <- read_csv('../input_data/gridmet_county_weather_variables_1979-2020.csv')

# USDA yields
df_maize_yields <- read_csv('../input_data/usda_maize_yields_1979-2020.csv')
df_soy_yields <- read_csv('../input_data/usda_soy_yields_1979-2020.csv')

# USDA irrigated acres (survey)
df_irr_maize_yields <- read_csv('../input_data/usda_maize_yields_irrigated_1979-2020.csv')
df_irr_maize_yields$fips <- str_pad(df_irr_maize_yields$fips, 5, pad = "0")
df_irr_maize_yields$state <- str_pad(df_irr_maize_yields$state, 2, pad = "0")

df_irr_soy_yields <- read_csv('../input_data/usda_soy_yields_irrigated_1979-2020.csv')
df_irr_soy_yields$fips <- str_pad(df_irr_soy_yields$fips, 5, pad = "0")
df_irr_soy_yields$state <- str_pad(df_irr_soy_yields$state, 2, pad = "0")

# USDA irrigated acres (census)
df_irr_acre_maize <- read_csv('../input_data/usda_maize_irrigated_acres_1997-2017.csv')
df_irr_acre_soy <- read_csv('../input_data/usda_soy_irrigated_acres_1997-2017.csv')

##################################################
# Rainfed
##################################################

#################
## Maize
#################
# Remove counties with >20% irrigation share in survey 
df_maize_irr_check <- inner_join(df_maize_yields, df_irr_maize_yields,
                                 by = c('fips', 'year', 'state'),
                                 suffix = c('_rf', '_irr')) 
df_maize_irr_check$irr_acre_share <- df_maize_irr_check$area_irr / (df_maize_irr_check$area_rf + df_maize_irr_check$area_irr)
irr_fips <- df_maize_irr_check[df_maize_irr_check$irr_acre_share > 0.2,]$fips

df_maize_yields <- df_maize_yields[!df_maize_yields$fips %in% irr_fips,]

# Remove counties with >20% irrigation share in census (any of 1997, 2002, 2007, 2012, 2017)
df_maize_irr_check <- inner_join(df_maize_yields, df_irr_acre_maize)
df_maize_irr_check$irr_acre_share <- df_maize_irr_check$irrigated_acreage / (df_maize_irr_check$irrigated_acreage + df_maize_irr_check$area)
irr_fips <- df_maize_irr_check[df_maize_irr_check$irr_acre_share > 0.2,]$fips

df_maize_yields <- df_maize_yields[!df_maize_yields$fips %in% irr_fips,]

# Add log and year2
df_maize_yields$log_yield <- log(df_maize_yields$yield)
df_maize_yields$year2 <- df_maize_yields$year**2

# Merge with weather date
df_maize <- inner_join(df_county_obs, df_maize_yields)

# Remove county-level trends 
log_maize_trend_model <- feols(log_yield ~ 1 | fips[year], df_maize)
df_maize$log_yield_resid <- log_maize_trend_model$residuals

## Frequentist model
maize_yield_freq <- feols(log_yield ~ GDD + EDD + prcp + prcp2 | fips[year], df_maize)
summary(maize_yield_freq)

## Bayes model
# Input data for models
Y <- df_maize %>% pivot_wider(names_from = fips, values_from = log_yield, id_cols = year)
GDD <- df_maize %>% pivot_wider(names_from = fips, values_from = GDD, id_cols = year, values_fill=-1)
EDD <- df_maize %>% pivot_wider(names_from = fips, values_from = EDD, id_cols = year, values_fill=-1.)
P <- df_maize %>% pivot_wider(names_from = fips, values_from = prcp, id_cols = year, values_fill=-1.)
P2 <- df_maize %>% pivot_wider(names_from = fips, values_from = prcp2, id_cols = year, values_fill=-1.)

# Needs to be ordered by year
Y <- Y[order(Y$year),]
GDD <- GDD[order(GDD$year),]
EDD <- EDD[order(EDD$year),]
P <- P[order(P$year),]
P2 <- P2[order(P2$year),]

# Dims
nyrs <- nrow(Y)
ncounties <- ncol(Y)-1

# Remove year column
Technology = as.matrix(Y[,1]) - 1979
Y = as.matrix(Y[,1:ncounties+1])
GDD = as.matrix(GDD[,1:ncounties+1])
EDD = as.matrix(EDD[,1:ncounties+1])
P = as.matrix(P[,1:ncounties+1])
P2 = as.matrix(P2[,1:ncounties+1])

# Set up model
model.file <- function() {
  # Model Level 1 #
  for (i in 1:ncounties)
  {
    for (t in 1:nyrs)
    {
      Y[t,i] ~ dnorm(alpha[i] + 
                       beta1[i]*Technology[t,1] + 
                       beta2*GDD[t,i] + 
                       beta3*EDD[t,i] + 
                       beta4*P[t,i] + 
                       beta5*P2[t,i],
                       tau.y)
    }
  }
  
  # Model Level 2 #
  for (i in 1:ncounties)
  {
    alpha[i] ~ dnorm(a0, tau.alpha)
    beta1[i] ~ dnorm(a1, tau.beta1)
  }
  
  # Priors #
  beta2 ~ dnorm(0.0, 0.001)
  beta3 ~ dnorm(0.0, 0.001)
  beta4 ~ dnorm(0.0, 0.001)
  beta5 ~ dnorm(0.0, 0.001)
  
  tau.alpha ~ dgamma(0.001,0.001)
  tau.beta1 ~ dgamma(0.001,0.001)
  
  tau.y ~ dgamma(0.001,0.001)
  sigma.y <- 1/tau.y
  
  a0 ~ dnorm(0, 0.001)
  a1 ~ dnorm(0, 0.001)
}

jags.data <- list('Technology' = Technology,
                  'GDD' = GDD,
                  'EDD' = EDD,
                  'Y' = Y,
                  'P' = P,
                  'P2' = P2,
                  'ncounties' = ncounties,
                  'nyrs'= nyrs)

jags.params <- c('tau.y',
                 'alpha','beta1','beta2','beta3','beta4','beta5',
                 'tau.alpha','tau.beta1','tau.beta2','tau.beta3','tau.beta4','tau.beta5')

jags.inits <- NULL

# Fit the model using run.jags -- parallel #
# 50000 iter ~ 25 mins
start_time <- Sys.time()
yield_model <- jags.parallel(model.file = model.file,
                             data = jags.data,
                             parameters.to.save = jags.params,
                             inits = jags.inits,
                             n.iter=10000L,
                             n.burnin=1000L,
                             n.chains = 3)
Sys.time() - start_time

# Rhat
all_convergence <- yield_model$BUGSoutput$summary[,8]
rhat_summary <- c(min(all_convergence),quantile(all_convergence,c(0.01,0.1,0.25,0.5,0.75,0.9,0.99)),max(all_convergence),mean(all_convergence))
rhat_summary

# Trace & density plots of parallel chains
jagsfit.mcmc <- as.mcmc(yield_model)
ptest <- mcmc_trace(jagsfit.mcmc, size = 0.1,
           pars = c('beta2', 'beta3', 'beta4', 'beta5'))
ggsave('../figures/test.png',
       plot = ptest,
       width = 12, height = 6, units="in")
ptest <- mcmc_dens_overlay(jagsfit.mcmc, pars = c('beta2', 'beta3', 'beta4', 'beta5'))
ggsave('../figures/test.png',
       plot = ptest,
       width = 12, height = 6, units="in")

# Access posteriors
tau <- yield_model$BUGSoutput$sims.list$tau.y
alpha <- yield_model$BUGSoutput$sims.list$alpha
beta1 <- yield_model$BUGSoutput$sims.list$beta1
beta2 <- yield_model$BUGSoutput$sims.list$beta2
beta3 <- yield_model$BUGSoutput$sims.list$beta3
beta4 <- yield_model$BUGSoutput$sims.list$beta4
beta5 <- yield_model$BUGSoutput$sims.list$beta5

# Precip param
gdd_fit <- fitdistr(beta2,densfun = "normal")

p_gdd <- ggplot() + 
  geom_density(data = as.data.frame(beta2),
               aes(x = V1, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(beta2),
                                        max(beta2))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = gdd_fit$estimate['mean'],
                            sd = gdd_fit$estimate['sd']/2),
                colour="black", lty="dashed", size=1) + 
  labs(x="GDD Coefficient", y="",
       subtitle=paste("Normal(", signif(gdd_fit$estimate['mean'], 3),
                      ",", signif(gdd_fit$estimate['sd']/2, 3), ")", sep="")) 

ggsave('../figures/test.png',
       plot = p_gdd,
       width = 12, height = 6, units="in")

# Precip param
prcp_fit <- fitdistr(beta4,densfun = "normal")

p_prcp <- ggplot() + 
  geom_density(data = as.data.frame(beta4),
               aes(x = V1, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(beta4),
                                        max(beta4))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = prcp_fit$estimate['mean'],
                            sd = prcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Precip Coefficient", y="",
       subtitle=paste("Normal(", signif(prcp_fit$estimate['mean'], 3),
                      ",", signif(prcp_fit$estimate['sd'], 3), ")", sep="")) 

p_prcp

ggsave('../figures/test.png',
       plot = p_prcp,
       width = 12, height = 6, units="in")

#################
## Soy
#################
# Remove counties with >50% irrigation share in any of 1997, 2002, 2007, 2012, 2017
df_soy_irr_check <- inner_join(df_soy_yields, df_soy_irr_acre)
df_soy_irr_check$irr_acre_share <- df_soy_irr_check$irrigated_acreage / df_soy_irr_check$area
soy_irr_fips <- df_soy_irr_check[df_soy_irr_check$irr_acre_share > 0.5,]$fips

df_soy_yields <- df_soy_yields[!df_soy_yields$fips %in% soy_irr_fips,]

# Add log and year2
df_soy_yields$log_yield <- log(df_soy_yields$yield)
df_soy_yields$year2 <- df_soy_yields$year**2

# Remove county-level trends 
df_soy_yields$log_yield_resid <- feols(log_yield ~ 1 | fips[year, year2], df_soy_yields)$residuals

# Merge with weather date
df_soy <- inner_join(df_county_obs, df_soy_yields)

# Frequentist model
soy_yield_freq <- lm(log_yield_resid ~ GDD + EDD + prcp + prcp2 - 1, df_soy)
soy_yield_freq <- feols(log_yield ~ GDD + EDD + prcp + prcp2 | fips[year, year2], df_soy)
summary(soy_yield_freq)

# Check specific county
df_soy$freq_predict <- soy_yield_freq$fitted.values
county <- "17019"
ggplot(data = df_soy[df_soy$fips == county,]) + 
  geom_line(aes(year, log_yield)) + 
  geom_line(aes(year, freq_predict), color='red')

# Bayesian model
soy_yield_bayes <- stan_glm(log_yield_resid ~ GDD + EDD + prcp + prcp2,
                            data = df_soy,
                            family = gaussian(),
                            # prior_intercept = normal(0, 100),
                            # prior = normal(0, 100),
                            # prior_aux = exponential(0.0001),
                            chains = 3, iter = 10000*2, 
                            cores = 3, seed = 84735)

# Effective sample size ratio and Rhat
neff_ratio(soy_yield_bayes)
rhat(soy_yield_bayes)

# Trace & density plots of parallel chains
mcmc_trace(soy_yield_bayes, size = 0.1)
mcmc_dens_overlay(soy_yield_bayes)

# Posterior predictive checks
pp_check(soy_yield_bayes, nreps = 100) + 
  xlab("log Yield (bu/acre)") +
  theme_grey()
