library(MASS)
library(tidyverse)
library(bayesplot)
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)
library(vars)
library(forecast)
library(brms)

###############################################
#### Read all data
###############################################
df_maize <- read_csv('../input_data/usda_maize_prices_deflated_1948-2016.csv')
df_maize$log_maize_ppi  <- log(df_maize$price_ppi)

df_soy <- read_csv('../input_data/usda_soy_prices_deflated_1948-2016.csv')
df_soy$log_soy_ppi  <- log(df_soy$price_ppi)

df_nclim <- read_csv('../input_data/nclimgrid_midwest_tavg_1895-2021.csv')

df_gridmet <- read_csv('../input_data/gridmet_midwest_tavg_1979-2020.csv')

###############################################
#### Exploratory plots
###############################################
# Initial check: nclimgrid vs gridmet
ggplot() + 
  geom_line(data=df_nclim, aes(x=year, y=tavg_mw), color='blue') + 
  geom_smooth(data=df_nclim, aes(x=year, y=tavg_mw), color='blue', method='lm') + 
  geom_line(data=df_gridmet, aes(x=year, y=tavg_mw), color='orange') +
  geom_smooth(data=df_gridmet, aes(x=year, y=tavg_mw), color='orange', method='lm')

# detrend
nclim_tavg_mw_linmod <- lm(tavg_mw ~ year, data=df_nclim)
df_nclim$tavg_mw_dt <- resid(nclim_tavg_mw_linmod)

gridmet_tavg_mw_linmod <- lm(tavg_mw ~ year, data=df_gridmet)
df_gridmet$tavg_mw_dt <- resid(gridmet_tavg_mw_linmod)

# now...
ggplot() + 
  geom_line(data=df_nclim, aes(x=year, y=tavg_mw_dt), color='blue') + 
  geom_line(data=df_gridmet, aes(x=year, y=tavg_mw_dt), color='orange')

ggplot() + 
  geom_density(data=df_nclim, aes(tavg_mw_dt), color='blue') + 
  geom_density(data=df_gridmet, aes(tavg_mw_dt), color='orange')

# NClimGrid shows considerably larger variations than gridMET in 
# historical average temperatures. In order to keep the model internally
# consistent we should calibrate all modules that depend on Midwest Tavg
# against NClimGrid

# Crops
ggplot() + 
  geom_line(data = df_soy, aes(x = year, y = price_ppi), color = 'blue') +
  geom_smooth(data = df_soy, aes(x = year, y = price_ppi), color = 'blue') +
  geom_line(data = df_maize, aes(x = year, y = price_ppi), color = 'orange') + 
  geom_smooth(data = df_maize, aes(x = year, y = price_ppi), color = 'orange') + 
  labs(y='Price [2018$]', x='year')

p1 <- ggplot() + 
  geom_line(data = df_soy, aes(x = year, y = log_soy_ppi, color = 'blue')) +
  geom_smooth(data = df_soy, aes(x = year, y = log_soy_ppi), method = 'lm', color = 'blue') +
  geom_line(data = df_maize, aes(x = year, y = log_maize_ppi, color = 'orange')) + 
  geom_smooth(data = df_maize, aes(x = year, y = log_maize_ppi), method = 'lm', color = 'orange') + 
  labs(y='log-Price (2018$)', x='year') + 
  scale_colour_manual(name = 'Crop', 
                      values =c('blue'='blue','orange'='orange'),
                      labels = c('Soy','Maize'))
p1

# detrend crops
maize_linmod <- lm(log_maize_ppi ~ year, data=df_maize)
df_maize$log_maize_ppi_dt <- resid(maize_linmod)

soy_linmod <- lm(log_soy_ppi ~ year, data=df_soy)
df_soy$log_soy_ppi_dt <- resid(soy_linmod)

# Merge
df <- subset(df_soy, select=c('year', 'log_soy_ppi_dt')) %>%
  inner_join(subset(df_maize, select=c('year', 'log_maize_ppi_dt'))) %>%
  inner_join(subset(df_nclim, select=c('year', 'tavg_mw_dt')))

# Final exploratory
p2 <- ggplot() + 
  geom_line(data=df, aes(x=year, y=tavg_mw_dt)) +
  labs(y='Midwest Temp. Anomaly (C)', x='Year')
p2

ggsave('../figures/price_raw.png',
       plot = p1 / p2,
       width = 12, height = 6, units="in")

#####################
# Frequentist model
#####################
# AR
maize_price_model_freq <- Arima(y = df$log_maize_ppi_dt,
                                xreg = df$tavg_mw_dt,
                                order = c(1, 0, 0),
                                seasonal = c(0, 0, 0),
                                include.mean=F,
                                lambda=NULL)
summary(maize_price_model_freq)

soy_price_model_freq <- Arima(y = df$log_soy_ppi_dt,
                                xreg = df$tavg_mw_dt,
                                order = c(1, 0, 0),
                                seasonal = c(0, 0, 0),
                                include.mean=F,
                                lambda=NULL)
summary(soy_price_model_freq)
##########################
# Bayesian models
##########################
# Add lags for brms
df_fit <-
  df %>% 
  mutate(log_maize_ppi_dt_lag1 = lag(log_maize_ppi_dt),
         log_soy_ppi_dt_lag1 = lag(log_soy_ppi_dt))

################
#### Maize
################
maize_price_model_bayes <- brm(data = df_fit,
                               family = gaussian(link = 'identity'),
                               bf(log_maize_ppi_dt ~ 0 + log_maize_ppi_dt_lag1 + tavg_mw_dt),
                               iter = 10000*2,
                               chains = 3, cores = 3,
                               seed = 84735)

prior_summary(maize_price_model_bayes)

maize_price_model_bayes

# Effective sample size ratio and Rhat
neff_ratio(maize_price_model_bayes)
rhat(maize_price_model_bayes)

# Trace & density plots of parallel chains
mcmc_trace(maize_price_model_bayes, size = 0.1)
mcmc_dens_overlay(maize_price_model_bayes)

# Posterior predictive
yrs = seq(min(df_fit$year)+1, max(df_fit$year))
p <- rbind(predict(maize_price_model_bayes)) %>% 
  data.frame() %>% 
  mutate(year = yrs) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/3) +
  geom_line(aes(y = Estimate), color='white') +
  geom_point(data = df_fit, aes(x = year, y = log_maize_ppi_dt),
             size = 2) + 
  labs(x='Year', y='log price (detrended)',
       subtitle='2.5% - 97.5% posterior predictive')
p

ggsave('../figures/price_maize_bayes_fit.png',
       plot = p,
       width = 12, height = 6, units="in")

# Approximate parameter posteriors
maize_price_model_posterior <- as.data.frame(maize_price_model_bayes)

# lag 1
l1_fit <- fitdistr(maize_price_model_posterior$b_log_maize_ppi_dt_lag1,
                     densfun = "normal")

p_l1 <- ggplot() + 
  geom_density(data = maize_price_model_posterior,
               aes(x = b_log_maize_ppi_dt_lag1, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(maize_price_model_posterior$b_log_maize_ppi_dt_lag1),
                                        max(maize_price_model_posterior$b_log_maize_ppi_dt_lag1))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = l1_fit$estimate['mean'],
                            sd = l1_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="lag-1", y="",
       subtitle=paste("Normal(", signif(l1_fit$estimate['mean'], 3),
                      ",", signif(l1_fit$estimate['sd'], 3), ")", sep=""))

p_l1

# tavg
tavg_fit <- fitdistr(maize_price_model_posterior$b_tavg_mw_dt,
                           densfun = "normal")

p_tavg <- ggplot() + 
  geom_density(data = maize_price_model_posterior,
               aes(x = b_tavg_mw_dt, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(maize_price_model_posterior$b_tavg_mw_dt),
                                        max(maize_price_model_posterior$b_tavg_mw_dt))),
                aes(x = x),
                fun = dnorm,
                args = list(sd = tavg_fit$estimate['sd'],
                            mean = tavg_fit$estimate['mean']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Midwest Tavg", y="",
       subtitle=paste("Normal(", signif(tavg_fit$estimate['mean'], 3),
                      ",", signif(tavg_fit$estimate['sd'], 3), ")", sep="")) 

p_tavg

# sigma
sigma_fit <- fitdistr(maize_price_model_posterior$sigma,
                     densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = maize_price_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(maize_price_model_posterior$sigma),
                                        max(maize_price_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']
                            ),
                colour="black", lty="dashed", size=1) + 
  labs(x="sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

p_sigma

ggsave('../figures/price_maize_bayes_posterior.png',
       plot = p_tavg / (p_l1 | p_sigma),
       width = 12, height = 6, units="in")

################
### Soy
################
soy_price_model_bayes <- brm(data = df_fit,
                             family = gaussian(link = 'identity'),
                             bf(log_soy_ppi_dt ~ 0 + log_soy_ppi_dt_lag1 + tavg_mw_dt),
                             iter = 10000*2,
                             chains = 3, cores = 3,
                             seed = 84735)

prior_summary(soy_price_model_bayes)

soy_price_model_bayes

# Effective sample size ratio and Rhat
neff_ratio(soy_price_model_bayes)
rhat(soy_price_model_bayes)

# Trace & density plots of parallel chains
mcmc_trace(soy_price_model_bayes, size = 0.1)
mcmc_dens_overlay(soy_price_model_bayes)

# Posterior predictive
yrs = seq(min(df_fit$year)+1, max(df_fit$year))
p <- rbind(predict(soy_price_model_bayes)) %>% 
  data.frame() %>% 
  mutate(year = yrs) %>% 
  ggplot(aes(x = year)) +
  geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5),
              alpha = 1/3) +
  geom_line(aes(y = Estimate), color='white') +
  geom_point(data = df_fit, aes(x = year, y = log_soy_ppi_dt),
             size = 2) + 
  labs(x='Year', y='log price (detrended)',
       subtitle='2.5% - 97.5% posterior predictive')
p

ggsave('../figures/price_soy_bayes_fit.png',
       plot = p,
       width = 12, height = 6, units="in")

# Approximate parameter posteriors
soy_price_model_posterior <- as.data.frame(soy_price_model_bayes)

# lag 1
l1_fit <- fitdistr(soy_price_model_posterior$b_log_soy_ppi_dt_lag1,
                   densfun = "normal")

p_l1 <- ggplot() + 
  geom_density(data = soy_price_model_posterior,
               aes(x = b_log_soy_ppi_dt_lag1, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(soy_price_model_posterior$b_log_soy_ppi_dt_lag1),
                                        max(soy_price_model_posterior$b_log_soy_ppi_dt_lag1))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = l1_fit$estimate['mean'],
                            sd = l1_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="lag-1", y="",
       subtitle=paste("Normal(", signif(l1_fit$estimate['mean'], 3),
                      ",", signif(l1_fit$estimate['sd'], 3), ")", sep=""))

p_l1

# tavg
tavg_fit <- fitdistr(soy_price_model_posterior$b_tavg_mw_dt,
                     densfun = "normal")

p_tavg <- ggplot() + 
  geom_density(data = soy_price_model_posterior,
               aes(x = b_tavg_mw_dt, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(soy_price_model_posterior$b_tavg_mw_dt),
                                        max(soy_price_model_posterior$b_tavg_mw_dt))),
                aes(x = x),
                fun = dnorm,
                args = list(sd = tavg_fit$estimate['sd'],
                            mean = tavg_fit$estimate['mean']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Midwest Tavg", y="",
       subtitle=paste("Normal(", signif(tavg_fit$estimate['mean'], 3),
                      ",", signif(tavg_fit$estimate['sd'], 3), ")", sep="")) 

p_tavg

# sigma
sigma_fit <- fitdistr(soy_price_model_posterior$sigma,
                      densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = soy_price_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(soy_price_model_posterior$sigma),
                                        max(soy_price_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']
                ),
                colour="black", lty="dashed", size=1) + 
  labs(x="sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

p_sigma

ggsave('../figures/price_soy_bayes_posterior.png',
       plot = p_tavg / (p_l1 | p_sigma),
       width = 12, height = 6, units="in")

###############################################################################
# Check correlation
soy_model_pp <- posterior_predict(soy_price_model_bayes)
maize_model_pp <- posterior_predict(maize_price_model_bayes)

zrep_corr <- matrix(nrow=1, ncol=dim(maize_model_pp)[1])

for (i in 1:dim(maize_model_pp)[1]){
  zrep_corr[i] <- cor(soy_model_pp[i,], maize_model_pp[i,])
}

ggplot() + 
  geom_density(data=as.data.frame(t(zrep_corr)), aes(zrep_corr), color='black') +
  geom_vline(aes(xintercept=cor(df_fit$log_soy_ppi_dt, df_fit$log_maize_ppi_dt)), color='blue')
