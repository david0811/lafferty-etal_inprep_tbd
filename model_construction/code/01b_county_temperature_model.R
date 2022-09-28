library(MASS)
library(tidyverse)
library(rstanarm)
library(bayesplot)
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)
library(fixest)
library(modelr)

##################################################
# Read all relevant data
##################################################

# gridmet county obs
df_county_obs <- read_csv('../input_data/gridmet_county_weather_variables_1979-2020.csv')
df_county_obs$state <- df_county_obs$fips %>% substr(1,2) # add state column

df_mw_obs <- read_csv('../input_data/nclimgrid_midwest_tavg_1895-2021.csv')
tavg_mw_linmod <- lm(tavg_mw ~ year, data=df_mw_obs)
df_mw_obs$tavg_mw_dt <- resid(tavg_mw_linmod)

##################################################
# Midwest Tavg -> county Tavg
##################################################
df <- inner_join(df_county_obs, df_mw_obs)

# Visualize data
p <- ggplot(df, aes(x = tavg_mw_dt, y = tavg)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x='Midwest temperature anomaly (C)',
       y='County temperature (C)')
p

ggsave('../figures/county_tavg_raw.png',
       plot = p,
       width = 12, height = 6, units="in")

# OLS model
tavg_model_freq <- lm(tavg ~ tavg_mw_dt, data = df)
summary(tavg_model_freq)

# Bayesian model
tavg_model <- stan_glm(tavg ~ tavg_mw_dt, data = df,
                     family = gaussian,
                     chains = 3, iter = 10000*2, 
                     cores = 3, seed = 84735)

prior_summary(tavg_model)

# Effective sample size ratio and Rhat
neff_ratio(tavg_model)
rhat(tavg_model)

# Trace & density plots of parallel chains
mcmc_trace(tavg_model, size = 0.1)
mcmc_dens_overlay(tavg_model)

# Plot predictive
p1 <- df %>%
  data_grid(tavg_mw_dt = seq_range(tavg_mw_dt, n = 101)) %>%
  add_predicted_draws(tavg_model) %>%
  ggplot(aes(x = tavg_mw_dt, y = tavg)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df, size = 0.1) +
  scale_fill_brewer() + 
  labs(x='Midwest temperature anomaly (C)',
       y='County temperature (C)')

ggsave('../figures/county_tavg_bayes_fit.png',
       plot = p1,
       width = 12, height = 6, units="in")

#######################################
# Approximate parameter posteriors
#######################################
tavg_model_posterior <- as.data.frame(tavg_model)
colnames(tavg_model_posterior)[1] <- "intercept"

# tavg_mw param
tavg_mw_fit <- fitdistr(tavg_model_posterior$tavg_mw_dt,
                     densfun = "normal")

p_tavg_mw <- ggplot() + 
  geom_density(data = tavg_model_posterior,
               aes(x = tavg_mw_dt, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(tavg_model_posterior$tavg_mw_dt),
                                        max(tavg_model_posterior$tavg_mw_dt))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = tavg_mw_fit$estimate['mean'],
                            sd = tavg_mw_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Midwest Tavg Coefficient", y="",
       subtitle=paste("Normal(", signif(tavg_mw_fit$estimate['mean'], 3),
                      ",", signif(tavg_mw_fit$estimate['sd'], 3), ")", sep="")) 

# Intercept
intcp_fit <- fitdistr(tavg_model_posterior$intercept,
                      densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = tavg_model_posterior,
               aes(x = intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(tavg_model_posterior$intercept),
                                        max(tavg_model_posterior$intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Intercept", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep=""))

# Sigma
sigma_fit <- fitdistr(tavg_model_posterior$sigma,
                      densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = tavg_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(tavg_model_posterior$sigma),
                                        max(tavg_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

ggsave('../figures/county_tavg_posterior.png',
       plot = p_tavg_mw / (p_intcp | p_sigma),
       width = 12, height = 6, units="in")
