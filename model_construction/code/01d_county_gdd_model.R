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

##################################################
# County GDD
##################################################

# Visualize data
p <- ggplot(df_county_obs, aes(x = tavg, y = GDD)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(y='County GDD',
       x='County temperature (C)')

ggsave('../figures/county_gdd_raw.png',
       plot = p,
       width = 12, height = 6, units="in")

# OLS model
gdd_model_freq <- lm(GDD ~ tavg, data = df_county_obs)
summary(gdd_model_freq)

# Bayesian model
gdd_model <- stan_glm(GDD ~ tavg, data = df_county_obs,
                     family = gaussian,
                     chains = 3, iter = 10000*2, 
                     cores = 3, seed = 84735)

prior_summary(gdd_model)

# Effective sample size ratio and Rhat
neff_ratio(gdd_model)
rhat(gdd_model)

# Trace & density plots of parallel chains
mcmc_trace(gdd_model, size = 0.1)
mcmc_dens_overlay(gdd_model)

# Plot predictive
sampling <- seq(1, nrow(df_county_obs), 10)
p <- df_county_obs %>%
  data_grid(tavg = seq_range(tavg, n = 101)) %>%
  add_predicted_draws(gdd_model) %>%
  ggplot(aes(x = tavg, y = GDD)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df_county_obs[sampling,], size = 0.01) +
  scale_fill_brewer() + 
  labs(x='County average temperature (C)',
       y='County GDD')

ggsave('../figures/county_gdd_bayes_fit.png',
       plot = p,
       width = 12, height = 6, units="in")

#######################################
# Approximate parameter posteriors
#######################################
gdd_model_posterior <- as.data.frame(gdd_model)
colnames(gdd_model_posterior)[1] <- "intercept"

# tavg param
tavg_fit <- fitdistr(gdd_model_posterior$tavg,
                        densfun = "normal")

p_tavg <- ggplot() + 
  geom_density(data = gdd_model_posterior,
               aes(x = tavg, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(gdd_model_posterior$tavg),
                                        max(gdd_model_posterior$tavg))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = tavg_fit$estimate['mean'],
                            sd = tavg_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Tavg Coefficient", y="",
       subtitle=paste("Normal(", signif(tavg_fit$estimate['mean'], 3),
                      ",", signif(tavg_fit$estimate['sd'], 3), ")", sep="")) 

# Intercept
intcp_fit <- fitdistr(gdd_model_posterior$intercept,
                      densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = gdd_model_posterior,
               aes(x = intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(gdd_model_posterior$intercept),
                                        max(gdd_model_posterior$intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Intercept", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep=""))

# Sigma
sigma_fit <- fitdistr(gdd_model_posterior$sigma,
                      densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = gdd_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(gdd_model_posterior$sigma),
                                        max(gdd_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

ggsave('../figures/county_gdd_posterior.png',
       plot = p_tavg / (p_intcp | p_sigma),
       width = 12, height = 6, units="in")
