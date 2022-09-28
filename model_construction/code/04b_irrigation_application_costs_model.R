library(MASS)
library(tidyverse)
library(rstan)
library(rstanarm)
library(bayesplot)
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)
library(modelr)

##################################################
# Read all relevant data
##################################################

# USDA water applied (all crops)
df_water_applied <- read_csv('../input_data/usda_water_applied_2013-2018_tidy.csv')
df_water_applied$state_name = toupper(df_water_applied$state_name)

# USDA groundwater pumping electricity costs
df_irr_app_cost <- read_csv('../input_data/usda_irrigation_application_cost_per_acre_2013-2018.csv')

##################################################
# Model
##################################################
df <- inner_join(df_water_applied, df_irr_app_cost)

# Exploratory plots
p1 <- ggplot(df, aes(x = log(water_applied_per_acre), y = log(irr_app_cost_per_acre))) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "log water applied (mm/acre)", y = "log cost ($)")

ggsave(filename = '../figures/water_applied_cost_raw.png',
       plot = p1)

# OLS model
water_applied_cost_model_freq <- lm(log(irr_app_cost_per_acre) ~ log(water_applied_per_acre),
                                     data = df)
summary(water_applied_cost_model_freq)

# Bayesian model
water_applied_cost_model <- stan_glm(log(irr_app_cost_per_acre) ~ log(water_applied_per_acre),
                                     data = df,
                                     family = gaussian(),
                                     # prior_intercept = normal(0, 100),
                                     # prior = normal(0, 100),
                                     # prior_aux = exponential(0.0001),
                                     chains = 3, iter = 10000*2, 
                                     cores = 3, seed = 84735)

prior_summary(water_applied_cost_model)

# Effective sample size ratio and Rhat
neff_ratio(water_applied_cost_model)
rhat(water_applied_cost_model)

# Trace & density plots of parallel chains
mcmc_trace(water_applied_cost_model, size = 0.1)
mcmc_dens_overlay(water_applied_cost_model)

# Posterior predictive checks
pp_check(water_applied_cost_model, nreps = 100) + 
  xlab("log cost ($)") +
  theme_grey()

p1 <- df %>%
  data_grid(water_applied_per_acre = seq_range(water_applied_per_acre, n = 101)) %>%
  add_predicted_draws(water_applied_cost_model) %>%
  ggplot(aes(x = water_applied_per_acre, y = irr_app_cost_per_acre)) +
  stat_lineribbon(aes(y = exp(.prediction)), .width = c(.99, .95, .8, .5), color = "#08519C") +
  geom_point(data = df, size = 2) +
  labs(x = "Water applied (mm/acre)", y = "Cost ($)") +
  scale_fill_brewer() +
  grid_lines(color = "white")

ggsave('../figures/water_applied_cost_bayes_fit.png',
       plot = p1,
       width = 12, height = 6, units="in")

# Approximate parameter posterior
water_applied_cost_model_posterior <- as.data.frame(water_applied_cost_model)
colnames(water_applied_cost_model_posterior)[1] <- "intercept"
colnames(water_applied_cost_model_posterior)[2] <- "log_water_applied_per_acre"

# Water applied param
wa_fit <- fitdistr(water_applied_cost_model_posterior$log_water_applied_per_acre,
         densfun = "normal")

p_wa <- ggplot() + 
  geom_density(data = water_applied_cost_model_posterior,
               aes(x = log_water_applied_per_acre, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(water_applied_cost_model_posterior$log_water_applied_per_acre),
                                        max(water_applied_cost_model_posterior$log_water_applied_per_acre))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = wa_fit$estimate['mean'],
                            sd = wa_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="log water-applied coefficient", y="",
       subtitle=paste("Normal(", signif(wa_fit$estimate['mean'], 3),
                      ",", signif(wa_fit$estimate['sd'], 3), ")", sep="")) 

# Intercept
intcp_fit <- fitdistr(water_applied_cost_model_posterior$intercept,
                    densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = water_applied_cost_model_posterior,
               aes(x = intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(water_applied_cost_model_posterior$intercept),
                                        max(water_applied_cost_model_posterior$intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Intercept", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep="")) 

# Sigma
sigma_fit <- fitdistr(water_applied_cost_model_posterior$sigma,
                      densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = water_applied_cost_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(water_applied_cost_model_posterior$sigma),
                                        max(water_applied_cost_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

ggsave('../figures/water_applied_cost_bayes_posterior.png',
       plot = p_wa / (p_intcp | p_sigma),
       width = 12, height = 6, units="in")
