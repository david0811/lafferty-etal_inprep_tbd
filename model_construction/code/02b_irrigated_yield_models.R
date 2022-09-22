library(MASS)
library(tidyverse)
library(rstanarm)
library(bayesplot)
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)
library(fixest)

##################################################
# Read all relevant data
##################################################

# gridmet county obs
df_county_obs <- read_csv('../input_data/gridmet_county_weather_variables_1979-2020.csv')

# USDA irrigated & non-irrigated yields
df_maize_yields <- read_csv('../input_data/usda_maize_yields_irr_nonirr_1979-2020.csv')
df_maize_yields$fips <- str_pad(df_maize_yields$fips, 5, pad = "0")

df_soy_yields <- read_csv('../input_data/usda_soy_yields_irr_nonirr_1979-2020.csv')
df_soy_yields$fips <- str_pad(df_soy_yields$fips, 5, pad = "0")

#################
#################
#################
## Maize
#################
#################
#################
# Merge rainfed/irrigated yields with weather data
df_maize <- inner_join(df_maize_yields, df_county_obs,
                       by = c('fips', 'year'))

# Get yield differential
df_maize$irr_to_nonirr_diff <- df_maize$yield_irr - df_maize$yield_nonirr

# Exploratory plots
ggplot(data = df_maize) + 
  geom_boxplot(aes(x=year, y=yield_irr, group=year), color='blue') + 
  geom_boxplot(aes(x=year, y=yield_nonirr, group=year), color='orange') +
  facet_wrap(~ state)

ggplot(data = df_maize) + 
  geom_boxplot(aes(x=year, y=irr_to_nonirr_diff, group=year)) +
  facet_wrap(~ state)

ggplot(data = df_maize, aes(x=year, y=irr_to_nonirr_diff)) + 
  geom_point() + 
  geom_smooth(method='lm')

# There is a trend that seems to level off around 2000, so in the name of 
# simplicity we will filter year >= 2000
df_maize_2000 <- df_maize[df_maize$year > 2000,]

# Subset county count > 15
fips_min <- df_maize_2000 %>% group_by(fips) %>% count() %>% filter(n >= 10)
fips_min <- fips_min$fips
df_maize_2000 <- df_maize_2000[df_maize_2000$fips %in% fips_min,]

# Subset to 'realistic' values
df_maize_2000 <- df_maize_2000[df_maize_2000$irr_to_nonirr_diff > 0.,]

# More exploratory plots
ggplot(data = df_maize_2000) + 
  geom_boxplot(aes(x=year, y=irr_to_nonirr_diff, group=year))

ggplot(data = df_maize_2000, aes(x=year, y=irr_to_nonirr_diff)) + 
  geom_point() + 
  geom_smooth(method='lm')

ggplot(data = df_maize_2000) + 
  geom_boxplot(aes(x=year, y=irr_to_nonirr_diff, group=year)) +
  facet_wrap(~ state)

p1 <- ggplot(df_maize_2000, aes(x = prcp, y = irr_to_nonirr_diff)) + 
  geom_point(size = 2) + 
  geom_smooth(method='lm') +
  labs(y = "Yield Differential", x = "Precip",
       title = "Maize")

p2 <- ggplot(df_maize_2000, aes(x = EDD, y = irr_to_nonirr_diff)) + 
  geom_point(size = 2) + 
  geom_smooth(method='lm') +
  labs(x = "EDD", y = "Yield Differential")

ggsave(filename = '../figures/yield_differential_maize_raw.png',
       plot = p1 + p2 & scale_y_continuous(limits=c(0, 200)))

# OLS model
yield_diff_maize_model_freq <- lm(irr_to_nonirr_diff ~ EDD + prcp,
                                      data = df_maize_2000)
summary(yield_diff_maize_model_freq)

########################
# Bayesian model
########################
yield_diff_maize_model <- stan_glm(irr_to_nonirr_diff ~ EDD + prcp,
                                      data = df_maize_2000,
                                      family = gaussian(),
                                      chains = 3, iter = 10000*2, 
                                      cores = 3, seed = 84735)

prior_summary(yield_diff_maize_model)

# Effective sample size ratio and Rhat
neff_ratio(yield_diff_maize_model)
rhat(yield_diff_maize_model)

# Trace & density plots of parallel chains
mcmc_trace(yield_diff_maize_model, size = 0.1)
mcmc_dens_overlay(yield_diff_maize_model)

# Posterior predictive checks
p <- pp_check(yield_diff_maize_model, nreps = 1000) + 
  labs(x="Yield Differential", y="Density",
       title = "Maize",
       subtitle = "1000 Posterior Predictive Draws") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

ggsave('../figures/yield_differential_maize_bayes_fit1.png',
       plot = p,
       width = 12, height = 6, units="in")

# Sampling for posterior predictive checks as function of predictors
sampling <- seq(1, nrow(df_maize_2000), 10)
yrep <- posterior_predict(yield_diff_maize_model, draws=1000)

p1 <- ppc_intervals(
  y = df_maize_2000$irr_to_nonirr_diff[sampling],
  yrep = yrep[,sampling],
  x = df_maize_2000$prcp[sampling],
  prob = 0.90, prob_outer=0.99) +
  labs(x = "Precip", y = "Yield Differential",
       title = "Maize",
       subtitle = "90, 99% posterior predictive intervals") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

p2 <- ppc_intervals(
  y = df_maize_2000$irr_to_nonirr_diff[sampling],
  yrep = yrep[,sampling],
  x = df_maize_2000$EDD[sampling],
  prob = 0.90, prob_outer=0.99) +
  labs(x = "EDD") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

ggsave('../figures/yield_differential_maize_bayes_fit2.png',
       plot = p1 & theme(legend.position = "none") | p2,
       width = 12, height = 6, units="in")

#######################################
# Approximate parameter posteriors
#######################################
yield_diff_maize_model_posterior <- as.data.frame(yield_diff_maize_model)
colnames(yield_diff_maize_model_posterior)[1] <- "intercept"

# Precip param
prcp_fit <- fitdistr(yield_diff_maize_model_posterior$prcp,
                     densfun = "normal")

p_prcp <- ggplot() + 
  geom_density(data = yield_diff_maize_model_posterior,
               aes(x = prcp, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_maize_model_posterior$prcp),
                                        max(yield_diff_maize_model_posterior$prcp))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = prcp_fit$estimate['mean'],
                            sd = prcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Precip Coefficient", y="",
       subtitle=paste("Normal(", signif(prcp_fit$estimate['mean'], 3),
                      ",", signif(prcp_fit$estimate['sd'], 3), ")", sep="")) 

# EDD param
edd_fit <- fitdistr(yield_diff_maize_model_posterior$EDD,
                    densfun = "normal")

p_edd <- ggplot() + 
  geom_density(data = yield_diff_maize_model_posterior,
               aes(x = EDD, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_maize_model_posterior$EDD),
                                        max(yield_diff_maize_model_posterior$EDD))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = edd_fit$estimate['mean'],
                            sd = edd_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="EDD Coefficient", y="",
       subtitle=paste("Normal(", signif(edd_fit$estimate['mean'], 3),
                      ",", signif(edd_fit$estimate['sd'], 3), ")", sep="")) 

# Intercept
intcp_fit <- fitdistr(yield_diff_maize_model_posterior$intercept,
                      densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = yield_diff_maize_model_posterior,
               aes(x = intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_maize_model_posterior$intercept),
                                        max(yield_diff_maize_model_posterior$intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Intercept", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep=""))

# Sigma
sigma_fit <- fitdistr(yield_diff_maize_model_posterior$sigma,
                      densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = yield_diff_maize_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_maize_model_posterior$sigma),
                                        max(yield_diff_maize_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

ggsave('../figures/yield_differential_maize_posterior.png',
       plot = (p_prcp | p_edd) / (p_intcp | p_sigma),
       width = 12, height = 6, units="in")

#################
#################
#################
## Soy
#################
#################
#################
# Merge rainfed/irrigated yields with weather data
df_soy <- inner_join(df_soy_yields, df_county_obs,
                     by = c('fips', 'year'))

# Get yield differential
df_soy$irr_to_nonirr_diff <- df_soy$yield_irr - df_soy$yield_nonirr

# Exploratory plots
ggplot(data = df_soy) + 
  geom_boxplot(aes(x=year, y=yield_irr, group=year), color='blue') + 
  geom_boxplot(aes(x=year, y=yield_nonirr, group=year), color='orange') +
  facet_wrap(~ state)

ggplot(data = df_soy) + 
  geom_boxplot(aes(x=year, y=irr_to_nonirr_diff, group=year)) +
  facet_wrap(~ state)

ggplot(data = df_soy, aes(x=year, y=irr_to_nonirr_diff)) + 
  geom_point() + 
  geom_smooth(method='lm')

# There is a trend that seems to level off around 2000, so in the name of 
# simplicity we will filter year >= 2000
df_soy_2000 <- df_soy[df_soy$year >= 2000,]

# Subset county count > 10
fips_min <- df_soy_2000 %>% group_by(fips) %>% count() %>% filter(n >= 10)
fips_min <- fips_min$fips
df_soy_2000 <- df_soy_2000[df_soy_2000$fips %in% fips_min,]

# Subset to 'realistic' values
df_soy_2000 <- df_soy_2000[df_soy_2000$irr_to_nonirr_diff > 0.,]

# More exploratory plots
ggplot(data = df_soy_2000) + 
  geom_boxplot(aes(x=year, y=irr_to_nonirr_diff, group=year))

ggplot(data = df_soy_2000, aes(x=year, y=irr_to_nonirr_diff)) + 
  geom_point() + 
  geom_smooth(method='lm')

ggplot(data = df_soy_2000) + 
  geom_boxplot(aes(x=year, y=irr_to_nonirr_diff, group=year)) +
  facet_wrap(~ state)

p1 <- ggplot(df_soy_2000, aes(x = prcp, y = irr_to_nonirr_diff)) + 
  geom_point(size = 2) + 
  geom_smooth(method='lm') +
  labs(y = "Yield Differential", x = "Precip",
       title = "Soy")

p2 <- ggplot(df_soy_2000, aes(x = EDD, y = irr_to_nonirr_diff)) + 
  geom_point(size = 2) + 
  geom_smooth(method='lm') +
  labs(x = "EDD", y = "Yield Differential")

ggsave(filename = '../figures/yield_differential_soy_raw.png',
       plot = p1 + p2 & scale_y_continuous(limits=c(0, 75)))

# OLS model
yield_diff_soy_model_freq <- lm(irr_to_nonirr_diff ~ EDD + prcp,
                                data = df_soy_2000)
summary(yield_diff_soy_model_freq)

#####################
# Bayesian model
#####################
yield_diff_soy_model <- stan_glm(irr_to_nonirr_diff ~ EDD + prcp,
                                 data = df_soy_2000,
                                 family = gaussian(),
                                 chains = 3, iter = 10000*2, 
                                 cores = 3, seed = 84735)

prior_summary(yield_diff_soy_model)

# Effective sample size ratio and Rhat
neff_ratio(yield_diff_soy_model)
rhat(yield_diff_soy_model)

# Trace & density plots of parallel chains
mcmc_trace(yield_diff_soy_model, size = 0.1)
mcmc_dens_overlay(yield_diff_soy_model)

# Posterior predictive checks
p <- pp_check(yield_diff_soy_model, nreps = 1000) + 
  labs(x="Yield Differential", y="Density",
       title = "Soy",
       subtitle = "1000 Posterior Predictive Draws") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

ggsave('../figures/yield_differential_soy_bayes_fit1.png',
       plot = p,
       width = 12, height = 6, units="in")

# Sampling for posterior predictive checks as function of predictors
sampling <- seq(1, nrow(df_soy_2000), 10)
yrep <- posterior_predict(yield_diff_soy_model, draws=1000)

p1 <- ppc_intervals(
  y = df_soy_2000$irr_to_nonirr_diff[sampling],
  yrep = yrep[,sampling],
  x = df_soy_2000$prcp[sampling],
  prob = 0.90, prob_outer=0.99) +
  labs(x = "Precip", y = "Yield Differential",
       title = "Soy",
       subtitle = "90, 99% posterior predictive intervals") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

p2 <- ppc_intervals(
  y = df_soy_2000$irr_to_nonirr_diff[sampling],
  yrep = yrep[,sampling],
  x = df_soy_2000$EDD[sampling],
  prob = 0.90, prob_outer=0.99) +
  labs(x = "EDD") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

ggsave('../figures/yield_differential_soy_bayes_fit2.png',
       plot = p1 & theme(legend.position = "none") | p2,
       width = 12, height = 6, units="in")

#######################################
# Approximate parameter posteriors
#######################################
yield_diff_soy_model_posterior <- as.data.frame(yield_diff_soy_model)
colnames(yield_diff_soy_model_posterior)[1] <- "intercept"

# Precip param
prcp_fit <- fitdistr(yield_diff_soy_model_posterior$prcp,
                     densfun = "normal")

p_prcp <- ggplot() + 
  geom_density(data = yield_diff_soy_model_posterior,
               aes(x = prcp, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_soy_model_posterior$prcp),
                                        max(yield_diff_soy_model_posterior$prcp))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = prcp_fit$estimate['mean'],
                            sd = prcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Precip Coefficient", y="",
       subtitle=paste("Normal(", signif(prcp_fit$estimate['mean'], 3),
                      ",", signif(prcp_fit$estimate['sd'], 3), ")", sep="")) 

# EDD param
edd_fit <- fitdistr(yield_diff_soy_model_posterior$EDD,
                    densfun = "normal")

p_edd <- ggplot() + 
  geom_density(data = yield_diff_soy_model_posterior,
               aes(x = EDD, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_soy_model_posterior$EDD),
                                        max(yield_diff_soy_model_posterior$EDD))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = edd_fit$estimate['mean'],
                            sd = edd_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="EDD Coefficient", y="",
       subtitle=paste("Normal(", signif(edd_fit$estimate['mean'], 3),
                      ",", signif(edd_fit$estimate['sd'], 3), ")", sep="")) 

# Intercept
intcp_fit <- fitdistr(yield_diff_soy_model_posterior$intercept,
                      densfun = "normal")

p_intcp <- ggplot() + 
  geom_density(data = yield_diff_soy_model_posterior,
               aes(x = intercept, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_soy_model_posterior$intercept),
                                        max(yield_diff_soy_model_posterior$intercept))),
                aes(x = x),
                fun = dnorm,
                args = list(mean = intcp_fit$estimate['mean'],
                            sd = intcp_fit$estimate['sd']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Intercept", y="",
       subtitle=paste("Normal(", signif(intcp_fit$estimate['mean'], 3),
                      ",", signif(intcp_fit$estimate['sd'], 3), ")", sep=""))

# Sigma
sigma_fit <- fitdistr(yield_diff_soy_model_posterior$sigma,
                      densfun = "gamma")

p_sigma <- ggplot() + 
  geom_density(data = yield_diff_soy_model_posterior,
               aes(x = sigma, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(yield_diff_soy_model_posterior$sigma),
                                        max(yield_diff_soy_model_posterior$sigma))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = sigma_fit$estimate['shape'],
                            rate = sigma_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Sigma", y="",
       subtitle=paste("Gamma(", signif(sigma_fit$estimate['shape'], 3),
                      ",", signif(sigma_fit$estimate['rate'], 3), ")", sep=""))

ggsave('../figures/yield_differential_soy_posterior.png',
       plot = (p_prcp | p_edd) / (p_intcp | p_sigma),
       width = 12, height = 6, units="in")
