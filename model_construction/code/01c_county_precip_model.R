library(MASS)
library(tidyverse)
library(rstan)
library(bayesplot)
bayesplot_theme_update(text = element_text(size = 16, family = "sans"))
theme_update(text = element_text(size = 16, family = "sans"))
library(tidybayes)
library(broom.mixed)
library(patchwork)

##################################################
# Read all relevant data
##################################################
df <- read_csv('../input_data/gridmet_county_weather_variables_1979-2020.csv')

##################################################
# Model
##################################################
# Exploratory plots
p1 <- ggplot(df, aes(x = prcp)) + 
  geom_histogram(bins=30) +
  labs(x = "County precipitation (mm)", y="")

p2 <- ggplot(data = df) + 
  geom_boxplot(aes(x=year, y=prcp, group=year)) + 
  labs(y = "County precipitation (mm)", x="Year")

ggsave(filename = '../figures/county_precip_raw.png',
       plot = p1 | p2)

# Frequentist model
prcp_model_freq <- fitdistr(df$prcp, densfun = "gamma")
prcp_model_freq

# Bayesian model
prcp_model_stan <- "
data {
  int<lower=1> N;
  vector[N] y;    // vector allows vectorization
}
parameters {
  real<lower = 0> alpha;
  real<lower = 0> beta;
}
model {
  alpha ~ normal(0, 100);
  beta ~ normal(0, 100);
  y ~ gamma(alpha, beta);
}
// generated quantities {
//   real y_rep[1000];
//   for (n in 1:1000) {
//     y_rep[n] = gamma_rng(alpha, beta);
//     }
// }
"

prcp_data <- list(y = df$prcp, 
                  N = length(df$prcp))

prcp_model <- stan(model_code = prcp_model_stan,
                   data = prcp_data, 
                   chains = 3, iter = 10000*2,
                   cores = 3, seed = 84735)

prcp_model

# Effective sample size ratio and Rhat
neff_ratio(prcp_model)
rhat(prcp_model)

# Trace & density plots of parallel chains
mcmc_trace(prcp_model, size = 0.1, pars=vars('alpha', 'beta'))
mcmc_dens_overlay(prcp_model, pars=vars('alpha', 'beta'))

# Posterior predictive checks
prcp_model_posterior <- as.data.frame(prcp_model, pars = c('alpha','beta'))
y_rep <- matrix(nrow=1000, ncol=1000)
for (i in 1:1000) {
  y_rep[i,] <- rgamma(n=1000,
                      shape=prcp_model_posterior$alpha,
                      rate=prcp_model_posterior$beta)
}
y_rep <- pivot_longer(as.data.frame(y_rep), cols=everything())

p1 <- ggplot() + 
  geom_density(data=y_rep, aes(x=value, group=name), 
               color='cadetblue', alpha=0.2) +
  geom_density(data=df, aes(x=prcp), color='black') +
  labs(x="County Precip (mm)", y="Density",
       subtitle = "1000 Posterior Predictive Draws") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")
p1

ggsave('../figures/county_prcip_bayes_fit.png',
       plot = p1,
       width = 12, height = 6, units="in")

# Approximate parameter posterior
# alpha
alpha_fit <- fitdistr(prcp_model_posterior$alpha,
                      densfun = "gamma")

p_alpha <- ggplot() + 
  geom_density(data = prcp_model_posterior,
               aes(x = alpha, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(prcp_model_posterior$alpha),
                                        max(prcp_model_posterior$alpha))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = alpha_fit$estimate['shape'],
                            rate = alpha_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="alpha", y="",
       subtitle=paste("Gamma(", signif(alpha_fit$estimate['shape'], 3),
                      ",", signif(alpha_fit$estimate['rate'], 3), ")", sep=""))

# beta
beta_fit <- fitdistr(prcp_model_posterior$beta,
                     densfun = "gamma")

p_beta <- ggplot() + 
  geom_density(data = prcp_model_posterior,
               aes(x = beta, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(prcp_model_posterior$beta),
                                        max(prcp_model_posterior$beta))),
                aes(x = x),
                fun = dgamma,
                args = list(rate = beta_fit$estimate['rate'],
                            shape = beta_fit$estimate['shape']),
                colour="black", lty="dashed", size=1) + 
  labs(x="beta", y="",
       subtitle=paste("Gamma(", signif(beta_fit$estimate['shape'], 3),
                      ",", signif(beta_fit$estimate['rate'], 3), ")", sep="")) 

ggsave('../figures/county_precip_bayes_posterior.png',
       plot = p_alpha | p_beta,
       width = 12, height = 6, units="in")
