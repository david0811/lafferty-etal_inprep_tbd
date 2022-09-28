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

# USDA irrigation infrastructure costs
df <- read_csv('../input_data/usda_irrigation_expansion_cost_2013-2018.csv')

##################################################
# Model
##################################################
# Exploratory plots
p1 <- ggplot(df, aes(x = cost_per_acre)) + 
  geom_histogram(bins=20) +
  labs(x = "cost per acre ($)", y="")

ggsave(filename = '../figures/irrigation_infrastructure_cost_raw.png',
       plot = p1)

# Bayesian model
irr_inf_model_stan <- "
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
generated quantities {
  real y_rep[N];
  for (n in 1:N) {
    y_rep[n] = gamma_rng(alpha, beta);
    }
}
"

irr_inf_cost_data <- list(y = df$cost_per_acre, 
                          N = length(df$cost_per_acre))

irr_inf_cost_model <- stan(model_code = irr_inf_model_stan,
                           data = irr_inf_cost_data, 
                           chains = 3, iter = 10000*2,
                           cores = 3, seed = 84735)

irr_inf_cost_model

irr_inf_cost_model <- stan_glm(log(cost_per_acre) ~ 1,
                               data = df,
                               family = gaussian,
                               chains = 3, iter = 10000*2, 
                               cores = 3, seed = 84735)

prior_summary(irr_inf_cost_model)

# Effective sample size ratio and Rhat
neff_ratio(irr_inf_cost_model)
rhat(irr_inf_cost_model)

# Trace & density plots of parallel chains
mcmc_trace(irr_inf_cost_model, size = 0.1, pars=vars('alpha', 'beta'))
mcmc_dens_overlay(irr_inf_cost_model, pars=vars('alpha', 'beta'))

# Posterior predictive checks
y_rep <- as.matrix(irr_inf_cost_model, pars = "y_rep")
p1 <- ppc_dens_overlay(df$cost_per_acre, y_rep[1:1000, ]) +
  labs(x="Cost per acre ($)", y="Density",
       subtitle = "1000 Posterior Predictive Draws") +
  panel_bg(fill = "gray90", color = NA) +
  grid_lines(color = "white")

ggsave('../figures/irrigation_infrastructure_cost_bayes_fit.png',
       plot = p1,
       width = 12, height = 6, units="in")

# Approximate parameter posterior
irr_inf_cost_model_posterior <- as.data.frame(irr_inf_cost_model)

# Alpha
alpha_fit <- fitdistr(irr_inf_cost_model_posterior$alpha,
                    densfun = "gamma")

p_alpha <- ggplot() + 
  geom_density(data = irr_inf_cost_model_posterior,
               aes(x = alpha, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(irr_inf_cost_model_posterior$alpha),
                                        max(irr_inf_cost_model_posterior$alpha))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = alpha_fit$estimate['shape'],
                            rate = alpha_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="Mean", y="",
       subtitle=paste("Gamma(", signif(alpha_fit$estimate['shape'], 3),
                      ",", signif(alpha_fit$estimate['rate'], 3), ")", sep="")) 

# beta
beta_fit <- fitdistr(irr_inf_cost_model_posterior$beta,
                      densfun = "gamma")

p_beta <- ggplot() + 
  geom_density(data = irr_inf_cost_model_posterior,
               aes(x = beta, after_stat(density)),
               colour="red", size=1) +
  geom_function(data = data.frame(x = c(min(irr_inf_cost_model_posterior$beta),
                                        max(irr_inf_cost_model_posterior$beta))),
                aes(x = x),
                fun = dgamma,
                args = list(shape = beta_fit$estimate['shape'],
                            rate = beta_fit$estimate['rate']),
                colour="black", lty="dashed", size=1) + 
  labs(x="beta", y="",
       subtitle=paste("Gamma(", signif(beta_fit$estimate['shape'], 3),
                      ",", signif(beta_fit$estimate['rate'], 3), ")", sep=""))

ggsave('../figures/irrigation_infrastructure_cost_bayes_posterior.png',
       plot = p_alpha | p_beta,
       width = 12, height = 6, units="in")
