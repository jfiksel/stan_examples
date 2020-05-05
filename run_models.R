### to install rstan: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
### stan user manual: https://mc-stan.org/docs/2_23/stan-users-guide/index.html
### stan forums: https://discourse.mc-stan.org/

library(rstan)
library(tidyverse)
library(ggmcmc)
library(brms)

rstan_options(auto_write = TRUE)
### below code is optional, will run chains in parallel
options(mc.cores = 4)


###### normal-normal hierarhchial model
### read about different parameterizations here: https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html

### Rule 1 of using Stan: always make sure you can recover parameters from simulated data!
### 3 groups
J <- 3
mu <- c(-1, 0, 1)
sigma <- rep(1, J)
### 1000 observations
N <- 1000
group <- sample(1:J, replace = TRUE, N)

y <- rnorm(N, mu[group], sigma[group])

hierarchical_data <- list(N = N, J = J, group = group, y = y)
hierarch_simulated_fit <- stan(data = hierarchical_data, file = 'normal_hierarchical.stan')

### get group means
mu_posterior <- ggs(hierarch_simulated_fit, family = "mu_group")
ggs_traceplot(mu_posterior)
ggs_caterpillar(mu_posterior)
ci(mu_posterior)

### do this with real data
hw_data <- read_table2('hw1-1.txt', skip = 1, col_names = FALSE)
colnames(hw_data) <- c('group', 'y')
N <- nrow(hw_data)
group <- hw_data$group
J <- max(group)
y <- hw_data$y

hw1_data <- list(N = N, J = J, group = group, y = y)
hw1_fit <- stan(data = hw1_data, file = 'normal_hierarchical.stan')
posteriors <- ggs(hw1_fit)
ggs_caterpillar(posteriors)
hw1_ci <- ci(posteriors, thick_ci = c(.025, .975)) %>%
    select(Parameter, low, median, high)
write_csv(hw1_ci, "hw1_ci.csv")

##### Mixture model
### following example here: https://mc-stan.org/users/documentation/case-studies/identifying_mixture_models.html
mu <- c(-2.75, 2.75)
sigma <- c(1, 1)
lambda <- 0.4
set.seed(689934)

N <- 1000
z <- rbinom(N, 1, lambda) + 1
y <- rnorm(N, mu[z], sigma[z])
plot(density(y))

input_data <- list(N = N, y = y)
degenerate_fit <- stan(file='gauss_mix.stan', data=input_data,
                       chains=4, seed=483892929)
### show label switching
mu_samples <- ggs(degenerate_fit, family = "mu")
ggs_pairs(mu_samples)
ggs_traceplot(mu_samples)
ggs_density(mu_samples)


### now impose ordering on the group means
ordered_fit <- stan(file='gauss_mix_ordered_prior.stan', data=input_data,
                    chains=4, seed=483892929, refresh=2000)
mu_samples <- ggs(ordered_fit, family = "mu")
### make sure we don't see label switching
ggs_pairs(mu_samples)
ggs_traceplot(mu_samples)
ggs_density(mu_samples)
ci(mu_samples)

############# Regression with brms
### for those not comfortable writing your own Stan code (or if you're pressed for time)
### data and model taken from here: https://jrnold.github.io/bayesian_notes/introduction-to-stan-and-linear-regression.html
data("Duncan", package = "carData")
duncan_lm <- lm(prestige ~ type + income + education, data = Duncan)
summary(duncan_lm)

### now do bayesian model
### You should be more careful with your priors--this is just an example of how to us brms!
brms_model <- brm(data = Duncan, family = gaussian,
                  prestige ~ 1 + type + income + education,
                  prior = c(prior(normal(0, 10), class = Intercept),
                            prior(normal(0, 10), class = b),
                            prior(uniform(0, 50), class = sigma)),
                  iter = 2000, warmup = 1000, chains = 4, cores = 4,
                  seed = 42323423)
### can see the generated Stan code
brms_model$model
### can get summary of fit
brms_model$fit
posterior_samples <- ggs(brms_model$fit)
ggs_traceplot(posterior_samples) + facet_wrap(~Parameter, scales = 'free_y')
ggs_caterpillar(posterior_samples)
ci(posterior_samples)
