data {
  int<lower=0> N; // number of observations (never hardcode this!)
  int<lower=0> J; // number of groups
  vector[N] y; // observed outcome
  int<lower=0, upper=J> group[N]; // observed group
}
parameters {
  vector[J] mu_group;
  real mu_overall;
  real<lower=0> sigma; // parameterized as standard deviation, not variance!
  real<lower=0> sigma_mu;
}
model {
  // vectorized code
  y ~ normal(mu_group[group], sigma);
  // can also write it like this:
  //for(i in 1:N) {
  //  y[i] ~ normal(mu_group[group[i]], sigma);
  //}
  mu_group ~ normal(mu_overall, sigma_mu);
  // Uninformative prior on mu_overall
  mu_overall ~ normal(0, 5);
  // half-cauchy prior on sigma_mu
  sigma_mu ~ cauchy(0, 5);
  // inv-gamma prior on sigma
  sigma ~ inv_gamma(.1,.1);
}
