data {
  int<lower=1> N;
  vector[N] x;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  x ~ normal(mu, sigma);
}