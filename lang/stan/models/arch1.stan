data {
  int<lower=0> T;
  real r[T];
}

parameters {
  real mu;
  real<lower=0> alpha0;
  real<lower=0, upper=1> alpha1;
}

model {
  for (t in 2:T) {
    r[t] ~ normal(mu, sqrt(alpha0 + alpha1 * pow(r[t-1] - mu, 2)));
  }
}
