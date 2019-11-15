data {
  int<lower=0> T;
  int<lower=0> P;
  real y[T];
}

parameters {
  real alpha;
  real beta[P];
  real<lower=0> sigma;
}

model {
  for (t in (P+1):T) {
    real mu;
    mu = alpha;
    for (p in 1:P) {
      mu = mu + beta[p] * y[t-p];
    }
    y[t] ~ normal(mu, sigma);
  }
}