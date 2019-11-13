data {
  int<lower=0> N;
  int<lower=0> P;
  real y[N];
}

parameters {
  real alpha;
  real beta[P];
  real<lower=0> sigma;
}

model {
  for (n in (P+1):N) {
    real mu;
    mu <- alpha;
    for (p in 1:P) {
      mu <- mu + beta[p] * y[n-p];
    }
    y[n] ~ normal(mu, sigma);
  }
}
