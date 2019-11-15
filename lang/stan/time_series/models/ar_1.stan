data {
  int<lower=0> T;
  real y[T];
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

model {
  for (t in 2:T) {
    y[t] ~ normal(alpha + beta * y[t-1], sigma);
  }
}
