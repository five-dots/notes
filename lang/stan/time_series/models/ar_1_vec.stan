data {
  int<lower=0> T;
  vector[T] y;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

model {
  y[2:T] ~ normal(alpha + beta * y[1:(T-1)], sigma);
}
