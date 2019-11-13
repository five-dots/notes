data {
  int<lower=0> N;
  vector[N] y;
}

parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}

model {
  /* for (i in 2:N) { */
  /*   y[i] ~ normal(alpha + beta * y[i-1], sigma); */
  /* } */
  /* tail(y, N-1) ~ normal(alpha + beta * head(y, N-1), sigma); */
  y[2:N] ~ normal(alpha + beta * y[1:(N-1)], sigma);
}
