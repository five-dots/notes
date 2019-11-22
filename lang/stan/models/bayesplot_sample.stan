data {
  int<lower=0> N;
  vector[N] X;
  vector[N] Y;
}

parameters {
  real mu;
  real beta;
  real<lower=0> sigma;
}

model {
  Y ~ normal(mu + beta * X, sigma);
}

generated quantities {
  vector[N] pred;
  for(n in 1:N)
    pred[n] = normal_rng(mu + beta * X[n], sigma);
}
