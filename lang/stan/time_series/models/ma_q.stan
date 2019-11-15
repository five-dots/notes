data {
  int<lower=0> Q;
  int<lower=3> T;
  vector[T] y;
}

parameters {
  real mu;
  real<lower=0> sigma;
  vector[Q] theta;
}

transformed parameters {
  vector[T] epsilon;
  for (t in 1:T) {
    epsilon[t] = y[t] - mu;
    for (q in 1:min(t - 1, Q))
      epsilon[t] = epsilon[t] - (theta[q] * epsilon[t - q]);
  }
}

model {
  vector[T] eta;
  mu ~ cauchy(0, 2.5);
  theta ~ cauchy(0, 2.5);
  sigma ~ cauchy(0, 2.5);

  for (t in 1:T) {
    eta[t] = mu;
    for (q in 1:min(t - 1, Q))
      eta[t] = eta[t] + theta[q] * epsilon[t - q];
  }

  y ~ normal(eta, sigma);
}
