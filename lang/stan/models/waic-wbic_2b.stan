data {
  int<lower=1> N;
  vector[N] Y;
}

parameters {
  real mu;
  real<lower=0> s;
}

model {
  for(n in 1:N)
    target += 1/log(N) * normal_lpdf(Y[n] | mu, s);
}

generated quantities {
  vector[N] log_lik;
  for(n in 1:N)
    log_lik[n] = normal_lpdf(Y[n] | mu, s);
}