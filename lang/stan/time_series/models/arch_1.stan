data {
  int<lower=0> T;
  real r[T];
}

parameters {
  real mu;
  // 条件付き分散が正であることを保証する制約
  real<lower=0> alpha0;
  // 条件付き分散が正であることを保証する制約 + 定常性を保証するための制約
  real<lower=0, upper=1> alpha1;
}

model {
  for (t in 2:T) {
    r[t] ~ normal(mu, sqrt(alpha0 + alpha1 * pow(r[t-1] - mu, 2)));
  }
}
