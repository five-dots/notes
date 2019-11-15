data {
  int<lower=3> T;
  vector[T] y;
}

parameters {
  real mu;
  real<lower=0> sigma;
  vector[2] theta;
}

transformed parameters {
  vector[T] epsilon;
  epsilon[1] = y[1] - mu;
  epsilon[2] = y[2] - mu - (theta[1] * epsilon[1]);

  // 観測値 - 平均 - MA 項を引いたものが当期の誤差項
  for (t in 3:T)
    epsilon[t] = (y[t] - mu
                  - theta[1] * epsilon[t - 1]
                  - theta[2] * epsilon[t - 2]);
}

model {
  // 事前分布にコーシー分布を指定
  mu    ~ cauchy(0, 2.5);
  theta ~ cauchy(0, 2.5);
  sigma ~ cauchy(0, 2.5);

  for (t in 3:T)
    y[t] ~ normal(mu
                  + theta[1] * epsilon[t - 1]
                  + theta[2] * epsilon[t - 2],
                  sigma);
}
