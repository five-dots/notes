data {
  int<lower=1> T;
  real y[T];
}

parameters {
  real mu;
  real<lower=-1, upper=1> phi;
  real<lower=-1, upper=1> theta;
  real<lower=0> sigma;
}

model {
  // 時点 t での予測値
  vector[T] nu;
  // 時点 t での誤差 (あとで残差チェックに使える)
  vector[T] err;
  
  // err[0] == 0と仮定
  nu[1] = mu + (phi * mu);
  err[1] = y[1] - nu[1];

  for (t in 2:T) {
    nu[t] = mu + (phi * y[t-1]) + (theta * err[t-1]);
    err[t] = y[t] - nu[t];
  }

  mu    ~ normal(0, 10);
  phi   ~ normal(0, 2);
  theta ~ normal(0, 2);
  sigma ~ cauchy(0, 5);
  err   ~ normal(0, sigma);    // 尤度
}

// 局所変数のベクトルを利用しない例
// model {
//   real err;
  
//   mu    ~ normal(0, 10);
//   phi   ~ normal(0, 2);
//   theta ~ normal(0, 2);
//   sigma ~ cauchy(0, 5);

//   err = y[1] - mu + (phi * mu);
//   err ~ normal(0, sigma);

//   for (t in 2:T) {
//     err = y[t] - (mu + phi * y[t-1] + theta * err);
//     err ~ normal(0, sigma);
//   }
// }