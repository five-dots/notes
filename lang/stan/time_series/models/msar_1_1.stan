#include arma_funs.stan

data {
  int<lower=1> T;
  real r[T];
}

parameters {
  vector<lower=0, upper=1>[2] p;
  vector[2] alpha;
  // vector<lower=-1, upper=1>[2] beta;
  real<lower=0> beta1;
  real<upper=0> beta2;
  vector<lower=0>[2] sigma;

  real<lower=0, upper=1> xi1_init; // 状態 1 のフィルター確率の初期値
  real r_tm1_init;
}

transformed parameters {
  matrix[T, 2] eta; // 2 状態分の尤度
  matrix[T, 2] xi;  // 状態毎のフィルター化確率
  vector[T] f;
  real p11;
  real p12;
  real p21;
  real p22;

  // 観測方程式 (状態毎の尤度)
  for (t in 1:T) {
    if (t == 1) {
      eta[t, 1] = exp(normal_lpdf(r[t]| alpha[1] + beta1 * r_tm1_init, sigma[1]));
      eta[t, 2] = exp(normal_lpdf(r[t]| alpha[2] + beta2 * r_tm1_init, sigma[2]));
    } else {
      eta[t, 1] = exp(normal_lpdf(r[t]| alpha[1] + beta1 * r[t-1], sigma[1]));
      eta[t, 2] = exp(normal_lpdf(r[t]| alpha[2] + beta2 * r[t-1], sigma[2]));
    }
  }

  // 観測方程式を元に、状態の確率毎に重み付け
  for (t in 1:T) {
    if (t == 1) {
      p11 = p[1]       * xi1_init       * eta[t, 1];
      p12 = (1 - p[1]) * xi1_init       * eta[t, 2];
      p22 = p[2]       * (1 - xi1_init) * eta[t, 2];
      p21 = (1 - p[2]) * (1 - xi1_init) * eta[t, 1];
    } else {
      p11 = p[1]       * xi[t-1, 1] * eta[t, 1];
      p12 = (1 - p[1]) * xi[t-1, 1] * eta[t, 2];
      p22 = p[2]       * xi[t-1, 2] * eta[t, 2];
      p21 = (1 - p[2]) * xi[t-1, 2] * eta[t, 1];
    }

    f[t] = p11 + p12 + p22 + p21;
    xi[t, 1] = (p11 + p12) / f[t];
    xi[t, 2] = 1.0 - xi[t, 1];
  }
}

model {
  // Priors
  p ~ beta(10, 2);
  // beta ~ normal(0, 2);
  beta1 ~ normal(0, 2);
  beta2 ~ normal(0, 2);
  sigma ~ cauchy(0, 5);
  xi1_init ~ beta(2, 2);
  r_tm1_init ~ normal(0, 0.1);

  target += sum(log(f));
  // for (t in 2:T) {
  //   r[t] ~ normal(alpha + constrain_stationary(beta) * r[t-1], sigma);
  // }
}