// TODO constrain_stationary() の real 対応

#include arma_funs.stan

data {
  int<lower=1> K;
  int<lower=1> T;
  real y[T];
}

parameters {
  // State model
  simplex[K] pi1;  // レジーム確率の初期値
  simplex[K] P[K]; // K x K の推移確率行列

  // Observation model
  vector[K] mu;
  vector<lower=-1, upper=1>[K] beta; // AR 係数

  // ボラティリティ順にレジームを並べる
  positive_ordered[K] sigma;
}

transformed parameters {
  // 対数レジーム確率 (フィルタ化確率) の時系列
  vector[K] log_pi[T];
  vector[K] accumulator;

  log_pi[1] = log(pi1) + normal_lpdf(y[1] | mu, sigma);

  for (t in 2:T) {
    // j = current (t)
    for (j in 1:K) {
      // i = previous (t-1)
      for (i in 1:K) {
        accumulator[i] =
          log_pi[t-1, i] +
          log(P[i, j]) +
          normal_lpdf(y[t] | mu[j] + beta[j] * y[t-1], sigma[j]);
      }
      // 対数尤度を一旦、指数関数で元に戻した上で合算し、再度対数化
      log_pi[t, j] = log_sum_exp(accumulator);
    }
  }
}

model {
  mu ~ normal(0, 0.05); // リターンの切片が 0 より大きく異なることは稀
  beta ~ normal(0, 2);
  sigma ~ cauchy(0, 5);

  // 対数尤度を累積しているので、最終時点 T のみを評価すればよい
  target += log_sum_exp(log_pi[T]);
}

generated quantities {
  vector[K] log_xi[T];
  vector[K] log_gamma[T];

  vector[K] pi[T];
  vector[K] xi[T];
  vector[K] gamma[T];

  int<lower=1, upper=K> zstar[T];
  real logp_zstar;

  /* レジーム確率 */
  { // Forward algorithm
    for (t in 1:T)
      // 対数尤度を合計 1 の確率ベクトルに変換する
      pi[t] = softmax(log_pi[t]);
  }

  /* 平滑化確率 */
  { // Backward algorithm
    real accumulator2[K];

    // 初期値 (=最新のデータ)
    for (j in 1:K)
      log_xi[T, j] = 1;

    // 新->旧の順に計算していく
    for (tforward in 0:(T-2)) {
      int t = T - tforward;

      /* レジーム確率の逆のアルゴリズム
         t-1 の状態から t の状態への遷移確率ではなく、
         t の状態で t-1 を更新していく */

      // j = previous (t-1)
      for (j in 1:K) {
        // i = next (t)
        for (i in 1:K) {
          // backwards t + transition prob + local evidence at t
          accumulator2[i] = log_xi[t, i] + log(P[j, i]) + normal_lpdf(y[t] | mu[i], sigma[i]);
        }
        log_xi[t-1, j] = log_sum_exp(accumulator2);
      }
    }
    for (t in 1:T)
      xi[t] = softmax(log_xi[t]);
  }

  { // Forwards-backwards algorithm
    for(t in 1:T)
      log_gamma[t] = pi[t] .* xi[t];

    for(t in 1:T)
      gamma[t] = normalize(log_gamma[t]);
  }

  /* MAP 推定量 */
  { // Viterbi algorithm
    // backpointer to the most likely previous state on the most probable path
    int bpointer[T, K];

    // max prob for the seq up to t with final output from state k for time t
    real delta[T, K];

    // 初期値
    for (j in 1:K)
      delta[1, K] = normal_lpdf(y[1] | mu[j], sigma[j]);

    for (t in 2:T) {
      // j = current (t)
      for (j in 1:K) {
        delta[t, j] = negative_infinity();
        // i = previous (t-1)
        for (i in 1:K) {
          real logp;
          logp = delta[t-1, i] + log(P[i, j]) + normal_lpdf(y[t] | mu[j], sigma[j]);
          if (logp > delta[t, j]) {
            bpointer[t, j] = i;
            delta[t, j] = logp;
          }
        }
      }
    }

    logp_zstar = max(delta[T]);

    for (j in 1:K)
      if (delta[T, j] == logp_zstar)
        zstar[T] = j;

    for (t in 1:(T - 1)) {
      zstar[T - t] = bpointer[T - t + 1, zstar[T - t + 1]];
    }
  }
}
