functions {
  vector normalize(vector x) {
    return x / sum(x);
  }
}

data {
  int<lower=1> T; // number of observations (length)
  int<lower=1> K; // number of hidden states
  real y[T];      // observations
}

parameters {
  // Discrete state model
  simplex[K] pi1;  // initial state probabilities
  simplex[K] A[K]; // transition probabilities
                   // A[i][j] = p(z_t = j | z_{t-1} = i)

  // Continuous observation model
  ordered[K] mu;          // observation means
  real<lower=0> sigma[K]; // observation standard deviations
}

transformed parameters {
  vector[K] logalpha[T];

  { // log p(z_t = j | x_{1:t})
    real accumulator[K];

    // t=1 の対数尤度
    // 確率 x 尤度の積ではなく、対数の和にしている
    logalpha[1] = log(pi1) + normal_lpdf(y[1] | mu, sigma);

    // t>1 の対数尤度
    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
         // Murphy (2012) Eq. 17.48
         // belief state + transition prob + local evidence at t
          accumulator[i] = logalpha[t-1, i] + log(A[i, j]) + normal_lpdf(y[t] | mu[j], sigma[j]);
        }
        logalpha[t, j] = log_sum_exp(accumulator);
      }
    }
  }
}

model {
  // Note: update based only on last logalpha
  target += log_sum_exp(logalpha[T]);
}

generated quantities {
  vector[K] logbeta[T];
  vector[K] loggamma[T];

  vector[K] alpha[T];
  vector[K] beta[T];
  vector[K] gamma[T];

  int<lower=1, upper=K> zstar[T];
  real logp_zstar;

  { // Forward algortihm
    // 人間が理解しやすいように、合計 1 の確率ベクトルに変換する
    for (t in 1:T)
      alpha[t] = softmax(logalpha[t]);
  }

  { // Backward algorithm log p(x_{t+1:T} | z_t = j)
    real accumulator[K];

    // 初期値 (=最新のデータ)
    for (j in 1:K)
      logbeta[T, j] = 1;

    // 最後から decrement していく
    // T=100 であれば、100, 99, 98, ..., 2 まで
    for (tforward in 0:(T-2)) {
      int t;
      t = T - tforward;

      // フィルタ化確率の逆のアルゴリズム
      // t-1 の状態から t の状態への遷移確率ではなく、
      // t の状態で t-1 を更新していく
      for (j in 1:K) { // j = previous (t-1)
        for (i in 1:K) { // i = next (t)
          // Murphy (2012) Eq. 17.58
          // backwards t + transition prob + local evidence at t
          accumulator[i] = logbeta[t, i] + log(A[j, i]) + normal_lpdf(y[t] | mu[i], sigma[i]);
        }
        logbeta[t-1, j] = log_sum_exp(accumulator);
      }
    }

    for (t in 1:T)
      beta[t] = softmax(logbeta[t]);
  }

  { // Forwards-backwards algorithm log p(z_t = j | x_{1:T})
    for(t in 1:T)
      loggamma[t] = alpha[t] .* beta[t];

    for(t in 1:T)
      gamma[t] = normalize(loggamma[t]);
  }

  { // Viterbi algorithm

    // backpointer to the most likely previous state on the most probable path
    int bpointer[T, K];

    // max prob for the seq up to t with final output from state k for time t
    real delta[T, K];

    // 初期値
    for (j in 1:K)
      delta[1, K] = normal_lpdf(y[1] | mu[j], sigma[j]);

    for (t in 2:T) {
      for (j in 1:K) { // j = current (t)
        delta[t, j] = negative_infinity();
        for (i in 1:K) { // i = previous (t-1)
          real logp;
          logp = delta[t-1, i] + log(A[i, j]) + normal_lpdf(y[t] | mu[j], sigma[j]);
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