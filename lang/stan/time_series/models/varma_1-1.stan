data {
	int<lower=1> T; // num observations
	int<lower=1> N; // num series
	vector[N] y[T]; // observed outputs
	int<lower=0> T_forecast; // forecasting span
}

parameters {
	vector[N] mu;        // mean coeffs
	matrix[N,N] Psi;     // autoregression coeff matrix
	matrix[N,N] Theta;   // moving avg coeff matrix
	cov_matrix[N] Sigma; // noise scale matrix
}

transformed parameters {
  vector[N] eps[T]; // error terms

  eps[1] = y[1] -mu;
  for (t in 2:T) {
    eps[t] = y[t] - (mu + Psi * y[t-1] + Theta * eps[t-1]);
  }
}

model {
	/* priors  */
	mu ~ normal(0,10);
	to_vector(Psi) ~ normal(0,2);
	to_vector(Theta) ~ normal(0,2);
	Sigma ~ inv_wishart(N, N*diag_matrix(rep_vector(1,N))); // LKJ 相関分布が Stan 推奨

	/* likelihood */
	for (t in 2:T){
	  y[t] ~ multi_normal(mu + Psi * y[t-1] + Theta * eps[t-1], Sigma);
	}
}

/* prediction */
generated quantities{
  vector[N] y_pred[T_forecast];
  vector[N] eps_pred[T_forecast];

  eps_pred[1] = multi_normal_rng(rep_vector(0,N), Sigma);
  y_pred[1] = mu + Psi * y[T] + Theta * eps[T] + eps_pred[1];

  for(t in 2:T_forecast) {
    eps_pred[t] = multi_normal_rng(rep_vector(0,N), Sigma);
    y_pred[t] = mu + Psi * y_pred[t-1] + Theta * eps_pred[t-1] + eps_pred[t];
  }
}
