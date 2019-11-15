data {
	int<lower=1> T; // num observations
	int<lower=1> N; // num series
	int<lower=0> p; // AR(p)
	int<lower=0> q; // MA(q)
	vector[N] y[T]; // observed outputs
	int<lower=0> T_forecast; // forecasting span
}

parameters {
	vector[N] mu;         // mean coeffs
	matrix[N,N] Psi[p];   // autoregression coeff matrix
	matrix[N,N] Theta[q]; // moving avg coeff matrix
	cov_matrix[N] Sigma;  // noise scale matrix
}

transformed parameters {
  vector[N] eps[T]; // error terms

  eps[1] = y[1] -mu;
  for (t in 2:T) {
    eps[t] = y[t] - mu;

    for(i in 1:min(t-1, p)) {
      eps[t] = eps[t] - Psi[i] * y[t-i];
    }

    for( i in 1:min(t-1,q)) {
      eps[t] = eps[t] - Theta[i] * eps[t-i];
    }
  }
}

model {
  vector[N] eta[T];

  /* priors  */
	mu ~ normal(0,10) ;
	for(i in 1:p)
	  to_vector(Psi[i]) ~ normal(0,2);
  for(i in 1:q)
	  to_vector(Theta[i]) ~ normal(0,2);
	Sigma ~ inv_wishart(N, N * diag_matrix(rep_vector(1, N)));

	/* likelihood */
	for (t in 1:T) {
	  eta[t] = mu;
	  for (i in 1:min(t-1, p))
	    eta[t] = eta[t] + Psi[i] * y[t-i];
	  for (i in 1:min(t-1, q))
	    eta[t] = eta[t] + Theta[i] * eps[t-i];
	  y[t] ~ multi_normal(eta[t], Sigma);
	}
}

/* prediction */
generated quantities{
  vector[N] y_pred[T+T_forecast];
  vector[N] eps_pred[T+T_forecast];

  eps_pred[1:T] = eps;
  y_pred[1:T] = y;

  for (t in (T+1):(T+T_forecast)) {
    eps_pred[t] = multi_normal_rng(rep_vector(0,N), Sigma);
    y_pred[t] = mu + eps_pred[t];
    for (i in 1:p)
	    y_pred[t] = y_pred[t] + Psi[i] * y_pred[t-i];
	  for(i in 1:q)
	    y_pred[t] = y_pred[t] + Theta[i] * eps_pred[t-i];
  }
}
