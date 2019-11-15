data {
	int<lower=1> T;
	int<lower=0> P;
	int<lower=0> Q;
	vector[T] y;
	int<lower=0> T_forecast;
}

parameters {
	real mu;
	vector[P] phi;
	vector[Q] theta;
	real<lower=0> sigma;
}

transformed parameters {
  // error term
  vector[T] eps;
  eps[1] = y[1] - mu;

  for (t in 2:T) {
    eps[t] = y[t] - mu;

    for(p in 1:min(t-1, P))
      eps[t] -= phi[p] * y[t-p];

    for(q in 1:min(t-1, Q))
      eps[t] -= theta[q] * eps[t-q];
  }
}

model {
  vector[T] eta;

  // priors
	mu ~ normal(0, 5);
  phi ~ normal(0, 1);
  theta ~ normal(0, 1);
  sigma ~ cauchy(0, 5); // TODO half-t or half-norm

	// log-likelihood
	for (t in 1:T) {
	  eta[t] = mu;

    // AR terms
	  for (p in 1:min(t-1, P))
	    eta[t] += phi[p] * y[t-p];

    // MA terms
	  for (q in 1:min(t-1, Q))
	    eta[t] += theta[q] * eps[t-q];

	  y[t] ~ normal(eta[t], sigma);
	}
}

generated quantities {
  vector[T+T_forecast] y_pred;
  vector[T+T_forecast] eps_pred;
  
  vector[T] log_lik;
  vector[T] eta;

  // prediction
  eps_pred[1:T] = eps;
  y_pred[1:T] = y;

  for (t in (T+1):(T+T_forecast)) {
    eps_pred[t] = normal_rng(0, sigma);
    y_pred[t] = mu + eps_pred[t];

    // AR terms
    for (p in 1:P)
	    y_pred[t] += phi[p] * y_pred[t-p];

    // MA terms
	  for (q in 1:Q)
	    y_pred[t] += theta[q] * eps_pred[t-q];
  }

  // log-likelihood for WAIC calculation by {loo}
  for (t in 1:T) {
    eta[t] = mu;

    // AR terms
	  for (p in 1:min(t-1, P))
	    eta[t] += phi[p] * y[t-p];

    // MA terms
	  for (q in 1:min(t-1, Q))
	    eta[t] += theta[q] * eps[t-q];

    log_lik[t] = normal_lpdf(y[t] | eta[t], sigma);
  }
}