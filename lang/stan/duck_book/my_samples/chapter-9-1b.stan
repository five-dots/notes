
data {
	int N;
	vector[N] Y;
	vector[N] X;
}

parameters {
	real a;
	real b;
	real<lower=0> sigma;
}

transformed parameters {
	vector[N] mu;
	mu = a + b * X;
}

model {
	Y ~ normal(mu, sigma);
}