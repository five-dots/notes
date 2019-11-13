
data {
	int N;
	real X[n];
	real Y[N];
}

parameters {
	real b1;
	real b2;
}

transformed parameters {
	real mu[N];
	for (n in 1:N) {
		real[n] = b1 + b2 * X[n];
	}
}

model {
	for (n in 1:N) {
		Y[n] ~ Normal(mu[n], sigma)	
	}
}