
data {
	int N;
	int<lower=0, upper=1> A[N];
	real<lower=0, upper=1> Score[N];
	real<lower=0, upper=1> Weather[N];
	int<lower=0, upper=1> Y[N];
}

parameters {
	real b[4];
}

model {
	for (n in 1:N) {
		Y[n] ~ bernoulli_logit(b[1] + b[2] * A[n] + b[3] * Score[n] + b[4] * Weather[n]);
	}
}
