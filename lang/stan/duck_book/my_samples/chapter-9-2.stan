
data {
	int N; // = 32
	int D; // = 2
	vector[D] Y[N];
}

parameters {
	vector[D] mn;
	cov_matrix[D] cov;
}

model {
	//for (n in 1:N)
	//	Y[n] ~ multi_normal(mn, cov);
	Y ~ multi_normal(mn, cov);
}