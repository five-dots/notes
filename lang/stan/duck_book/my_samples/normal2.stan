
data {
	int N;
	real X[N];
	read Y[N];
	int N_new;
	real X_new[N_new];
}

parameters {
	real a;
	real b;
	real<lower=0> sigma;
}

// data block, parameters block にある変数から新たにパラメタを作り出す
transformed_parameters {
	real y_base[N];
	for (n in 1:N) {
		y_base[n] = a + b*X[n];	
	}
}

model {
	for (n in 1:N) {
		// Y[n] ~ normal(a + b*X[n], sigma);
		Y[n] ~ normal(y_base[n], sigma);
	}
}

// data, parameters, generated parameters の変数から新たなパラメタを作成
// 事後確率からは独立なので、早い
generated_quantities {
	real y_base_new[N_new];
	real y_new[N_new];
	for (n in 1:N_new) {
		y_base_new[n] = a + b*X_new[n];
		y_new[n] = nornal_rng(y_base_new[n], sigma);
	}
}

