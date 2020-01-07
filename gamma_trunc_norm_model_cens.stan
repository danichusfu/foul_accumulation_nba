//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int < lower = 0 > N;      // Number of observations not censored
  int < lower = 0 > N_cens; // Number of censored observations
  int < lower = 0 > J;      // Number of player foul level combinations
  int < lower = 0 > K;      // Number of groups for the prior i.e. (foul levels, positions, foul levels per position, player)
  int < lower = 0 > P;     //
  real y[N];           //
  real y_cens[N_cens];
  int K_J[K + 1];
  int seq[J + 1];
  int seq_cens[J + 1];
  vector[2] a_b_mu[K];
  matrix[2, 2] a_b_sigma[K];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  matrix < lower = 0 > [J, 2] alpha_beta; // 
}

transformed parameters {
  ordered [6] alpha_over_beta[P];

  for(m in 1:P){
    for(l in 0:5){
      alpha_over_beta[m, l + 1] = alpha_beta[J - (l * P) + m - P, 1]/alpha_beta[J - (l * P) + m - P, 2];
    }
  }
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  for(k in 1:K){
    for (j in K_J[k]:K_J[k + 1]){
      alpha_beta[j] ~ multi_normal(a_b_mu[k], a_b_sigma[k]);
      y[(seq[j] + 1):seq[j + 1]] ~ gamma(alpha_beta[j, 1], alpha_beta[j, 2]);
      target += gamma_lccdf(y_cens[(seq_cens[j] + 1):seq_cens[j + 1]] | alpha_beta[j, 1], alpha_beta[j, 2]);
    }
  }
}

generated quantities{
  real < lower = 0 > y_new[J];
  for (j in 1:J){
    y_new[j] = gamma_rng(alpha_beta[j, 1], alpha_beta[j, 2]);
    }
}

