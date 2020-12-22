// generated with brms 2.13.5
functions {
  /* cumulative-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     real p;
     if (y == 1) {
       p = inv_logit(disc * (thres[1] - mu));
     } else if (y == nthres + 1) {
       p = 1 - inv_logit(disc * (thres[nthres] - mu));
     } else {
       p = inv_logit(disc * (thres[y] - mu)) -
           inv_logit(disc * (thres[y - 1] - mu));
     }
     return log(p);
   }
  /* cumulative-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
     return cumulative_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
  /* ordered-logistic log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real ordered_logistic_merged_lpmf(int y, real mu, vector thres, int[] j) {
     return ordered_logistic_lpmf(y | mu, thres[j[1]:j[2]]);
   }
}
data {
  int<lower=1> N;  // number of observations
  int Y[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> K_disc;  // number of population-level effects
  matrix[N, K_disc] X_disc;  // population-level design matrix
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  int<lower=1> J_1[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  int<lower=1> J_2[N];  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  vector[N] Z_2_2;
  vector[N] Z_2_3;
  vector[N] Z_2_4;
  vector[N] Z_2_disc_5;
  vector[N] Z_2_disc_6;
  vector[N] Z_2_disc_7;
  vector[N] Z_2_disc_8;
  int<lower=1> NC_2;  // number of group-level correlations
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K;
  matrix[N, Kc] Xc;  // centered version of X
  vector[Kc] means_X;  // column means of X before centering
  int Kc_disc = K_disc - 1;
  matrix[N, Kc_disc] Xc_disc;  // centered version of X_disc without an intercept
  vector[Kc_disc] means_X_disc;  // column means of X_disc before centering
  for (i in 1:K) {
    means_X[i] = mean(X[, i]);
    Xc[, i] = X[, i] - means_X[i];
  }
  for (i in 2:K_disc) {
    means_X_disc[i - 1] = mean(X_disc[, i]);
    Xc_disc[, i - 1] = X_disc[, i] - means_X_disc[i - 1];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  ordered[nthres] Intercept;  // temporary thresholds for centered predictors
  vector[Kc_disc] b_disc;  // population-level effects
  real Intercept_disc;  // temporary intercept for centered predictors
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  vector[N_1] z_1[M_1];  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  matrix[M_2, N_2] z_2;  // standardized group-level effects
  cholesky_factor_corr[M_2] L_2;  // cholesky factor of correlation matrix
}
transformed parameters {
  vector[N_1] r_1_1;  // actual group-level effects
  matrix[N_2, M_2] r_2;  // actual group-level effects
  // using vectors speeds up indexing in loops
  vector[N_2] r_2_1;
  vector[N_2] r_2_2;
  vector[N_2] r_2_3;
  vector[N_2] r_2_4;
  vector[N_2] r_2_disc_5;
  vector[N_2] r_2_disc_6;
  vector[N_2] r_2_disc_7;
  vector[N_2] r_2_disc_8;
  r_1_1 = (sd_1[1] * (z_1[1]));
  // compute actual group-level effects
  r_2 = (diag_pre_multiply(sd_2, L_2) * z_2)';
  r_2_1 = r_2[, 1];
  r_2_2 = r_2[, 2];
  r_2_3 = r_2[, 3];
  r_2_4 = r_2[, 4];
  r_2_disc_5 = r_2[, 5];
  r_2_disc_6 = r_2[, 6];
  r_2_disc_7 = r_2[, 7];
  r_2_disc_8 = r_2[, 8];
}
model {
  // initialize linear predictor term
  vector[N] mu = Xc * b;
  // initialize linear predictor term
  vector[N] disc = Intercept_disc + Xc_disc * b_disc;
  for (n in 1:N) {
    // add more terms to the linear predictor
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n] + r_2_2[J_2[n]] * Z_2_2[n] + r_2_3[J_2[n]] * Z_2_3[n] + r_2_4[J_2[n]] * Z_2_4[n];
  }
  for (n in 1:N) {
    // add more terms to the linear predictor
    disc[n] += r_2_disc_5[J_2[n]] * Z_2_disc_5[n] + r_2_disc_6[J_2[n]] * Z_2_disc_6[n] + r_2_disc_7[J_2[n]] * Z_2_disc_7[n] + r_2_disc_8[J_2[n]] * Z_2_disc_8[n];
  }
  for (n in 1:N) {
    // apply the inverse link function
    disc[n] = exp(disc[n]);
  }
  // priors including all constants
  target += normal_lpdf(b[1] | 3.5,3);
  target += normal_lpdf(b[2] | 0, 1);
  target += normal_lpdf(b[3] | 0, 1);
  target += normal_lpdf(b[4] | 0, 1);
  target += normal_lpdf(b[5] | 1.5, 1);
  target += normal_lpdf(b[6] | 3.5,3);
  target += normal_lpdf(b[7] | 3.5,3);
  target += normal_lpdf(b[8] | 3.5,3);
  target += normal_lpdf(Intercept[1] | 7, 3);
  target += normal_lpdf(Intercept[2] | 8, 3);
  target += normal_lpdf(b_disc | 0, 1);
  target += normal_lpdf(Intercept_disc | -1.5, 1);
  target += normal_lpdf(sd_1[1] | 5, 2)
    - 1 * normal_lccdf(0 | 5, 2);
  target += std_normal_lpdf(z_1[1]);
  target += normal_lpdf(sd_2[1] | 7, 3)
    - 1 * normal_lccdf(0 | 7, 3);
  target += normal_lpdf(sd_2[2] | 7, 3)
    - 1 * normal_lccdf(0 | 7, 3);
  target += normal_lpdf(sd_2[3] | 7, 3)
    - 1 * normal_lccdf(0 | 7, 3);
  target += normal_lpdf(sd_2[4] | 7, 3)
    - 1 * normal_lccdf(0 | 7, 3);
  target += normal_lpdf(sd_2[5] | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += normal_lpdf(sd_2[6] | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += normal_lpdf(sd_2[7] | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += normal_lpdf(sd_2[8] | 0, 1)
    - 1 * normal_lccdf(0 | 0, 1);
  target += std_normal_lpdf(to_vector(z_2));
  target += lkj_corr_cholesky_lpdf(L_2 | 1);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += cumulative_logit_lpmf(Y[n] | mu[n], disc[n], Intercept);
    }
  }
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
  // actual population-level intercept
  real b_disc_Intercept = Intercept_disc - dot_product(means_X_disc, b_disc);
  // compute group-level correlations
  corr_matrix[M_2] Cor_2 = multiply_lower_tri_self_transpose(L_2);
  vector<lower=-1,upper=1>[NC_2] cor_2;
  // extract upper diagonal of correlation matrix
  for (k in 1:M_2) {
    for (j in 1:(k - 1)) {
      cor_2[choose(k - 1, 2) + j] = Cor_2[j, k];
    }
  }
}

