mu_est <- function(data) {
  n <- ncol(data)
  return(matrix(apply(data, 2, mean), ncol = 1, nrow = n))
}

sigma_est <- function(data) {
  n <- ncol(data)
  t <- nrow(data)
  cor <- (t - 1) / t
  return(matrix(cor * cov(data), ncol = n, nrow = n))
}

onevec_est <- function(n) {
  return(matrix(1, nrow = n, ncol = 1))
}

r_est <- function(sigma) {
  n <- ncol(sigma)
  num <- solve(sigma) %*% onevec_est(n) %*% t(onevec_est(n)) %*% solve(sigma)
  denom <- c(t(onevec_est(n)) %*% solve(sigma) %*% onevec_est(n))
  return(solve(sigma) - (num / denom))
}

b_func <- function(x, a, b) {
  integrand <- function(y) {
    (y^(a - 1)) * ((1 - y)^(b - 1))
  }
  return(integrate(integrand, lower = 0, upper = x)$value)
}

omega_eqw_est <- function(n) {
  return(rep(1 / n, n))
}

omega_g_est <- function(sigma) {
  n <- ncol(sigma)
  num <- solve(sigma) %*% onevec_est(n)
  denom <- c(t(onevec_est(n)) %*% solve(sigma) %*% onevec_est(n))
  out <- num / denom
  return(out)
}

omega_h_est <- function(gamma, mu, sigma) {
  out <- (1 / gamma) * r_est(sigma) %*% mu
  return(out)
}

omega_s_est <- function(gamma, mu, sigma) {
  return((1 / gamma) * solve(sigma) %*% mu)
}

omega_g_tilde_est <- function(n, sigma) {
  return(solve(sigma) %*% onevec_est(n))
}

omega_h_tilde_est <- function(gamma, n, sigma, mu) {
  mu_g <- c(t(omega_g(sigma)) %*% mu)
  return((1 / gamma) * solve(sigma) %*% (mu - mu_g * onevec_est(n)))
}

utility_est <- function(gamma, mu, sigma, omega) {
  ret <- c(t(omega) %*% mu)
  var <- t(omega) %*% sigma %*% omega
  eu <- ret - (gamma / 2) * var
  return(eu)
}

sharpe_ratio_est <- function(gamma, mu, sigma, omega) {
  ret <- c(t(omega) %*% mu)
  var <- c(t(omega) %*% sigma %*% omega)
  sr <- ret / sqrt(var)
  return(sr)
}

sigma2_h_est <- function(n, t, theta2_h) {
  part_a <- ((t - n - 2) * theta2_h - n) / t
  part_b <- 2 * (theta2_h^(n / 2)) * ((1 + theta2_h)^(-(t - 2) / 2))
  part_c <- t * b_func(theta2_h / (1 + theta2_h), n / 2, (t - n) / 2)
  return(part_a + part_b / part_c)
}

eu_est <- function(gamma, vec) {
  ret <- mean(vec)
  var <- var(vec)
  return(ret - (gamma / 2) * var)
}

eu_est_2 <- function(w, mu, sigma) {
  eu <- w%*%mu - (gamma/2)*t(w)%*%sigma%*%w
  return(eu %>% as.numeric())
}

simulate_wcopula <- function(num, data){
  require(copula)
  require(gofCopula)
  require(MASS)
  
  #data <- matrix(data)
  ncol <- ncol(data)
  nrow <- nrow(data)
  mean_vec <- apply(data, 2, mean)
  sd_vec <- apply(data, 2, sd)
  mean_mat <- matrix(rep(mean_vec, each = num), ncol = ncol)
  sd_mat <- matrix(rep(sd_vec, each = num), ncol = ncol)

  data_pseudo <- pobs(data)
  fit_copula_data <- fitCopula(tCopula(dim = ncol, dispstr = 'un'), data_pseudo, method = 'ml')    
  sim_copula <- rCopula(n = num, copula = fit_copula_data@copula)
  sim_data_norm <- qnorm(sim_copula)
  sim_data <- (sim_data_norm * sd_mat) + mean_mat
  return(sim_data)
}
