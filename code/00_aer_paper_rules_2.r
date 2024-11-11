omega_kz_est <- function(t, gamma, mu, sigma, theta_cor = FALSE) {

  n <- ncol(sigma)
  omega_g <- omega_g_est(sigma = sigma)
  omega_h <- omega_h_est(gamma = gamma, mu = mu, sigma = sigma)

  mu_g <- t(omega_g) %*% mu
  mu_h <- t(omega_h) %*% mu

  sigma2_g <- t(omega_g) %*% sigma %*% omega_g
  sigma2_h <- t(omega_h) %*% sigma %*% omega_h

  theta2_h <- (mu_h^2) / sigma2_h

  if(theta_cor == TRUE) {
  part_a <- ((t - n - 2) * theta2_h - n) / t
  part_b <- 2 * (theta2_h ^ (n / 2)) * ((1 + theta2_h) ^ ( - (t - 2) / 2))
  part_c <- t * b_func(theta2_h / (1 + theta2_h), n / 2, (t - n) / 2)
  theta2_h <-  part_a + part_b / part_c 
  }

  a_est_kz <- c(((t - n - 1) * (t - n - 4) / (t * (t - 2))) * (mu_g / sigma2_g))
  b_est_kz <- c(((t - n - 1) * (t - n - 4) / (t * (t - 2))) *
                  (theta2_h / (theta2_h + n / t)))

  out <- (1 / gamma) * a_est_kz * omega_g + b_est_kz * omega_h

  return(out)
}

omega_q_est <- function(t, gamma, mu, sigma, theta_cor = FALSE) {

  n <- ncol(sigma)
  omega_g <- omega_g_est(sigma = sigma)
  omega_h <- omega_h_est(gamma = gamma, mu = mu, sigma = sigma)

  mu_g <- t(omega_g) %*% mu
  mu_h <- t(omega_h) %*% mu

  sigma2_g <- t(omega_g) %*% sigma %*% omega_g
  sigma2_h <- t(omega_h) %*% sigma %*% omega_h

  theta2_h <- (mu_h^2) / sigma2_h

  if(theta_cor == TRUE) {
  part_a <- ((t - n - 2) * theta2_h - n) / t
  part_b <- 2 * (theta2_h ^ (n / 2)) * ((1 + theta2_h) ^ ( - (t - 2) / 2))
  part_c <- t * b_func(theta2_h / (1 + theta2_h), n / 2, (t - n) / 2)
  theta2_h <-  part_a + part_b / part_c 
  }

  a_est_q <- c(((t - n - 1) / (t - 2)) * (mu_g / sigma2_g))
  b_est_q <- c(((t - n) * (t - n - 3) / (t * (t - 2))) *
                 (theta2_h / (theta2_h + (n - 1) / t)))

  out <- (1 / gamma) * a_est_q * omega_g + b_est_q * omega_h
  return(out)
}

omega_m_est <- function(t, gamma, mu, sigma, theta_cor = FALSE) {

  n <- ncol(sigma)
  omega_g <- omega_g_est(sigma = sigma)
  omega_h <- omega_h_est(gamma = gamma, mu = mu, sigma = sigma)

  mu_g <- t(omega_g) %*% mu
  mu_h <- t(omega_h) %*% mu

  sigma2_h <- t(omega_h) %*% sigma %*% omega_h
  sigma2_g <- t(omega_g) %*% sigma %*% omega_g

  theta2_h <- (mu_h^2) / sigma2_h

  if(theta_cor == TRUE) {
  part_a <- ((t - n - 2) * theta2_h - n) / t
  part_b <- 2 * (theta2_h ^ (n / 2)) * ((1 + theta2_h) ^ ( - (t - 2) / 2))
  part_c <- t * b_func(theta2_h / (1 + theta2_h), n / 2, (t - n) / 2)
  theta2_h <-  part_a + part_b / part_c 
  }

  a_est_m <- c(((t - n - 1) * (t - n - 4) / (t * (t - 2))) * (mu_g / sigma2_g))
  b_est_m <- c(((t - n) * (t - n - 3) / (t * (t - 2))) *
                 (theta2_h / (theta2_h + (n - 1) / t)))

  out <- (1 / gamma) * a_est_m * omega_g + b_est_m * omega_h
  return(out)
}

omega_qs_est <- function(t, gamma, mu, sigma, theta_cor = FALSE) {

  n <- ncol(sigma)
  omega_g <- omega_g_est(sigma = sigma)
  omega_h <- omega_h_est(gamma = gamma, mu = mu, sigma = sigma)

  mu_g <- t(omega_g) %*% mu
  mu_h <- t(omega_h) %*% mu

  sigma2_g <- t(omega_g) %*% sigma %*% omega_g
  sigma2_h <- t(omega_h) %*% sigma %*% omega_h

  theta2_h <- (mu_h^2) / sigma2_h

  if(theta_cor == TRUE) {
  part_a <- ((t - n - 2) * theta2_h - n) / t
  part_b <- 2 * (theta2_h ^ (n / 2)) * ((1 + theta2_h) ^ ( - (t - 2) / 2))
  part_c <- t * b_func(theta2_h / (1 + theta2_h), n / 2, (t - n) / 2)
  theta2_h <-  part_a + part_b / part_c 
  }

  a_est_qs <- c(((t - n - 1) * (t - n - 4) / (t * (t - 2))) * (mu_g / sigma2_g))
  b_est_qs <- c(((t - n) * (t - n - 5) * (t - n - 7) / ((t^2) * (t - 2))) *
                  (theta2_h / (theta2_h + ((n - 1) / t))))

  out <- (1 / gamma) * a_est_qs * omega_g + b_est_qs * omega_h
  return(out)
}

omega_cbr_est <- function(t, gamma, mu, sigma, theta_cor = FALSE) {

  n <- ncol(sigma)
  omega_g <- omega_g_est(sigma = sigma)
  omega_h <- omega_h_est(gamma = gamma, mu = mu, sigma = sigma)

  mu_g <- t(omega_g) %*% mu
  mu_h <- t(omega_h) %*% mu

  sigma2_g <- t(omega_g) %*% sigma %*% omega_g
  sigma2_h <- t(omega_h) %*% sigma %*% omega_h

  theta2_h <- (mu_h^2) / sigma2_h

  if(theta_cor == TRUE) {
  part_a <- ((t - n - 2) * theta2_h - n) / t
  part_b <- 2 * (theta2_h ^ (n / 2)) * ((1 + theta2_h) ^ ( - (t - 2) / 2))
  part_c <- t * b_func(theta2_h / (1 + theta2_h), n / 2, (t - n) / 2)
  theta2_h <-  part_a + part_b / part_c 
  }
  
  a_est_cbr <- c(((t - n - 1) * (t - n - 4) / (t * (t - 2))) *
                   (mu_g / sigma2_g))
  b_est_cbr <- c((((t - n) * (t - n - 5) * (t - n - 7)) / ((t^2) * (t - 2))) *
                   (theta2_h / (theta2_h + (n / t))))

  out <- (1 / gamma) * a_est_cbr * omega_g  + b_est_cbr * omega_h
  return(out)
}
