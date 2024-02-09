test_that("compare log likelihood numeric gradient with analytical one", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design; outcome <- data$outcome
  beta <- matrix(colMeans(design))

  approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
    numerical_grad <- rep(0, length(x))
    for (i in seq_along(x)) {
      zero_vector <- rep(0, length(x))
      zero_vector[i] <- dx
      numerical_grad[i] <- (func(x + zero_vector) - func(x - zero_vector)) / (2*dx)
    }
    return(numerical_grad)
  }

  approx_gradient <- approx_grad(function(x) log_likelihood(x, design, outcome), beta)
  analytic_gradient <- log_likelihood_gradient(beta, design, outcome)

  expect_true(are_all_close(
    approx_gradient, analytic_gradient, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
