test_that('return TRUE since log likelihood numeric gradient approximation should match analytical gradient', {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design
  outcome <- data$outcome
  beta <- matrix(colMeans(design))

  approx_gradient <- approx_grad(function(x) log_likelihood(x, design, outcome), beta)
  analytic_gradient <- log_likelihood_gradient(beta, design, outcome)

  expect_true(are_all_close(
    approx_gradient, analytic_gradient, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
