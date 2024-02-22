test_that('return TRUE since linear model log likelihood numeric gradient approximation should match analytical gradient', {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
  design <- data$design
  outcome <- data$outcome
  beta <- matrix(rep(0, n_pred))

  approx_gradient <- approx_grad(function(x) log_likelihood_linear(x, design, outcome), beta)
  analytic_gradient <- log_likelihood_linear_gradient(beta, design, outcome)

  expect_true(are_all_close(
    approx_gradient, analytic_gradient, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that('return TRUE since logistic model log likelihood numeric gradient approximation should match analytical gradient', {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design
  outcome <- data$outcome
  beta <- matrix(rep(0, n_pred))

  approx_gradient <- approx_grad(function(x) log_likelihood_logit(x, design, outcome), beta)
  analytic_gradient <- log_likelihood_logit_gradient(beta, design, outcome)

  expect_true(are_all_close(
    approx_gradient, analytic_gradient, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})

test_that('return TRUE since logistic regression log likelihood numeric Hessian approximation should match analytical Hessian', {
  n_obs <- 32
  n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design
  outcome <- data$outcome
  beta <- matrix(rep(0, n_pred))

  approx_hessian <- approx_hessian(function(x) log_likelihood_logit_gradient(x, design, outcome), matrix(0, n_pred, n_pred))
  analytic_hessian <- log_likelihood_logit_hessian(beta, design, outcome)

  expect_true(are_all_close(
    approx_hessian, analytic_hessian, abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
