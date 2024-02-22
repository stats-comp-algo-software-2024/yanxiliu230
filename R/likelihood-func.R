#' log likelihood function for linear model
#'
#' @details This function calculate the log likelihood function of linear model simplified to only contain parts related to parameter (betas/coefficients)
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#' @param noise_var optional error parameter
#'
#' @return log likelihood function for linear model
#'
#' @export
#'
#' @examples log_likelihood_linear(par, design, outcome)
#'
log_likelihood_linear <- function(par, design, outcome, noise_var = 1) {
  residual <- outcome - design %*% par
  -t(residual) %*% residual / (2 * noise_var)
}

#' gradient of log likelihood function for linear model, taking derivative over beta
#'
#' @details This function calculate the gradient of log likelihood function of linear model with respect to betas (betas/coefficients)
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#' @param noise_var optional error parameter
#'
#' @return gradient of log likelihood function of linear model with with respect to betas
#'
#' @export
#'
#' @examples log_likelihood_linear_gradient(par, design, outcome)
#'
log_likelihood_linear_gradient <- function(par, design, outcome, noise_var = 1) {
  (t(design) %*% outcome - t(design) %*% design %*% par) / noise_var
}

#' log likelihood function for logistic model
#'
#' @details This function calculate the log likelihood function of logistic model simplified to only contain parts related to parameter (betas/coefficients)
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @return log likelihood function for logistic model
#'
#' @export
#'
#' @examples log_likelihood_logit(par, design, outcome)
#'
log_likelihood_logit <- function(par, design, outcome) {
  outcome %*% design %*% par - sum(log(1 + exp(design %*% par)))
}

#' gradient of log likelihood function for logistic model, taking derivative over beta
#'
#' @details This function calculate the gradient of log likelihood function of logistic model with respect to betas (betas/coefficients)
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @return gradient of log likelihood function of logistic model with with respect to betas
#'
#' @export
#'
#' @examples log_likelihood_logit_gradient(par, design, outcome)
#'
log_likelihood_logit_gradient <- function(par, design, outcome) {
  eta <- design %*% par
  pi <- exp(eta) / (1 + exp(eta))
  t(design) %*% (outcome - pi)
}

#' Hessian of log likelihood function for logistic model, taking double derivatives over beta
#'
#' @details This function calculate the Hessian of log likelihood function of logistic model with respect to betas
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @return Hessian of log likelihood function of logistic model with with respect to betas
#'
#' @export
#'
#' @examples log_likelihood_logit_hessian(par, design, outcome)
#'
log_likelihood_logit_hessian <- function(par, design, outcome) {
  n_obs <- length(outcome)
  eta <- design %*% par
  pi <- exp(eta) / (1 + exp(eta))
  W <- diag(c(pi) * (1 - c(pi)), nrow = n_obs)
  - t(design) %*% W %*% design
}
