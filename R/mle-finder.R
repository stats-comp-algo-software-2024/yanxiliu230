#' log likelihood function
#'
#' log likelihood function for linear model, simplified
#'
#' @details This function calculate the log likelihood function of linear model simplified to only contain parts related to betas
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#' @param noise_var optional error parameter
#'
#' @return log likelihood function
#'
#' @export
#'
#' @examples log_likelihood(par, design, outcome)
#'
log_likelihood <- function(par, design, outcome, noise_var = 1) {
  -t(outcome - design %*% par) %*% (outcome - design %*% par)
}

#' gradient of log likelihood function
#'
#' gradient of log likelihood function for linear model, taking derivative over beta
#'
#' @details This function calculate the gradient of log likelihood function of linear model with respect to betas
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
#' @examples log_likelihood_gradient(par, design, outcome)
#'
log_likelihood_gradient <- function(par, design, outcome, noise_var = 1) {
  c(2*t(design) %*% outcome - 2 * t(design) %*% design %*% par)
}


#' approximate gradient of functions
#'
#' approximate gradient of functions using finite difference method
#'
#' @details This function approximate gradient of functions using finite difference method, when we do not know the exact formula for the gradient
#'
#' @param func function to take gradient of
#' @param x where the function to take gradient of
#' @param dx delta x
#'
#' @return gradient of function at input x
#'
#' @export
#'
#' @examples approx_grad(function(x) log_likelihood(x, design, outcome), beta)
#'
approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for (i in seq_along(x)) {
    zero_vector <- rep(0, length(x))
    zero_vector[i] <- dx
    numerical_grad[i] <- (func(x + zero_vector) - func(x - zero_vector)) / (2*dx)
  }
  return(numerical_grad)
}


#' linear model mle
#'
#' linear model mle finder using pseudoinverse method
#'
#' @details This function calculate mle of linear model using pseudoinverse
#'
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @return mle of linear model
#'
#' @export
#'
#' @examples mle_finder_inv(design, outcome)
#'
mle_finder_inv <- function(design, outcome) {
  c(MASS::ginv(t(design) %*% design) %*% t(design) %*% outcome)
}

#' linear model mle
#'
#' linear model mle finder using optimization method
#'
#' @details This function calculate mle of linear model using optimization method with a pre-specified solver
#'
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @return mle of linear model, approximated
#'
#' @export
#'
#' @examples mle_finder_optim(design, outcome, func, grad, option = list(mle_solver = 'BFGS'))
mle_finder_optim <- function(design, outcome, func, grad, option) {
  optim_out <- stats::optim(fn = func,
                            par = matrix(colMeans(design)),
                            design = design,
                            outcome = outcome,
                            gr = grad,
                            method = option$mle_solver, control=list(fnscale=-1))
  c(optim_out$par)
}
