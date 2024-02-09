#' @export
hiper_glm <- function(design, outcome, model="linear", option = NULL) {

  hglm_out <- list()
  coef <- c()

  # find mle using methods in input option
  if (is.null(option)) coef <- mle_finder_inv(design, outcome)
  else if (!is.null(option$mle_solver)) coef <- mle_finder_optim(design, outcome, log_likelihood, log_likelihood_gradient, option)

  hglm_out <- structure(list(coef = coef), class = "hglm")
  return(hglm_out)
}

#' log likelihood function (simplified) for linear model
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#' @param noise_var optional error parameter
#'
#' @export
log_likelihood <- function(par, design, outcome, noise_var = 1) {
  -t(outcome - design %*% par) %*% (outcome - design %*% par)
}

#' gradient of log likelihood function for linear model, taking derivative over beta
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#' @param noise_var optional error parameter
#'
#' @export
log_likelihood_gradient <- function(par, design, outcome, noise_var = 1) {
  c(2*t(design) %*% outcome - 2 * t(design) %*% design %*% par)
}

#' linear model mle finder using pseudoinverse method
#'
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @export
mle_finder_inv <- function(design, outcome) {
  c(MASS::ginv(t(design) %*% design) %*% t(design) %*% outcome)
}

#' linear model mle finder using optimization method
#'
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @export
mle_finder_optim <- function(design, outcome, func, grad, option) {
  optim_out <- stats::optim(fn = func,
                            par = matrix(colMeans(design)),
                            design = design,
                            outcome = outcome,
                            gr = grad,
                            method = option$mle_solver, control=list(fnscale=-1))
  c(optim_out$par)
}
