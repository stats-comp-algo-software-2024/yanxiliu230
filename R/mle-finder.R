#' log likelihood function for linear model
#'
#' @details This function calculate the log likelihood function of linear model simplified to only contain parts related to parameter (betas/coefficients)
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
    residual <- outcome - design %*% par
    -t(residual) %*% residual
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
#' @examples log_likelihood_gradient(par, design, outcome)
#'
log_likelihood_gradient <- function(par, design, outcome, noise_var = 1) {
    c(2 * t(design) %*% outcome - 2 * t(design) %*% design %*% par)
}


#' linear model mle finder, by pseudoinverse method
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
    c(solve(t(design) %*% design, t(design) %*% outcome))
}


#' linear model mle finder, by optimization method
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
    optim_out <- stats::optim(fn = func, par = colMeans(design),
                              design = design, outcome = outcome,
                              gr = grad,
                              method = option$mle_solver, control = list(fnscale = -1))
    optim_out$par
}
