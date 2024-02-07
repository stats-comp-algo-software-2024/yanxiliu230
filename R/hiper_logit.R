#' @export
hiper_glm <- function(design, outcome, model="linear", option = NULL, noise_var = 1) {

  hglm_out <- list()
  coef <- c()

  if (is.null(option)) {
    coef <- c(MASS::ginv(t(design) %*% design) %*% t(design) %*% outcome)
  } else if (!is.null(option$mle_solver)) {

    optim_out <- stats::optim(fn = log_likelihood,
                              par = matrix(rep(1, ncol(design))),
                              design = design, outcome = outcome,
                              gr = log_likelihood_gradient,
                              method = option$mle_solver, control=list(fnscale=-1))
    coef <- c(optim_out$par)
  }

  hglm_out <- structure(list(coef = coef), class = "hglm")
  return(hglm_out)
}

#' @export
log_likelihood <- function(par, design, outcome) {
  -t(outcome - design %*% par) %*% (outcome - design %*% par)
}

#' @export
log_likelihood_gradient <- function(par, design, outcome) {
  2*t(design) %*% outcome - 2 * t(design) %*% design %*% par
}
