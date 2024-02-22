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
#' @examples mle_finder_linear_inv(design, outcome)
#'
mle_finder_linear_inv <- function(design, outcome) {
    c(solve(t(design) %*% design, t(design) %*% outcome))
}

#' finding mle by optimization
#'
#' @details This function calculate mle of given likelihood (and score) function using optimization method with a pre-specified solver
#'
#' @param design design matrix
#' @param outcome outcome vector
#'
#' @return mle of input likelihood (and score), approximated
#'
#' @export
#'
#' @examples mle_finder_optim(design, outcome, func, grad, option = list(mle_solver = 'BFGS'))
#'
mle_finder_optim <- function(design, outcome, func, grad, option) {
    optim_out <- stats::optim(fn = func, par = rep(0, ncol(design)),
                              design = design, outcome = outcome,
                              gr = grad,
                              method = option$mle_solver, control = list(fnscale = -1))
    optim_out$par
}

#' finding mle of logit by Newton Raphson
#'
#' @details This function calculate mle of logistic regression of Bernoulli response variable using Newton Raphson
#'
#' @param design design matrix
#' @param outcome outcome vector
#' @param epsilon convergence threshold for log likelihood between two consecutive iterations
#'
#' @return mle coefficient of logit
#'
#' @export
#'
#' @examples mle_finder_logit_nr(design, outcome)
#'
mle_finder_logit_nr <- function(design, outcome, epsilon = 1e-8) {
    n_pred <- ncol(design)
    beta_0 <- rep(0, n_pred)
    beta_1 <- beta_0

    log_like_null <- log_likelihood_logit(beta_0, design, outcome)
    log_like_0 <- log_like_null
    log_like_1 <- log_like_null

    iterations_done <- 0

    while (iterations_done < 100) {
        first_derivative <- log_likelihood_logit_gradient(beta_0, design, outcome)
        second_derivative <- log_likelihood_logit_hessian(beta_0, design, outcome)

        beta_1 <- beta_0 - solve(second_derivative, first_derivative)

        log_like_1 <- log_likelihood_logit(beta_1, design, outcome)

        if (are_close(log_like_1, log_like_0, abs_tol = epsilon, rel_tol = epsilon)) {
            break
        } else {
            beta_0 <- beta_1
            log_like_0 <- log_like_1
            iterations_done <- iterations_done + 1
        }
    }
    c(beta_1)
}
