#' fit GLM
#'
#' fit GLM of input design and outcome matrix
#'
#' @details This function is fit a GLM model using a use-input solver option, or pseudoinverse method if no solver option input
#'
#' @param par vector of parameter to estimate
#' @param design design matrix
#' @param outcome outcome vector
#' @param noise_var optional error parameter
#'
#' @return hiper glm class object containing coefficients of the model
#'
#' @export
#'
#' @examples hiper_glm(design, outcome, model = 'linear', option = list(mle_solver = 'BFGS')
#'
hiper_glm <- function(design, outcome, model = "linear", option = NULL) {

    hglm_out <- list()
    coef <- c()

    if (is.null(option)) coef <- mle_finder_inv(design, outcome)
    else if (option$mle_solver == "BFGS") coef <- mle_finder_optim(design, outcome, log_likelihood, log_likelihood_gradient, option)
    else if (option$mle_solver == "pseudoinverse") coef <- mle_finder_inv(design, outcome)

    hglm_out <- structure(list(coef = coef), class = "hglm")
    return(hglm_out)
}
