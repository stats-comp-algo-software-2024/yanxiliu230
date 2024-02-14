#' @export
coef.hglm <- function(hglm_out) {
    # TODO: actually compute the coefficients
    print(hglm_out$coef)
}

#' @export
vcov.hglm <- function(hglm_out) {
    # TODO: actually compute the CI
    warning("confidence interval function not implemented")
}

#' @export
print.hglm <- function(hglm_out) {
    # TODO: print something useful
    warning("print function not fully operational yet")
    cat("\ncoef: ", hglm_out$coef)
}
