#' @export
coef.hglm <- function(hglm_out) {
  # TODO: actually compute the coefficients
  print(hglm_out$coef)
}

#' @export
vcov.hglm <- function(hglm_out) {
  # TODO: actually compute the CI
  print("confidence interval")
}

#' @export
print.hglm <- function(hglm_out) {
  # TODO: print something useful
  print("something")
}
