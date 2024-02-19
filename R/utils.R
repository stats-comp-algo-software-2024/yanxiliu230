#' Test two numeric values are close
#'
#' @details This function check if two numeric value are close in terms of absolute and relative difference
#'
#' @param v first numeric value
#' @param w second numeric value
#' @param abs_tol absolute tolerance
#' @param rel_tol relative tolerance
#'
#' @return TRUE if the difference between the two numeric values does not exceed absolute and relative tolerance, FALSE otherwise
#'
#' @export
#'
#' @examples are_close(1, 1.01)
#'
are_close <- function(v, w, abs_tol = 1e-6, rel_tol = 1e-6) {
  abs_diff <- abs(v - w)
  is_within_atol <- abs_diff < abs_tol
  is_within_rtol <- (abs_diff < rel_tol) * pmax(abs(v), abs(w))
  return(is_within_atol && is_within_rtol)
}
