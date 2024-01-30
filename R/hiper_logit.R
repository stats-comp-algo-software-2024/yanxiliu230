#' @export
hiper_glm <- function(design, outcome) {
  # TODO: actually fit the data and output something other than the list
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}
