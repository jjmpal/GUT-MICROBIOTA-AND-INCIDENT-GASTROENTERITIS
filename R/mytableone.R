#'
#'
#' @param
#'
#' @return
#'
#' @export
mytableone <- function(x, vars, fo = ~., ...) {
  x <- if (inherits(x, "TreeSummarizedExperiment")) tse_meta(x, rownames = FALSE) else x
  purrr::iwalk(vars, ~ data.table::setattr(x[[.y]], "label", .x))
  return(table1::table1(fo, x, ...))
}
