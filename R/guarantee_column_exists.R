#'
#'
#' @param
#'
#' @return
#'
#' @export
guarantee_column_exists <-  function(.data, cols, default_value = NA) {
  .data[setdiff(cols, names(.data))] <- NA
}
