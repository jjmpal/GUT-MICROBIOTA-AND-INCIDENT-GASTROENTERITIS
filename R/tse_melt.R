#'
#'
#' @param
#'
#' @return
#'
#' @export
tse_melt <- function(x, method = "clr", prefix = "GUT_") {
  mia::transformAssay(x, method = method, pseudocount = 1) %>%
    mia::meltAssay(add_row_data = TRUE, assay_name = method) %>%
    dplyr::mutate(FeatureID = glue::glue("{prefix}{FeatureID}")) %>%
    dplyr::mutate(FeatureID = stringr::str_replace_all(FeatureID, c(" " = "_", "-" = "_"))) %>%
    dplyr::select(SampleID, FeatureID, clr) %>%
    tidyr::spread(FeatureID, clr) %>%
    dplyr::full_join(tse_meta(x), by = dplyr::join_by(SampleID == rownames))
}
