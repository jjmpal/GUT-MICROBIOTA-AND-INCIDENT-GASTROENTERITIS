#'
#'
#' @param
#'
#' @return
#'
#' @export
tse_clinical_subset <- function(x, rank, prevalence, detection) {
  x <- mia::subsetByPrevalentFeatures(x, rank = rank, detection = detection, prevalence =prevalence, as_relative = TRUE)
  y <- mia::subsetByPrevalentFeatures(x, rank = rank, detection = detection, prevalence = 1-prevalence, as_relative = TRUE)

  keep <- setdiff(rownames(x), rownames(y))

  x[keep,]
}
