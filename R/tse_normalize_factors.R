#'
#'
#' @param
#'
#' @return
#'
#' @export
tse_normalize_factors <- function(tse, na_level = NA, threshold = 10) {
  tse %>%
    tse_mutate(dplyr::across(where(~ is.character(.x) | is.integer(.x)),
                             ~if(dplyr::n_distinct(.x) <= threshold) as.factor(.x) else .x)) %>%
    tse_mutate(dplyr::across(is.factor, ~forcats::fct_na_value_to_level(.x, level = na_level) %>% forcats::fct_drop()))
}
