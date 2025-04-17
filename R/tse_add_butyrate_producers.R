#' Use list of pre specified genera and species to calculate the total abudance of butyrate
#' producers
#'
#' @param
#'
#' @return
#'
#' @export
tse_add_butyrate_producers <- function(.data,
                                       label = "butyrates",
                                       transformation = "relabundance",
                                       c_butyrate_genera =  c("Alistipes_A_871400",
                                                              "Alistipes_A_871404",
                                                              "Anaerostipes",
                                                              "Anaerobutyricum",
                                                              "Agathobacter_164117",
                                                              "Agathobacter_164119",
                                                              "Butyricicoccus_A_77030",
                                                              "Butyricicoccus_A_77419",
                                                              "Butyricimonas",
                                                              "Butyrivibrio_A_168226",
                                                              "Butyrivibrio_A_180067",
                                                              "Coprococcus_A_121497",
                                                              "Coprococcus_A_187866",
                                                              "Eubacterium_B",
                                                              "Eubacterium_C",
                                                              "Eubacterium_F",
                                                              "Eubacterium_G",
                                                              "Eubacterium_I",
                                                              "Eubacterium_J",
                                                              "Eubacterium_M",
                                                              "Eubacterium_N",
                                                              "Eubacterium_O_258268",
                                                              "Eubacterium_O_258270",
                                                              "Eubacterium_Q",
                                                              "Eubacterium_R",
                                                              "Eubacterium_S",
                                                              "Eubacterium_T",
                                                              "Faecalibacterium",
                                                              "Flavonifractor",
                                                              "Gemmiger_A_73129",
                                                              "Gemmiger_A_73276",
                                                              "Odoribacter_865974",
                                                              "Oscillibacter",
                                                              "Pseudoflavonifractor_81068",
                                                              "Roseburia",
                                                              "Shuttleworthia"),
                                       c_butyrate_species = c("Gemmiger_A_73129 variabilis",
                                                              "Eubacterium_G ventriosum"),
                                       ...) {

  selected_taxa <- SummarizedExperiment::rowData(.data)$Genus %in% c_butyrate_genera |
                                                      SummarizedExperiment::rowData(.data)$Species %in% c_butyrate_species

  grouping <- ifelse(selected_taxa, label, rownames(.data)) %>% as.factor

  tse_merged <- mia::mergeFeatures(.data, f = grouping)
  SummarizedExperiment::rowData(tse_merged)[label, colnames(SummarizedExperiment::rowData(.data))] <- NA
  tse_transformed <- mia::transformAssay(tse_merged, method = transformation, ...)

  new_data <- tse_transformed[label, ] %>%
    mia::meltAssay(assay_name = transformation) %>%
    dplyr::select(SampleID, FeatureID, {{transformation}}) %>%
    tidyr::spread(FeatureID, {{transformation}}) %>%
    dplyr::full_join(tse_meta(.data), ., by = dplyr::join_by(rownames == SampleID)) %>%
    tibble::column_to_rownames(var = "rownames") %>%
    S4Vectors::DataFrame()

  SummarizedExperiment::colData(.data) <- new_data
  .data
}
