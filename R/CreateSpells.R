#' 'CreateSpells'
#'
#'
#'CreateSpells takes as input a dataset with multiple time windows per unit of observation. Multiple categories of time windows may be recorded per unit, and time windows of the same unit may overlap, even within the same category. The purpose of the function is creating a dataset where the time windows of the each person and category are disjoint (a time window which is disjoint form the others is called spell). Additionally, a category  '_overall' is added, where time windows are processed regardless of their category. As an option, overlap of pairs of categories are also processed: each pair will be associated to spells where both values are recorded.
#'
#' @param dataset name of dataset
#' @param id variable containing the identifier of the unit of observation
#' @param start_date variable containing the start date (the date must me ordered as Year Month Day)
#' @param end_date variable containing the end date (the date must me ordered as Year Month Day)
#' @param category (optional) categorical variable
#' @param replace_missing_end_date (optional). When specified, it contains a date to replace end_date when it is missing.
#' @param overlap (optional) default FALSE. If TRUE, overlaps of pairs of categories are processed as well.
#' @param dataset_overlap (optional) if overlap TRUE, the name of the file containing the overlap dataset
#' @param only_overlaps (optional) if only_overlaps TRUE, skip the calculation the spells
#' @param gap_allowed (optional) Allowed gap in days between two observation periods after which they are counted as a different spell
#' @importFrom data.table :=

# NOTE: Developed under R  4.3.0


CreateSpells <- function(dataset, id, start_date, end_date, category = NULL, replace_missing_end_date = NULL,
                         overlap = F, dataset_overlap = "df_overlap", only_overlaps = F, gap_allowed = 1,
                         birth_date = NULL, gap_allowed_birth = 1){

  ..start_date <- ..end_date <- row_id <- .N <- lag_end_date <- num_spell <- ..id <- . <- "Shut up!"

  pass_all_arguments("sanitize_inputs")
  dataset <- data_preparation(dataset, start_date, end_date, replace_missing_end_date)

  if (!only_overlaps) {

    dataset <- data_preparation_2(dataset, category)
    dataset <- data_preparation_3(dataset, birth_date, gap_allowed_birth)

    dataset <- CreateSpells.internal(dataset, id, start_date, end_date, category, gap_allowed)
    assign("output_spells_category", dataset)
  }

  #OPTIONAL SECTION REGARDING OVERLAPS

  if(overlap || only_overlaps){
    dataset <- sanitize_inputs_overlap(dataset, id, start_date, end_date, category, gap_allowed)

    export_df <- overlap.internal(dataset, id, start_date, end_date, category, gap_allowed)
    assign(dataset_overlap, export_df, envir = parent.frame())
  }

  if(!only_overlaps) return(output_spells_category)
}
