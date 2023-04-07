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

# NOTE: Developed under R  4.2.3


CreateSpells <- function(dataset, id, start_date, end_date, category = NULL, replace_missing_end_date = NULL,
                         overlap = F, dataset_overlap = "df_overlap", only_overlaps = F, gap_allowed = 1){

  ..start_date <- ..end_date <- row_id <- .N <- lag_end_date <- num_spell <- ..id <- . <- "Shut up!"

  pass_all_arguments("check_sanitize_inputs")

  if (!only_overlaps) {

    dataset <- data_preparation(dataset, id, start_date, end_date, category, replace_missing_end_date)

    if(is.null(category)){
      order_vec <- c(id, start_date, end_date)
      grouping_vars <- id
    } else {
      order_vec <- c(id, category, start_date, end_date)
      grouping_vars <- c(id, category)
    }
    #group by and arrange the dataset
    data.table::setorderv(dataset, order_vec)

    #row id by group
    dataset[, row_id := seq_len(.N), by = grouping_vars]

    #lagged end_date
    dataset[, lag_end_date := data.table::fifelse(row_id > 1, data.table::shift(get(..end_date)), get(..end_date))]

    # cumulative max for dates
    dataset[, lag_end_date := as.integer(lag_end_date)]
    dataset[, lag_end_date := cummax(lag_end_date), by = grouping_vars]
    dataset[, lag_end_date := as.Date(lag_end_date, "1970-01-01")]

    #compute the number of spell
    dataset[, num_spell := data.table::fifelse(row_id > 1 & get(..start_date) <= lag_end_date + gap_allowed, 0, 1)]
    dataset[, num_spell := cumsum(num_spell), by = grouping_vars]

    #group by num spell and compute min and max date for each one
    keep_col <- c(grouping_vars, "num_spell", "entry_spell_category", "exit_spell_category")
    grouping_vars <- c(grouping_vars, "num_spell")

    dataset <- dataset[, .(entry_spell_category = min(get(..start_date)),
                           exit_spell_category = max(get(..end_date))), by = grouping_vars]
    dataset <- dataset[, keep_col, with = FALSE]

    assign("output_spells_category", dataset)
  }

  #OPTIONAL SECTION REGARDING OVERLAPS

  if(overlap || only_overlaps){
    export_df <- overlap.internal(dataset, id, start_date, end_date, category, gap_allowed)
    assign(dataset_overlap, export_df, envir = parent.frame())
  }

  if(!only_overlaps) return(output_spells_category)
}
