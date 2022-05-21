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
#' @param replace_missing_end_date: (optional). When specified, it contains a date to replace end_date when it is missing.
#' @param overlap: (optional) default FALSE. If TRUE, overlaps of pairs of categories are processed as well.
#' @param dataset_overlap: (optional) if overlap TRUE, the name of the file containing the overlap dataset
#' @param only_overlaps: (optional) if only_overlaps TRUE, skip the calculation the spells
#' @param gap_allowed: (optional) Allowed gap in days between two observation periods after which they are counted as a different spell
#'
#' NOTE: Developed under R  4.0.3


CreateSpells <- function(dataset, id, start_date, end_date, category = NULL, replace_missing_end_date = NULL,
                         overlap=F, dataset_overlap = "df_overlap", only_overlaps=F, gap_allowed = 1){
  library(data.table)
  library(vetr)

  mycall <- match.call()
  mycall[[1]] <- as.symbol("check_sanitize_inputs") # use inner 1
  dataset <- eval(mycall)

  if (!only_overlaps) {
    dataset[, (start_date) := lubridate::ymd(get(..start_date))]
    dataset[, (end_date) := lubridate::ymd(get(..end_date))]

    if(!any(is.na(dataset[[start_date]]))) print("All start dates are not missing")
    else print("Some start dates are missing")
    if(!any(is.na(dataset[[end_date]]))) print("All end dates are not missing")
    else {print("Some end dates are missing")

      replace_missing_end_date <- lubridate::ymd(replace_missing_end_date)
      print(paste("Replacing missing end date/s with", replace_missing_end_date))
      dataset<-dataset[is.na(get(end_date)), (end_date) := replace_missing_end_date]

      #filter dataset
      dataset<-dataset[get(start_date) < get(end_date)]
    }

    #add level overall if category is given as input and has more than 1 category
    if (!is.null(category) & length(unique(dataset[[category]])) >= 2){
      dataset <- rbindlist(list(dataset, copy(dataset)[, (category) := "_overall"]))
      print("The level 'overall' is added as the is more than one category")
    }

    if(is.null(category)){
      order_vec <- c(id, start_date, end_date)
      grouping_vars <- id
    } else {
      order_vec <- c(id, category, start_date, end_date)
      grouping_vars <- c(id, category)
    }
    #group by and arrange the dataset
    setorderv(dataset, order_vec)

    #row id by group
    dataset[, row_id := seq_len(.N), by = grouping_vars]

    #lagged end_date
    dataset[, lag_end_date := fifelse(row_id > 1, shift(get(..end_date)), get(..end_date))]

    # cumulative max for dates
    dataset[, lag_end_date := as.integer(lag_end_date)]
    dataset[, lag_end_date := cummax(lag_end_date), by = grouping_vars]
    dataset[, lag_end_date := as.Date(lag_end_date, "1970-01-01")]

    #compute the number of spell
    dataset[, num_spell := fifelse(row_id > 1 & get(..start_date) <= lag_end_date + gap_allowed, 0, 1)]
    dataset[, num_spell := cumsum(num_spell), by = grouping_vars]

    #group by num spell and compute min and max date for each one
    keep_col <- c(grouping_vars, "num_spell", "entry_spell_category", "exit_spell_category")
    grouping_vars <- c(grouping_vars, "num_spell")

    dataset <- dataset[, .(entry_spell_category = min(get(..start_date)),
                           exit_spell_category = max(get(..end_date))), by = grouping_vars]
    dataset <- dataset[, ..keep_col]

    assign("output_spells_category", dataset)
  }

  #OPTIONAL SECTION REGARDING OVERLAPS

  if(overlap || only_overlaps){

    export_df <- data.table()
    dataset <- dataset[get(category) != "_overall",]

    # Create list of unique not missing categories
    unique_cat <- as.list(unique(dataset[[category]]))
    unique_cat <- unique_cat[!is.na(unique_cat)]

    # Create the combinations of pairs of categories
    permut <- t(utils::combn(unique_cat, 2))

    # Cycle for each pair
    for (i in seq_len(nrow(permut))) {

      p_1 <- permut[i, 1]
      p_2 <- permut[i, 2]

      ens_1 <- paste0("entry_spell_category_", p_1)
      exs_1 <- paste0("exit_spell_category_", p_1)
      ens_2 <- paste0("entry_spell_category_", p_2)
      exs_2 <- paste0("exit_spell_category_", p_2)

      #	For each pair of values A and B, create two temporary datasets
      outputA <- dataset[get(category) == p_1, ][, c("num_spell", category) := NULL]
      setnames(outputA, c("entry_spell_category", "exit_spell_category"), c(ens_1, exs_1))

      outputB <- dataset[get(category) == p_2, ][, c("num_spell", category) := NULL]
      setnames(outputB, c("entry_spell_category", "exit_spell_category"), c(ens_2, exs_2))

      #	Perform a join multi-to-multi of the two datasets
      CAT <- merge(outputA, outputB, by = c(id), all = T, allow.cartesian = T)
      CAT <- CAT[(get(ens_1) <= get(exs_2) + gap_allowed & get(exs_1) + gap_allowed >= get(ens_2)) | (get(ens_2) <= get(exs_1) + gap_allowed & get(exs_2) + gap_allowed >= get(ens_1)), ]

      #	If no overlap go to next pair
      if (nrow(CAT) == 0) next

      # Calculate overlapping spells between categories
      CAT <- CAT[, .(entry_spell_category = max(get(ens_1), get(ens_2)),
                     exit_spell_category = min(get(exs_1), get(exs_2))), by = id]
      CAT <- CAT[, (category) := paste(p_1, p_2, sep = "_")]
      CAT <- CAT[order(c(id, "entry_spell_category"))][, c(..id, "entry_spell_category", "exit_spell_category", ..category)]
      CAT <- CAT[, num_spell := rowid(get(..id))]

      export_df <- rbind(export_df, CAT)
    }

    assign(dataset_overlap, export_df, envir = parent.frame())
  }

  if(!only_overlaps) return(output_spells_category)
}
