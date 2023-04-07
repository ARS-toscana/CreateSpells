pass_all_arguments <- function(x) {
  mycall <- match.call(sys.function(sys.parent()), sys.call(sys.parent()))
  mycall[[1]] <- as.symbol(x) # use inner 1
  eval(mycall)
}

data_preparation <- function(dataset, id, start_date, end_date, category, replace_missing_end_date) {

  dataset[, (start_date) := lubridate::ymd(get(..start_date))]
  dataset[, (end_date) := lubridate::ymd(get(..end_date))]

  nrow_before <- nrow(dataset)
  dataset <- dataset[!is.na(get(start_date))]
  nrow_after <- nrow(dataset)
  if(nrow_before != nrow_after) {
    warning("Some start dates are missing. Those periods have been removed from computation of spells")
  }

  if(any(is.na(dataset[[end_date]]))) {

    print("Some end dates are missing")

    if (is.null(replace_missing_end_date)){
      warning("Some end dates are missing. Since parameter 'replace_missing_end_date' has not been specified,
                  those periods have been removed from computation of spells")
      dataset <- dataset[!is.na(get(end_date))]

      nrow_before <- nrow(dataset)
      dataset <- dataset[get(start_date) <= get(end_date)]
      nrow_after <- nrow(dataset)
      if(nrow_before != nrow_after) {
        warning("Some start dates are after their respective end dates.
                    Those periods have been removed from computation of spells")
      }

    } else {
      replace_missing_end_date <- lubridate::ymd(replace_missing_end_date)
      print(paste("Replacing missing end date/s with", replace_missing_end_date))
      dataset[is.na(get(end_date)), (end_date) := replace_missing_end_date]
      dataset_missing <- dataset[is.na(get(end_date))][, (end_date) := replace_missing_end_date]
      dataset <- dataset[!is.na(get(end_date))]

      nrow_before <- nrow(dataset)
      dataset <- dataset[get(start_date) <= get(end_date)]
      nrow_after <- nrow(dataset)
      if(nrow_before != nrow_after) {
        warning("Some start dates are after their respective end dates.
                    Those periods have been removed from computation of spells")
      }

      dataset_missing <- dataset_missing[get(start_date) <= get(end_date)]

      dataset <- rbindlist(list(dataset_missing, dataset))
    }
  }

  #add level overall if category is given as input and has more than 1 category
  if (!is.null(category) && length(unique(dataset[[category]])) >= 2){
    dataset <- data.table::rbindlist(list(dataset, data.table::copy(dataset)[, (category) := "_overall"]))
    print("The level 'overall' is added as the is more than one category")
  }

  return(dataset)
}

overlap.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {

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
    data.table::setnames(outputA, c("entry_spell_category", "exit_spell_category"), c(ens_1, exs_1))

    outputB <- dataset[get(category) == p_2, ][, c("num_spell", category) := NULL]
    data.table::setnames(outputB, c("entry_spell_category", "exit_spell_category"), c(ens_2, exs_2))

    #	Perform a join multi-to-multi of the two datasets
    CAT <- merge(outputA, outputB, by = c(id), all = T, allow.cartesian = T)
    CAT <- CAT[(get(ens_1) <= get(exs_2) + gap_allowed & get(exs_1) + gap_allowed >= get(ens_2)) | (get(ens_2) <= get(exs_1) + gap_allowed & get(exs_2) + gap_allowed >= get(ens_1)), ]

    #	If no overlap go to next pair
    if (nrow(CAT) == 0) next

    # Calculate overlapping spells between categories
    CAT <- CAT[, .(entry_spell_category = pmax(get(ens_1), get(ens_2)),
                   exit_spell_category = pmin(get(exs_1), get(exs_2)))]
    CAT <- CAT[, (category) := paste(p_1, p_2, sep = "_")]

    select_col <- c(id, "entry_spell_category", "exit_spell_category", category)
    CAT <- CAT[order(c(id, "entry_spell_category"))][, select_col, with = FALSE]

    CAT <- CAT[, num_spell := data.table::rowid(get(..id))]

    export_df <- rbind(export_df, CAT)
  }

  return(export_df)

}
