pass_all_arguments <- function(x) {
  mycall <- match.call(sys.function(sys.parent()), sys.call(sys.parent()))
  mycall[[1]] <- as.symbol(x) # use inner 1
  eval(mycall)
}

data_preparation <- function(dataset, start_date, end_date, replace_missing_end_date) {

  if (!inherits(dataset$start_date, 'Date')) dataset[, (start_date) := lubridate::ymd(get(..start_date))]
  if (!inherits(dataset$end_date, 'Date')) dataset[, (end_date) := lubridate::ymd(get(..end_date))]

  if (any(is.na(dataset[[end_date]]))) {

    message("Some end dates are missing")

    if (is.null(replace_missing_end_date)){
      warning("Since parameter 'replace_missing_end_date' has not been specified,
              those periods have been removed from computation of spells  (Warning 01)")
      prev_env <- environment(NULL)
      dataset <- dataset[!is.na(get(prev_env$end_date)), ]

      # nrow_before <- nrow(dataset)
      # dataset <- dataset[get(start_date) <= get(end_date)]
      # nrow_after <- nrow(dataset)
      # if(nrow_before != nrow_after) {
      #   warning("Some start dates are after their respective end dates.
      #               Those periods have been removed from computation of spells")
      # }

    } else {
      replace_missing_end_date <- lubridate::ymd(replace_missing_end_date)
      message(paste("Replacing missing end date/s with", replace_missing_end_date))

      prev_env <- environment(NULL)
      dataset[is.na(get(prev_env$end_date)), (end_date) := replace_missing_end_date]
      dataset <- dataset[get(prev_env$start_date) <= get(prev_env$end_date)]
      # dataset_missing <- dataset[is.na(get(end_date))][, (end_date) := replace_missing_end_date]
      # dataset <- dataset[!is.na(get(end_date))]
      #
      # nrow_before <- nrow(dataset)
      # dataset <- dataset[get(start_date) <= get(end_date)]
      # nrow_after <- nrow(dataset)
      # if(nrow_before != nrow_after) {
      #   warning("Some start dates are after their respective end dates.
      #               Those periods have been removed from computation of spells")
      # }
      #
      # dataset_missing <- dataset_missing[get(start_date) <= get(end_date)]
      #
      # dataset <- rbindlist(list(dataset_missing, dataset))
    }
  }

  return(dataset)
}

data_preparation_2 <- function(dataset, category) {

  #add level overall if category is given as input and has at least 2 categories
  if (!is.null(category) && length(unique(dataset[[category]])) >= 2){
    dataset <- data.table::rbindlist(list(dataset, data.table::copy(dataset)[, (category) := "_overall"]))
    message("The level 'overall' is added as the is more than one category")
  }

  return(dataset)
}

data_preparation_3 <- function(dataset, start_date, birth_date, gap_allowed_birth) {

  if (!inherits(dataset$birth_date, 'Date')) dataset[, (birth_date) := lubridate::ymd(get(..birth_date))]
  dataset[get(prev_env$start_date) - gap_allowed_birth <= get(prev_env$birth_date), (start_date) := birth_date]

  return(dataset)
}

overlap.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {

  sanitize_inputs_overlap(dataset, id, start_date, end_date, category)

  dataset <- dataset[get("category") != "_overall",]

  # Create list of unique not missing categories
  unique_cat <- as.list(unique(dataset[[category]]))
  unique_cat <- unique_cat[!is.na(unique_cat)]

  # Create the combinations of pairs of categories
  permut <- t(utils::combn(unique_cat, 2))

  export_df <- list()

  # Cycle for each pair
  for (i in seq_len(nrow(permut))) {

    p_1 <- permut[i, 1]
    p_2 <- permut[i, 2]

    ens_1 <- paste0("entry_spell_category_", p_1)
    exs_1 <- paste0("exit_spell_category_", p_1)
    ens_2 <- paste0("entry_spell_category_", p_2)
    exs_2 <- paste0("exit_spell_category_", p_2)

    #	For each pair of values A and B, create two temporary datasets
    to_drop <- c(category)
    if ("num_spell" %in% colnames(dataset)) to_drop <- c("num_spell", to_drop)

    outputA <- dataset[get("category") == p_1, ][, (to_drop) := NULL]
    data.table::setnames(outputA, c(start_date, end_date), c(ens_1, exs_1))

    outputB <- dataset[get("category") == p_2, ][, (to_drop) := NULL]
    data.table::setnames(outputB, c(start_date, end_date), c(ens_2, exs_2))

    #	Perform a join multi-to-multi of the two datasets
    CAT <- merge(outputA, outputB, by = c(id), all = T, allow.cartesian = T)
    CAT <- CAT[(get(ens_1) <= get(exs_2) + gap_allowed & get(exs_1) + gap_allowed >= get(ens_2)) | (get(ens_2) <= get(exs_1) + gap_allowed & get(exs_2) + gap_allowed >= get(ens_1)), ]

    #	If no overlap go to next pair
    if (nrow(CAT) == 0) next

    # Calculate overlapping spells between categories
    CAT <- CAT[, .(id, entry_spell_category = pmax(get(ens_1), get(ens_2)),
                   exit_spell_category = pmin(get(exs_1), get(exs_2)))]
    CAT <- CAT[, (category) := paste(p_1, p_2, sep = "_")]

    select_col <- c(id, "entry_spell_category", "exit_spell_category", category)
    data.table::setorderv(CAT, c(get(id), "entry_spell_category", "exit_spell_category"))
    CAT <- CAT[, select_col, with = FALSE]

    CAT <- CAT[, num_spell := data.table::rowid(get(..id))]

    export_df <- append(export_df, list(CAT))
  }

  export_df <- data.table::rbindlist(export_df)

  return(export_df)

}


CreateSpells.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {

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
  dataset[, num_spell := as.integer(num_spell)]

  #group by num spell and compute min and max date for each one
  keep_col <- c(grouping_vars, "num_spell", "entry_spell_category", "exit_spell_category")
  grouping_vars <- c(grouping_vars, "num_spell")

  dataset <- dataset[, .(entry_spell_category = min(get(..start_date)),
                         exit_spell_category = max(get(..end_date))), by = grouping_vars]
  dataset <- dataset[, keep_col, with = FALSE]

}
