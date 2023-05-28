# Pass all argument of a previous the external function to an inner one
pass_all_arguments <- function(x) {
  mycall <- match.call(sys.function(sys.parent()), sys.call(sys.parent()))
  mycall[[1]] <- as.symbol(x) # use inner 1
  eval(mycall)
}

# General preparation of the data
data_preparation <- function(dataset, start_date, end_date, replace_missing_end_date) {

  ..start_date <- ..end_date <- "Shut up!"

  if (!inherits(dataset$start_date, 'Date')) dataset[, (start_date) := lubridate::ymd(get(..start_date))]
  if (!inherits(dataset$end_date, 'Date')) dataset[, (end_date) := lubridate::ymd(get(..end_date))]

  if (any(is.na(dataset[[end_date]]))) {

    message("Some end dates are missing")

    if (is.null(replace_missing_end_date)){

      warning("Since parameter 'replace_missing_end_date' has not been specified,
              those periods have been removed from computation of spells (Warning 01)")
      prev_env <- environment(NULL)
      dataset <- dataset[!is.na(get(prev_env$end_date)), ]

    } else {

      replace_missing_end_date <- lubridate::ymd(replace_missing_end_date)
      message(paste("Replacing missing end date/s with", replace_missing_end_date, "and removing all periods with start > end"))

      prev_env <- environment(NULL)
      dataset[is.na(get(prev_env$end_date)), (end_date) := replace_missing_end_date]
      dataset <- dataset[get(prev_env$start_date) <= get(prev_env$end_date)]

    }
  }

  return(dataset)
}

# Preparation for the categories
data_preparation_2 <- function(dataset, category) {

  #add level overall if category is given as input and has at least 2 categories
  if (!is.null(category) && length(unique(dataset[[category]])) >= 2){
    dataset <- data.table::rbindlist(list(dataset, data.table::copy(dataset)[, (category) := "_overall"]))
    message("The level 'overall' is added as the is more than one category")
  }

  return(dataset)
}

#Preparation for overlaps
data_preparation_3 <- function(dataset, start_date, birth_date, gap_allowed_birth) {

  ..birth_date <- prev_env <- "Shut up!"

  if (!inherits(dataset$birth_date, 'Date')) dataset[, (birth_date) := lubridate::ymd(get(..birth_date))]
  dataset[get(prev_env$start_date) - gap_allowed_birth <= get(prev_env$birth_date), (start_date) := birth_date]

  return(dataset)
}

# Internal function for calculating overlaps
overlap.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {

  ..to_keep_start <- ..to_keep_end <- ..select_col <- num_spell <- ..id <- "Shut up!"

  data.table::setnames(dataset, c(start_date, end_date, category),
                       c("entry_spell_category", "exit_spell_category", "category"))

  start_date <- "entry_spell_category"
  end_date <- "exit_spell_category"
  category <- "category"

  # Create list of unique not missing categories
  unique_cat <- unique(dataset[!is.na(category), category])

  # Create the combinations of pairs of categories
  permut <- utils::combn(unique_cat, 2, simplify = F)

  export_df <- list()

  # Cycle for each pair
  for (i in permut) {

    p_1 <- i[1]
    p_2 <- i[2]

    #	For each pair of values A and B, create two temporary datasets
    to_keep_start <- c(id, start_date, end_date, category)
    to_keep_end <- c(id, start_date, end_date)
    dataset <- dataset[, ..to_keep_start]

    dataset_filtered_1 <- data.table::copy(dataset)[get("category") == p_1][, ..to_keep_end]
    dataset_filtered_2 <- dataset[get("category") == p_2][, ..to_keep_end]

    data.table::setkeyv(dataset_filtered_2, to_keep_end)
    dataset_filtered_1 <- data.table::foverlaps(dataset_filtered_1, dataset_filtered_2, nomatch = NULL)
    rm(dataset_filtered_2)

    foverlaps_start <- paste0("i.", start_date)
    foverlaps_end <- paste0("i.", end_date)

    dataset_filtered_1[, (start_date) := pmax(get(start_date), get(foverlaps_start))]
    dataset_filtered_1[, (end_date) := pmin(get(end_date), get(foverlaps_end))]

    dataset_filtered <- dataset_filtered_1[, ..to_keep_end]
    rm(dataset_filtered_1)

    dataset_filtered <- dataset_filtered[, (category) := paste(p_1, p_2, sep = "_")]

    select_col <- c(id, start_date, end_date, category)
    dataset_filtered <- dataset_filtered[, ..select_col]

    dataset_filtered <- dataset_filtered[, num_spell := data.table::rowid(get(..id))]

    export_df <- append(export_df, list(dataset_filtered))
  }

  export_df <- data.table::rbindlist(export_df)

  return(export_df)

}

# Internal function for calculating the spells
CreateSpells.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {

  row_id <- .N <- lag_end_date <- ..end_date <- num_spell <- ..start_date <- . <- "Shut up!"

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
