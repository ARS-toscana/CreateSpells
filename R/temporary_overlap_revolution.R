overlap.internal <- function(dataset, id, start_date, end_date, category, gap_allowed) {

  dataset <- dataset[get("category") != "_overall",]

  # Create list of unique not missing categories
  unique_cat <- unique(dataset[!is.na(category), category])

  # Create the combinations of pairs of categories
  permut <- utils::combn(unique_cat, 2, simplify = F)

  export_df <- list()

  # Cycle for each pair
  for (i in permut) {

    p_1 <- i[1]
    p_2 <- i[2]

    ens_1 <- paste0("entry_spell_category_", p_1)
    exs_1 <- paste0("exit_spell_category_", p_1)
    ens_2 <- paste0("entry_spell_category_", p_2)
    exs_2 <- paste0("exit_spell_category_", p_2)

    #	For each pair of values A and B, create two temporary datasets
    to_drop <- c(category)
    if ("num_spell" %in% colnames(dataset)) to_drop <- c("num_spell", to_drop)

    dataset_filtered <- dataset[get("category") %in% c(p_1, p_2)]

    # Copy the dataset and exit_spell_category add 1 day to get the first day without events
    dataset_filtered_end <- copy(dataset_filtered)[, get(start_date) := NULL]
    dataset_filtered_end[, (end_date) := get(end_date) + 1]
    setnames(dataset_filtered_end, end_date, "date")

    # In dataset remove exit_spell_category
    dataset_filtered[, get(end_date) := NULL]
    setnames(dataset_filtered, start_date, "date")

    # Combine all datasets. value_of_variable is 1 when person has event.
    dataset_filtered <- rbindlist(list(dataset_filtered[, contribution := 1],
                                       dataset_filtered_end[, contribution := -1]))

    dataset_filtered <- dataset_filtered[, (category) := paste(p_1, p_2, sep = "_")]

    # Sum contribution/value_of_variable in each date for each person
    dataset_filtered <- dataset_filtered[, .(contribution = sum(contribution)), by = c("person_id", "date")]

    # Order by person and date the take the cumulative sum
    setorder(dataset_filtered, "person_id", "date")
    dataset_filtered <- dataset_filtered[, .(date, contribution = cumsum(contribution)), by = "person_id"]
    setnames(D3_TD_list, "contribution", "value_of_variable")

    #Add end date of period
    dataset_filtered <- dataset_filtered[, exit_spell_category = shift(date, type = "lead"), by = "person_id"]
    setnames(D3_TD_list, "date", "entry_spell_category")

    select_col <- c(id, "entry_spell_category", "exit_spell_category", category)
    CAT <- CAT[, select_col, with = FALSE]

    CAT <- CAT[, num_spell := data.table::rowid(get(..id))]

    export_df <- append(export_df, list(CAT))
  }

  export_df <- data.table::rbindlist(export_df)

  return(export_df)

}


