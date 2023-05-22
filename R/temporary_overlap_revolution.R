overlap.internal_2 <- function(dataset, id, start_date, end_date, category, gap_allowed) {

  sanitize_inputs_overlap(dataset, id, start_date, end_date, category, gap_allowed)

  dataset <- dataset[get("category") != "_overall",]
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

#
#     # Copy the dataset and exit_spell_category add 1 day to get the first day without events
#     dataset_filtered_end <- copy(dataset_filtered)[, get(start_date) := NULL]
#     dataset_filtered_end[, (end_date) := get(..end_date) + 1]
#     setnames(dataset_filtered_end, end_date, "date")
#
#     # In dataset remove exit_spell_category
#     dataset_filtered[, get(end_date) := NULL]
#     setnames(dataset_filtered, start_date, "date")
#
#     # Combine all datasets. value_of_variable is 1 when person has event.
#     dataset_filtered <- rbindlist(list(dataset_filtered[, contribution := 1],
#                                        dataset_filtered_end[, contribution := -1]))
#
#     # Sum contribution/value_of_variable in each date for each person
#     dataset_filtered <- dataset_filtered[, .(contribution = sum(contribution)), by = c(id, "date")]
#
#     # Order by person and date the take the cumulative sum
#     setorder(dataset_filtered, id, "date")
#     dataset_filtered <- dataset_filtered[, .(date, contribution = cumsum(contribution)), by = id]
#     setnames(dataset_filtered, "contribution", "value_of_variable")
#
#     #Add end date of period
#     dataset_filtered <- dataset_filtered[, exit_spell_category := data.table::shift(date, type = "lead"), by = id]
#     setnames(dataset_filtered, "date", "entry_spell_category")
#
#     dataset_filtered <- dataset_filtered[value_of_variable > 1, ]

    dataset_filtered <- dataset_filtered[, (category) := paste(p_1, p_2, sep = "_")]

    select_col <- c(id, start_date, end_date, category)
    dataset_filtered <- dataset_filtered[, ..select_col]

    dataset_filtered <- dataset_filtered[, num_spell := data.table::rowid(get(..id))]

    export_df <- append(export_df, list(dataset_filtered))
  }

  export_df <- data.table::rbindlist(export_df)

  return(export_df)

}
