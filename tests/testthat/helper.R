test_error_type <- function(dataset = test_data, id = "id", start_date = "op_start_date",
                            end_date = "op_end_date", category = NULL, replace_missing_end_date = NULL,
                            overlap=F, dataset_overlap = NA_character_, only_overlaps=F, gap_allowed = 1, error_include) {
  eval(bquote(expect_error(check_sanitize_inputs(dataset = dataset, id = id, start_date = start_date,
                                                 end_date = end_date, category = category,
                                                 replace_missing_end_date = replace_missing_end_date,
                                                 overlap = overlap, dataset_overlap = dataset_overlap,
                                                 only_overlaps = only_overlaps, gap_allowed = gap_allowed),
                           regexp = error_include)))
}

test_CreateSpells.internal <- function(..., id = "id", start_date = "start_date",
                                       end_date = "end_date", category = NULL, gap_allowed = 1) {
  dataset <- row_wise_dt(...)
  CreateSpells.internal(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                        category = category, gap_allowed = gap_allowed)
}

test_type_mult <- function(col_name, error_include, not_allowed_values, ...) {
  for (i in not_allowed_values) {
    arguments <- list(...)
    simple_list <- list(i, error_include)
    simple_list <- c(arguments, simple_list)
    names(simple_list) <- c(col_name, "error_include")
    do.call(test_error_type, simple_list)
  }
}

row_wise_dt <- function(...) {
  tmp <- data.table::as.data.table(tibble::tribble(...))
  if ("entry_spell_category" %in% colnames(tmp)) {
    tmp[, entry_spell_category := lubridate::ymd(entry_spell_category)]
  } else {
    tmp[, start_date := lubridate::ymd(start_date)]
  }
  if ("exit_spell_category" %in% colnames(tmp)) {
    tmp[, exit_spell_category := lubridate::ymd(exit_spell_category)]
  } else {
    tmp[, end_date := lubridate::ymd(end_date)]
  }
}

