test_error_type <- function(dataset = test_data, id = "person_id", start_date = "op_start_date",
                            end_date = "op_end_date", category = NULL, replace_missing_end_date = NULL,
                            overlap = F, dataset_overlap = NA_character_, only_overlaps = F, gap_allowed = 1,
                            birth_date = NULL, gap_allowed_birth = 1, error_include) {
  expect_error(sanitize_inputs(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                               category = category, replace_missing_end_date = replace_missing_end_date,
                               overlap = overlap, dataset_overlap = dataset_overlap, only_overlaps = only_overlaps,
                               gap_allowed = gap_allowed, birth_date = birth_date, gap_allowed_birth = gap_allowed_birth),
               regexp = error_include)
}

test_error_type_special_names <- function(dataset = test_data_special_names, id = "id", start_date = "start_date",
                                          end_date = "end_date", category = NULL, replace_missing_end_date = NULL,
                                          overlap = F, dataset_overlap = NA_character_, only_overlaps = F,
                                          gap_allowed = 1, birth_date = NULL, gap_allowed_birth = 1, error_include) {
  expect_error(sanitize_inputs(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                               category = category, replace_missing_end_date = replace_missing_end_date,
                               overlap = overlap, dataset_overlap = dataset_overlap, only_overlaps = only_overlaps,
                               gap_allowed = gap_allowed, birth_date = birth_date, gap_allowed_birth = gap_allowed_birth),
               regexp = error_include)
}

test_error_type_overlap <- function(dataset = test_data_overlap, id = "person_id", start_date = "op_start_date",
                                    end_date = "op_end_date", category = "op_meaning", gap_allowed = 1, error_include) {
  expect_error(sanitize_inputs_overlap(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                                       category = category, gap_allowed = gap_allowed),
               regexp = error_include)
}

test_error_type_overlap_special_names <- function(dataset = test_data_overlap_special_names, id = "id",
                                                  start_date = "start_date", end_date = "end_date",
                                                  category = "category", gap_allowed = 1, error_include) {
  expect_error(sanitize_inputs_overlap(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                                       category = category, gap_allowed = gap_allowed),
               regexp = error_include)
}

expect_no_error_with_defaults <- function(dataset = test_data, id = "person_id", start_date = "op_start_date",
                                          end_date = "op_end_date", category = NULL, replace_missing_end_date = NULL,
                                          overlap = F, dataset_overlap = NA_character_, only_overlaps = F,
                                          gap_allowed = 1, birth_date = NULL, gap_allowed_birth = 1) {
  expect_no_error(sanitize_inputs(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                                  category = category, replace_missing_end_date = replace_missing_end_date,
                                  overlap = overlap, dataset_overlap = dataset_overlap, only_overlaps = only_overlaps,
                                  gap_allowed = gap_allowed, birth_date = birth_date, gap_allowed_birth = gap_allowed_birth))
}

expect_no_error_with_defaults_special_names <- function(dataset = test_data_special_names, id = "id",
                                                        start_date = "start_date", end_date = "end_date",
                                                        category = NULL, replace_missing_end_date = NULL, overlap = F,
                                                        dataset_overlap = NA_character_, only_overlaps = F,
                                                        gap_allowed = 1, birth_date = NULL, gap_allowed_birth = 1) {
  expect_no_error(sanitize_inputs(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                                  category = category, replace_missing_end_date = replace_missing_end_date,
                                  overlap = overlap, dataset_overlap = dataset_overlap, only_overlaps = only_overlaps,
                                  gap_allowed = gap_allowed, birth_date = birth_date, gap_allowed_birth = gap_allowed_birth))
}

expect_no_error_with_defaults_overlap <- function(dataset = test_data_overlap, id = "person_id",
                                                  start_date = "op_start_date", end_date = "op_end_date",
                                                  category = "op_meaning", gap_allowed = 1) {
  expect_no_error(sanitize_inputs_overlap(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                                          category = category, gap_allowed = gap_allowed))
}

expect_no_error_with_defaults_overlap_special_names <- function(dataset = test_data_overlap_special_names, id = "id",
                                                                start_date = "start_date", end_date = "end_date",
                                                                category = "category", gap_allowed = 1) {
  expect_no_error(sanitize_inputs_overlap(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                                          category = category, gap_allowed = gap_allowed))
}

test_CreateSpells.internal <- function(..., id = "person_id", start_date = "op_start_date", end_date = "op_end_date",
                                       category = NULL, gap_allowed = 1) {
  dataset <- row_wise_dt(...)
  CreateSpells.internal(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                        category = category, gap_allowed = gap_allowed)
}

test_overlap.internal <- function(..., id = "person_id", start_date = "op_start_date", end_date = "op_end_date",
                                  category = "op_meaning", gap_allowed = 1) {
  dataset <- row_wise_dt(...)
  overlap.internal_2(dataset = dataset, id = id, start_date = start_date, end_date = end_date,
                     category = category, gap_allowed = gap_allowed)
}

test_data_preparation <- function(..., start_date = "op_start_date", end_date = "op_end_date",
                                  replace_missing_end_date = NULL) {
  dataset <- data.table::as.data.table(tibble::tribble(...))
  data_preparation(dataset = dataset, start_date = start_date, end_date = end_date,
                   replace_missing_end_date = replace_missing_end_date)
}

test_data_preparation_special_names <- function(..., start_date = "start_date", end_date = "end_date",
                                                replace_missing_end_date = NULL) {
  dataset <- data.table::as.data.table(tibble::tribble(...))
  data_preparation(dataset = dataset, start_date = start_date, end_date = end_date,
                   replace_missing_end_date = replace_missing_end_date)
}

row_wise_dt <- function(...) {
  tmp <- data.table::as.data.table(tibble::tribble(...))

  start_col <- intersect(colnames(tmp), c("entry_spell_category", "start_date", "op_start_date"))
  if (length(start_col) != 0) tmp[, (start_col) := lubridate::ymd(get(..start_col))]

  end_col <- intersect(colnames(tmp), c("exit_spell_category", "end_date", "op_end_date"))
  if (length(end_col) != 0) tmp[, (end_col) := lubridate::ymd(get(..end_col))]

  birth_col <- intersect(colnames(tmp), c("date_of_birth", "birth_date"))
  if (length(birth_col) != 0) tmp[, (birth_col) := lubridate::ymd(get(..birth_col))]

  if ("num_spell" %in% colnames(tmp)) {
    tmp[, num_spell := as.integer(num_spell)]
  }

  return(tmp)
}

transform_to_special_names <- function(tmp) {
  start_col <- intersect(colnames(tmp), c("entry_spell_category", "op_start_date"))
  data.table::setnames(tmp, start_col, "start_date")

  end_col <- intersect(colnames(tmp), c("exit_spell_category", "op_end_date"))
  data.table::setnames(tmp, end_col, "end_date")
}
