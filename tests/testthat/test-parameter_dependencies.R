test_that("Error 03 if specified column name is not in dataset", {
  test_error_type(id = "a", error_include = "Error 03")
  test_error_type(start_date = "a", error_include = "Error 03")
  test_error_type(end_date = "a", error_include = "Error 03")
  test_error_type(category = "a", error_include = "Error 03")
})

test_that("Error 04 if want to calculate overlap without specifying the category argument", {
  test_error_type(overlap = T, error_include = "Error 04")
  test_error_type(only_overlaps = T, error_include = "Error 04")
})

test_that("Error 05 if want to calculate overlap without specifying the category argument", {
  test_error_type(overlap = T, error_include = "Error 05", category = "op_meaning", dataset = test_data[op_meaning == "a"])
  test_error_type(only_overlaps = T, error_include = "Error 05", category = "op_meaning", dataset = test_data[op_meaning == "a"])
})

test_that("Error 06 if want to calculate overlap without specifying the category argument", {
  test_error_type(dataset_overlap = "df_overlap", error_include = "Error 06", category = "op_meaning")
  test_error_type(dataset_overlap = "df_overlap", error_include = NA, category = "op_meaning", overlap = T)
  test_error_type(dataset_overlap = "df_overlap", error_include = NA, category = "op_meaning", only_overlaps = T)
})
