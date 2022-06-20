test_that("dataset only accepts dataframes", {
  test_type_mult(col_name = "dataset", error_include = "data.frame", not_allowed_values = c(1, NA, NULL))
  test_error_type(dataset = "test_data", error_include = "data.frame")
  test_error_type(dataset = test_data, error_include = NA)
})

# test_that("id only accepts characters", {
#   test_type_mult(col_name = "id", error_include = "character", not_allowed_values = c(1, NA, NULL))
#   test_error_type(id = "id", error_include = NA)
# })
#
# test_that("start_date only accepts characters", {
#   test_type_mult(col_name = "start_date", error_include = "character", not_allowed_values = c(1, NA, NULL))
#   test_error_type(start_date = "op_start_date", error_include = NA)
# })
#
# test_that("end_date only accepts characters", {
#   test_type_mult(col_name = "end_date", error_include = "character", not_allowed_values = c(1, NA, NULL))
#   test_error_type(end_date = "op_end_date", error_include = NA)
# })
#
# test_that("category only accepts characters and NULL", {
#   test_type_mult(col_name = "category", error_include = "character", not_allowed_values = c(1, NA))
#   test_error_type(category = "op_meaning", error_include = NA)
#   test_error_type(category = NULL, error_include = NA)
# })
#
# test_that("replace_missing_end_date only accepts valid dates or NULL", {
#   test_error_type(replace_missing_end_date = NA, error_include = "Error 01")
#   test_error_type(replace_missing_end_date = 1, error_include = "Error 02")
#   test_error_type(replace_missing_end_date = "1", error_include = "Error 02")
#   test_error_type(replace_missing_end_date = 20210101, error_include = NA)
#   test_error_type(replace_missing_end_date = "20210101", error_include = NA)
# })
#
# test_that("overlap only accepts logical", {
#   test_type_mult(col_name = "overlap", error_include = "logical", not_allowed_values = c(1, "1", NA, NULL))
#   test_error_type(overlap = F, error_include = NA)
# })
#
# test_that("only_overlaps only accepts logical", {
#   test_type_mult(col_name = "only_overlaps", error_include = "logical", not_allowed_values = c(1, "1", NA, NULL))
#   test_error_type(only_overlaps = F, error_include = NA)
# })
#
# test_that("dataset_overlap only accepts characters", {
#   test_type_mult(col_name = "dataset_overlap", error_include = "character", not_allowed_values = c(1, NA, NULL))
#   test_error_type(dataset_overlap = "1", error_include = NA, overlap = T, category = "op_meaning")
# })
#
# test_that("gap_allowed only accepts integer", {
#   test_type_mult(col_name = "gap_allowed", error_include = "integer", not_allowed_values = c(1.1, 1, NA, NULL))
#   test_error_type(gap_allowed = 1, error_include = NA)
#   test_error_type(gap_allowed = 0, error_include = NA)
# })
