

# par_excl_dataset <- list(dataset = "test_data",
#                          id = "id",
#                          start_date = "op_start_date",
#                          end_date = "op_end_date",
#                          category ="op_meaning",
#                          replace_missing_end_date = 20250101,
#                          gap_allowed = 1,
#                          overlap = T,
#                          dataset_overlap = "df_overlap_calculated",
#                          only_overlaps = F,
#                          gap_allowed = 1)
#
# par_excl_dataset[["dataset"]]

expect_floor_equal <- function(dataset = test_data, id = "id", start_date = "op_start_date",
                               end_date = "op_end_date", category = NULL, replace_missing_end_date = NULL,
                               overlap=F, dataset_overlap = "df_overlap", only_overlaps=F, gap_allowed = 1, error_include) {
  eval(bquote(expect_error(check_sanitize_inputs(dataset = dataset, id = id, start_date = start_date,
                                                 end_date = end_date, category = category,
                                                 replace_missing_end_date = replace_missing_end_date,
                                                 overlap = overlap, dataset_overlap = dataset_overlap,
                                                 only_overlaps = only_overlaps, gap_allowed = gap_allowed),
                           regexp = error_include)))
}

test_that("dataset only accepts dataframes", {
  expect_floor_equal(dataset = 1, error_include = "data.frame")
  expect_floor_equal(dataset = NA, error_include = "data.frame")
  expect_floor_equal(dataset = NULL, error_include = "data.frame")
  expect_floor_equal(dataset = "test_data", error_include = "data.frame")
  expect_floor_equal(dataset = test_data, error_include = NA)
})

test_that("id only accepts characters", {
  expect_floor_equal(id = 1, error_include = "character")
  expect_floor_equal(id = NA, error_include = "character")
  expect_floor_equal(id = NULL, error_include = "character")
  expect_floor_equal(id = "id", error_include = NA)
})

test_that("start_date only accepts characters", {
  expect_floor_equal(start_date = 1, error_include = "character")
  expect_floor_equal(start_date = NA, error_include = "character")
  expect_floor_equal(start_date = NULL, error_include = "character")
  expect_floor_equal(start_date = "op_start_date", error_include = NA)
})

test_that("end_date only accepts characters", {
  expect_floor_equal(end_date = 1, error_include = "character")
  expect_floor_equal(end_date = NA, error_include = "character")
  expect_floor_equal(end_date = NULL, error_include = "character")
  expect_floor_equal(end_date = "op_end_date", error_include = NA)
})
