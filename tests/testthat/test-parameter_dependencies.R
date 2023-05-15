test_that("Error 01 if replace_missing_end_date is missing", {
  test_error_type(replace_missing_end_date = NA, error_include = "Error 01")
  expect_no_error_with_defaults(replace_missing_end_date = "20100101")
})

test_that("Error 02 if replace_missing_end_date is not a date", {
  test_error_type(replace_missing_end_date = 123456, error_include = "Error 02")
  test_error_type(replace_missing_end_date = "123456", error_include = "Error 02")
  test_error_type(replace_missing_end_date = "abcabc", error_include = "Error 02")
  expect_no_error_with_defaults(replace_missing_end_date = lubridate::ymd("20100101"))
})

test_that("Error 03 if specified column name is not in dataset", {

  test_error_type(id = "a", error_include = "Error 03")
  expect_no_error_with_defaults(id = "id")

  test_error_type(start_date = "a", error_include = "Error 03")
  expect_no_error_with_defaults(start_date = "op_start_date")

  test_error_type(end_date = "a", error_include = "Error 03")
  expect_no_error_with_defaults(end_date = "op_end_date")

  test_error_type(category = "a", error_include = "Error 03")
  expect_no_error_with_defaults(category = NULL)
  expect_no_error_with_defaults(category = "op_meaning")
})

test_that("Error 04 if want to calculate overlap without specifying the category argument", {
  test_error_type(overlap = T, error_include = "Error 04")
  expect_no_error_with_defaults(overlap = T, category = "op_meaning")

  test_error_type(only_overlaps = T, error_include = "Error 04")
  expect_no_error_with_defaults(only_overlaps = T, category = "op_meaning")
})

test_that("Error 05 if want to calculate overlap with only one category", {
  test_error_type(overlap = T, error_include = "Error 05", category = "op_meaning",
                  dataset = test_data[test_data$op_meaning == "a", ])
  expect_no_error_with_defaults(overlap = T, category = "op_meaning", dataset = test_data)
  test_error_type(only_overlaps = T, error_include = "Error 05", category = "op_meaning",
                  dataset = test_data[test_data$op_meaning == "a", ])
  expect_no_error_with_defaults(only_overlaps = T, category = "op_meaning", dataset = test_data)
})

test_that("Error 06 if dataset_overlap has been specified but overlap or only_overlaps are missing", {
  test_error_type(dataset_overlap = "df_overlap", error_include = "Error 06", category = "op_meaning")
  expect_no_error_with_defaults(dataset_overlap = "df_overlap", category = "op_meaning", overlap = T)
  expect_no_error_with_defaults(dataset_overlap = "df_overlap", category = "op_meaning", only_overlaps = T)
})

test_that("Error 07 some start dates are missing", {
  test_error_type(dataset = row_wise_dt(~id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                        "a",     "20100101",    "20200101",         "a",
                                        "a",             NA,    "20200101",         "a"), error_include = "Error 07")
  test_error_type(dataset = row_wise_dt(~id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                        "a",       20100101,      20200101,         "a",
                                        "a",             NA,      20200101,         "a"), error_include = "Error 07")
  expect_no_error_with_defaults(dataset = row_wise_dt(~id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                                      "a",     "20100101",    "20200101",         "a",
                                                      "a",     "20110101",    "20200101",         "a"))
  expect_no_error_with_defaults(dataset = row_wise_dt(~id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                                      "a",       20100101,      20200101,         "a",
                                                      "a",       20110101,      20200101,         "a"))
})

test_that("Error 08 some start dates are not dates or ymd format", {
  test_error_type(dataset = tibble::tribble(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                            "a",   "2020010112",   "20200101",         "a"), error_include = "Error 08")
  test_error_type(dataset = tibble::tribble(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                            "a",     2020010112,     20200101,         "a"), error_include = "Error 08")
  expect_no_error_with_defaults(dataset = tibble::tribble(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",     "20200101",   "20200101",         "a"))
  expect_no_error_with_defaults(dataset = tibble::tribble(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",       20200101,     20200101,         "a"))
  expect_no_error_with_defaults(dataset = tibble::tribble(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",       "200101",     "200101",         "a"))
  expect_no_error_with_defaults(dataset = tibble::tribble(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",         200101,       200101,         "a"))
})

test_that("Error 09 some end dates are not dates or ymd format", {
  test_error_type(dataset = row_wise_dt(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                        "a",     "20100101",   "20200101",         "a",
                                        "a",     "20200101",   "20100101",         "a"), error_include = "Error 10")
  expect_no_error_with_defaults(dataset = row_wise_dt(~id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                      "a",     "20200101",   "20200101",         "a"))
})



test_that("Error 01 if specified column name is not in dataset", {

  test_error_type_2(id = "a", error_include = "Error 01")
  expect_no_error_with_defaults_2(id = "id")

  test_error_type_2(start_date = "a", error_include = "Error 01")
  expect_no_error_with_defaults_2(start_date = "op_start_date")

  test_error_type_2(end_date = "a", error_include = "Error 01")
  expect_no_error_with_defaults_2(end_date = "op_end_date")

  test_error_type_2(category = "a", error_include = "Error 01")
  # expect_no_error_with_defaults_2(category = NULL)
  expect_no_error_with_defaults_2(category = "op_meaning")
})
