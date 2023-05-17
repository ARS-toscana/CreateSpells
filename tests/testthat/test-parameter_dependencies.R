test_that("Error 01 if replace_missing_end_date is missing", {
  test_error_type(replace_missing_end_date = NA, error_include = "Error 01")
  test_error_type_3(dataset = test_data_3, replace_missing_end_date = NA, error_include = "Error 01")
  expect_no_error_with_defaults(replace_missing_end_date = "20100101")
  expect_no_error_with_defaults_3(dataset = test_data_3, replace_missing_end_date = "20100101")
})

test_that("Error 02 if replace_missing_end_date is not a date", {
  test_error_type(replace_missing_end_date = 123456, error_include = "Error 02")
  test_error_type(replace_missing_end_date = "123456", error_include = "Error 02")
  test_error_type(replace_missing_end_date = "abcabc", error_include = "Error 02")
  test_error_type_3(replace_missing_end_date = 123456, error_include = "Error 02")
  test_error_type_3(replace_missing_end_date = "123456", error_include = "Error 02")
  test_error_type_3(replace_missing_end_date = "abcabc", error_include = "Error 02")
  expect_no_error_with_defaults(replace_missing_end_date = 20100101)
  expect_no_error_with_defaults(replace_missing_end_date = "20100101")
  expect_no_error_with_defaults(replace_missing_end_date = lubridate::ymd("20100101"))
  expect_no_error_with_defaults_3(replace_missing_end_date = 20100101)
  expect_no_error_with_defaults_3(replace_missing_end_date = "20100101")
  expect_no_error_with_defaults_3(replace_missing_end_date = lubridate::ymd("20100101"))
})

test_that("Error 03 if specified column name is not in dataset", {

  test_error_type(id = "a", error_include = "Error 03")
  test_error_type_3(id = "a", error_include = "Error 03")
  expect_no_error_with_defaults(id = "person_id")
  expect_no_error_with_defaults_3(id = "id")

  test_error_type(start_date = "a", error_include = "Error 03")
  test_error_type_3(start_date = "a", error_include = "Error 03")
  expect_no_error_with_defaults(start_date = "op_start_date")
  expect_no_error_with_defaults_3(start_date = "start_date")

  test_error_type(end_date = "a", error_include = "Error 03")
  test_error_type_3(end_date = "a", error_include = "Error 03")
  expect_no_error_with_defaults(end_date = "op_end_date")
  expect_no_error_with_defaults_3(end_date = "end_date")

  test_error_type(category = "a", error_include = "Error 03")
  test_error_type_3(category = "a", error_include = "Error 03")
  expect_no_error_with_defaults(category = NULL)
  expect_no_error_with_defaults(category = "op_meaning")
  expect_no_error_with_defaults_3(category = NULL)
  expect_no_error_with_defaults_3(category = "category")
})

test_that("Error 04 if want to calculate overlap without specifying the category argument", {
  test_error_type(overlap = T, error_include = "Error 04")
  test_error_type_3(overlap = T, error_include = "Error 04")
  expect_no_error_with_defaults(overlap = T, category = "op_meaning")
  expect_no_error_with_defaults_3(overlap = T, category = "category")

  test_error_type(only_overlaps = T, error_include = "Error 04")
  test_error_type_3(only_overlaps = T, error_include = "Error 04")
  expect_no_error_with_defaults(only_overlaps = T, category = "op_meaning")
  expect_no_error_with_defaults_3(only_overlaps = T, category = "category")
})

test_that("Error 05 if want to calculate overlap with only one category", {
  test_error_type(overlap = T, error_include = "Error 05", category = "op_meaning",
                  dataset = test_data[test_data$op_meaning == "a", ])
  test_error_type_3(overlap = T, error_include = "Error 05", category = "category",
                  dataset = test_data_3[test_data_3$category == "a", ])
  expect_no_error_with_defaults(overlap = T, category = "op_meaning")
  expect_no_error_with_defaults_3(overlap = T, category = "category")

  test_error_type(only_overlaps = T, error_include = "Error 05", category = "op_meaning",
                  dataset = test_data[test_data$op_meaning == "a", ])

  test_error_type_3(only_overlaps = T, error_include = "Error 05", category = "category",
                  dataset = test_data_3[test_data_3$category == "a", ])
  expect_no_error_with_defaults(only_overlaps = T, category = "op_meaning")
  expect_no_error_with_defaults_3(only_overlaps = T, category = "category")
})

test_that("Error 06 if dataset_overlap has been specified but overlap or only_overlaps are missing", {
  test_error_type(dataset_overlap = "df_overlap", error_include = "Error 06", category = "op_meaning")
  test_error_type_3(dataset_overlap = "df_overlap", error_include = "Error 06", category = "category")
  expect_no_error_with_defaults(dataset_overlap = "df_overlap", category = "op_meaning", overlap = T)
  expect_no_error_with_defaults(dataset_overlap = "df_overlap", category = "op_meaning", only_overlaps = T)
  expect_no_error_with_defaults_3(dataset_overlap = "df_overlap", category = "category", overlap = T)
  expect_no_error_with_defaults_3(dataset_overlap = "df_overlap", category = "category", only_overlaps = T)
})

test_that("Error 07 some start dates are missing", {
  test_error_type(dataset = row_wise_dt(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                        "a",     "20100101",    "20200101",         "a",
                                        "a",             NA,    "20200101",         "a"), error_include = "Error 07")
  test_error_type(dataset = row_wise_dt(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                        "a",       20100101,      20200101,         "a",
                                        "a",             NA,      20200101,         "a"), error_include = "Error 07")
  test_error_type_3(dataset = row_wise_dt(~id, ~start_date,  ~end_date, ~category,
                                        "a",    "20100101", "20200101",       "a",
                                        "a",            NA, "20200101",       "a"), error_include = "Error 07")
  test_error_type_3(dataset = row_wise_dt(~id, ~start_date,  ~end_date, ~category,
                                        "a",      20100101,   20200101,       "a",
                                        "a",            NA,   20200101,       "a"), error_include = "Error 07")
  expect_no_error_with_defaults(dataset = row_wise_dt(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                                      "a",     "20100101",    "20200101",         "a",
                                                      "a",     "20110101",    "20200101",         "a"))
  expect_no_error_with_defaults(dataset = row_wise_dt(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                                      "a",       20100101,      20200101,         "a",
                                                      "a",       20110101,      20200101,         "a"))
  expect_no_error_with_defaults_3(dataset = row_wise_dt(~id, ~start_date,  ~end_date, ~category,
                                                      "a",    "20100101", "20200101",       "a",
                                                      "a",    "20110101", "20200101",       "a"))
  expect_no_error_with_defaults_3(dataset = row_wise_dt(~id, ~start_date,  ~end_date, ~category,
                                                      "a",      20100101,   20200101,       "a",
                                                      "a",      20110101,   20200101,       "a"))
})

test_that("Error 08 some start dates are not dates or ymd format", {
  test_error_type(dataset = tibble::tribble(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                            "a",   "2020010112",   "20200101",         "a"), error_include = "Error 08")
  test_error_type(dataset = tibble::tribble(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                            "a",     2020010112,     20200101,         "a"), error_include = "Error 08")
  test_error_type_3(dataset = tibble::tribble(~id, ~start_date, ~end_date, ~category,
                                              "a","2020010112","20200101",       "a"), error_include = "Error 08")
  test_error_type_3(dataset = tibble::tribble(~id, ~start_date, ~end_date, ~category,
                                              "a",  2020010112,  20200101,       "a"), error_include = "Error 08")
  expect_no_error_with_defaults(dataset = tibble::tribble(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",     "20200101",   "20200101",         "a"))
  expect_no_error_with_defaults(dataset = tibble::tribble(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",       20200101,     20200101,         "a"))
  expect_no_error_with_defaults(dataset = tibble::tribble(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",       "200101",     "200101",         "a"))
  expect_no_error_with_defaults(dataset = tibble::tribble(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                          "a",         200101,       200101,         "a"))
  expect_no_error_with_defaults_3(dataset = tibble::tribble(~id, ~start_date, ~end_date, ~category,
                                                            "a",  "20200101","20200101",       "a"))
  expect_no_error_with_defaults_3(dataset = tibble::tribble(~id, ~start_date, ~end_date, ~category,
                                                            "a",    20200101,  20200101,       "a"))
  expect_no_error_with_defaults_3(dataset = tibble::tribble(~id, ~start_date, ~end_date, ~category,
                                                            "a",    "200101",  "200101",       "a"))
  expect_no_error_with_defaults_3(dataset = tibble::tribble(~id, ~start_date, ~end_date, ~category,
                                                            "a",      200101,    200101,       "a"))
})

test_that("Error 09 some end dates are not dates or ymd format", {
  test_error_type(dataset = row_wise_dt(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                        "a",     "20100101",   "20200101",         "a",
                                        "a",     "20200101",   "20100101",         "a"), error_include = "Error 10")
  test_error_type_3(dataset = row_wise_dt(~id, ~start_date, ~end_date, ~category,
                                          "a",  "20100101","20200101",       "a",
                                          "a",  "20200101","20100101",       "a"), error_include = "Error 10")
  expect_no_error_with_defaults(dataset = row_wise_dt(~person_id, ~op_start_date, ~op_end_date, ~op_meaning,
                                                      "a",     "20200101",   "20200101",         "a"))
  expect_no_error_with_defaults_3(dataset = row_wise_dt(~id, ~start_date, ~end_date, ~category,
                                                        "a",  "20200101","20200101",       "a"))
})



test_that("Error 01 if specified column name is not in dataset", {
  test_error_type_2(id = "a", error_include = "Error 01")
  test_error_type_4(id = "a", error_include = "Error 01")
  expect_no_error_with_defaults_2(id = "person_id")
  expect_no_error_with_defaults_4(id = "id")

  test_error_type_2(start_date = "a", error_include = "Error 01")
  test_error_type_4(start_date = "a", error_include = "Error 01")
  expect_no_error_with_defaults_2(start_date = "op_start_date")
  expect_no_error_with_defaults_4(start_date = "start_date")

  test_error_type_2(end_date = "a", error_include = "Error 01")
  test_error_type_4(end_date = "a", error_include = "Error 01")
  expect_no_error_with_defaults_2(end_date = "op_end_date")
  expect_no_error_with_defaults_4(end_date = "end_date")

  test_error_type_2(category = "a", error_include = "Error 01")
  test_error_type_4(category = "a", error_include = "Error 01")
  # expect_no_error_with_defaults_2(category = NULL)
  expect_no_error_with_defaults_2(category = "op_meaning")
  expect_no_error_with_defaults_4(category = "category")
})

test_that("Error 02 if some start dates are missing", {
  test_data_tmp <- test_data_2
  test_data_tmp[1, "op_start_date"] <- NA
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 02")
})

test_that("Error 02 if some start dates are missing", {
  test_data_tmp <- test_data_2
  test_data_tmp[1, "op_start_date"] <- NA
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 02")
})

test_that("Error 03 if some end dates are missing", {
  test_data_tmp <- test_data_2
  test_data_tmp[1, "op_end_date"] <- NA
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 03")
})

test_that("Error 04 if some start dates are not interpretable as dates", {
  test_data_tmp <- test_data_2
  test_data_tmp$op_start_date <- as.integer(test_data_tmp$op_start_date)
  test_data_tmp[1, "op_start_date"] <- 123456
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 04")

  test_data_tmp[1, "op_start_date"] <- 20100101
  expect_no_error_with_defaults_2(dataset = test_data_tmp)

  test_data_tmp$op_start_date <- as.character(test_data_tmp$op_start_date)
  test_data_tmp[1, "op_start_date"] <- "123456"
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 04")

  test_data_tmp[1, "op_start_date"] <- "abcabc"
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 04")

  test_data_tmp[1, "op_start_date"] <- "20100101"
  expect_no_error_with_defaults_2(dataset = test_data_tmp)

  test_data_tmp$op_start_date <- lubridate::ymd(test_data_tmp$op_start_date)
  test_data_tmp[1, "op_start_date"] <- lubridate::ymd("20100101")
  expect_no_error_with_defaults_2(dataset = test_data_tmp)
})

test_that("Error 05 if some end dates are not interpretable as dates", {
  test_data_tmp <- test_data_2
  test_data_tmp$op_end_date <- as.integer(test_data_tmp$op_end_date)
  test_data_tmp[1, "op_end_date"] <- 123456
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 05")

  test_data_tmp[1, "op_end_date"] <- 20100101
  expect_no_error_with_defaults_2(dataset = test_data_tmp)

  test_data_tmp$op_end_date <- as.character(test_data_tmp$op_end_date)
  test_data_tmp[1, "op_end_date"] <- "123456"
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 05")

  test_data_tmp[1, "op_end_date"] <- "abcabc"
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 05")

  test_data_tmp[1, "op_end_date"] <- "20100101"
  expect_no_error_with_defaults_2(dataset = test_data_tmp)

  test_data_tmp$op_end_date <- lubridate::ymd(test_data_tmp$op_end_date)
  test_data_tmp[1, "op_end_date"] <- lubridate::ymd("20100101")
  expect_no_error_with_defaults_2(dataset = test_data_tmp)
})

test_that("Error 06 if some start dates are after their respective end dates", {
  test_data_tmp <- test_data_2
  test_data_tmp[1, "op_start_date"] <- "20200101"
  test_data_tmp[1, "op_end_date"] <- "20100101"
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 06")
})

test_that("Error 07 if a person has overlapping periods within categories", {
  test_data_tmp <- test_data_2
  test_data_tmp[1, "person_id"] <- "a2"
  test_data_tmp[1, "op_start_date"] <- "20110101"
  test_data_tmp[1, "op_end_date"] <- "20210101"
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 07")

  test_data_tmp <- row_wise_dt(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                               "a",  "20100101", "20191231",       "a",
                               "a",  "20200101", "20250101",       "a")
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 1, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 2, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 3, error_include = "Error 07")
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = 0)
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = -1)
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = -2)

  test_data_tmp <- row_wise_dt(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                               "a",  "20100101", "20191231",       "a",
                               "a",  "20200102", "20250101",       "a")
  expect_no_error_with_defaults_2(dataset = test_data_tmp)
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = 1)
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 2, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 3, error_include = "Error 07")
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = 0)
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = -1)
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = -2)

  test_data_tmp <- row_wise_dt(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                               "a",  "20100101", "20191231",       "a",
                               "a",  "20191230", "20250101",       "a")
  test_error_type_2(dataset = test_data_tmp, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 1, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 2, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 3, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = 0, error_include = "Error 07")
  test_error_type_2(dataset = test_data_tmp, gap_allowed = -1, error_include = "Error 07")
  expect_no_error_with_defaults_2(dataset = test_data_tmp, gap_allowed = -2)

})
