test_that("replace_missing_end_date functionality (and Warning 01)", {
  expect_warning(suppressMessages(test_data_preparation(~person_id, ~op_start_date,  ~op_end_date,
                                                        "a",  "20100101", "20200101",
                                                        "a",  "20100101",         NA)),
                 regexp = "Warning 01")
  expect_warning(suppressMessages(test_data_preparation_2(~person_id, ~start_date,  ~end_date,
                                                          "a",  "20100101", "20200101",
                                                          "a",  "20100101",         NA)),
                 regexp = "Warning 01")

  expect_identical(suppressMessages(test_data_preparation(~person_id, ~op_start_date,  ~op_end_date,
                                                          "a",  "20100101", "20200101",
                                                          "a",  "20100101",         NA,
                                                          replace_missing_end_date = 20150101)),
                   row_wise_dt(~person_id,~op_start_date,~op_end_date,
                               "a", "20100101","20200101",
                               "a", "20100101","20150101"))
  expect_identical(suppressMessages(test_data_preparation_2(~id, ~start_date,  ~end_date,
                                                            "a",  "20100101", "20200101",
                                                            "a",  "20100101",         NA,
                                                            replace_missing_end_date = 20150101)),
                   row_wise_dt(~id,~start_date, ~end_date,
                               "a", "20100101","20200101",
                               "a", "20100101","20150101"))

  expect_identical(suppressMessages(test_data_preparation(~person_id, ~op_start_date,  ~op_end_date,
                                                           "a",  "20100101", "20200101",
                                                           "a",  "20100101", "20190101",
                                                           replace_missing_end_date = 20150101)),
                   row_wise_dt(~person_id,~op_start_date, ~op_end_date,
                               "a", "20100101","20200101",
                               "a", "20100101","20190101"))
  expect_identical(suppressMessages(test_data_preparation_2(~id, ~start_date,  ~end_date,
                                                          "a",  "20100101", "20200101",
                                                          "a",  "20100101", "20190101",
                                                          replace_missing_end_date = 20150101)),
                   row_wise_dt(~id,~start_date, ~end_date,
                               "a", "20100101","20200101",
                               "a", "20100101","20190101"))

  expect_identical(suppressMessages(test_data_preparation(~person_id, ~op_start_date,  ~op_end_date,
                                                          "a",  "20100101", "20200101",
                                                          "a",  "20200101",         NA,
                                                          replace_missing_end_date = 20150101)),
                   row_wise_dt(~person_id,~op_start_date, ~op_end_date,
                               "a", "20100101","20200101"))
  expect_identical(suppressMessages(test_data_preparation_2(~id, ~start_date,  ~end_date,
                                                          "a",  "20100101", "20200101",
                                                          "a",  "20200101",         NA,
                                                          replace_missing_end_date = 20150101)),
                   row_wise_dt(~id,~start_date, ~end_date,
                               "a", "20100101","20200101"))
})

test_that("Other columns are kept", {

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                                        "a",  "20100101", "20200101",      "a"))
  tmp_result <- suppressMessages(data_preparation(dataset = tmp_data, start_date = "op_start_date",
                                                  end_date = "op_end_date", replace_missing_end_date = 20150101))
  expect_identical(colnames(tmp_result), colnames(tmp_data))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~start_date,  ~end_date, ~category,
                                                          "a",  "20100101", "20200101",      "a"))
  tmp_result <- suppressMessages(data_preparation(dataset = tmp_data, start_date = "start_date",
                                                    end_date = "end_date", replace_missing_end_date = 20150101))
  expect_identical(colnames(tmp_result), colnames(tmp_data))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                                        "a",  "20100101",         NA,      "a"))
  tmp_result <- suppressMessages(data_preparation(dataset = tmp_data, start_date = "op_start_date", end_date = "op_end_date",
                                                  replace_missing_end_date = 20150101))
  expect_identical(colnames(tmp_result), colnames(tmp_data))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~start_date,  ~end_date, ~category,
                                                        "a",  "20100101",         NA,      "a"))
  tmp_result <- suppressMessages(data_preparation(dataset = tmp_data, start_date = "start_date", end_date = "end_date",
                                                  replace_missing_end_date = 20150101))
  expect_identical(colnames(tmp_result), colnames(tmp_data))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                                        "a",  "20100101", "20200101",      "a",
                                                        "a",  "20200101",         NA,      "a"))
  tmp_result <- suppressMessages(data_preparation(dataset = tmp_data, start_date = "op_start_date", end_date = "op_end_date",
                                    replace_missing_end_date = 20150101))
  expect_identical(colnames(tmp_result), colnames(tmp_data))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~start_date,  ~end_date, ~category,
                                                        "a",  "20100101", "20200101",      "a",
                                                        "a",  "20200101",         NA,      "a"))
  tmp_result <- suppressMessages(data_preparation(dataset = tmp_data, start_date = "start_date", end_date = "end_date",
                                                  replace_missing_end_date = 20150101))
  expect_identical(colnames(tmp_result), colnames(tmp_data))
})

test_that("Arguments with same names or different ones", {
  expect_no_error(suppressMessages(test_data_preparation(~person_id, ~op_start_date,  ~op_end_date, ~op_meaning,
                                                         "person_id", "20100101", "20200101",         "a",
                                                         start_date = "op_start_date", end_date = "op_end_date")))
  expect_no_error(suppressMessages(test_data_preparation(~id, ~start_date,  ~end_date, ~category,
                                                         "id", "20100101", "20200101",         "a",
                                                         start_date = "start_date", end_date = "end_date")))
})



test_that("Test category argument", {

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "a",
                                                        "a",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "meaning")),
                   data.table::as.data.table(tibble::tribble(~person_id,~meaning,
                                                             "a",     "a",
                                                             "a",     "a")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "a",
                                                        "a",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "category")),
                   data.table::as.data.table(tibble::tribble(~id,~category,
                                                             "a",     "a",
                                                             "a",     "a")))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "a",
                                                        "a",      "a",
                                                        "a",      "b"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "meaning")),
                   data.table::as.data.table(tibble::tribble(~person_id,  ~meaning,
                                                             "a",       "a",
                                                             "a",       "a",
                                                             "a",       "b",
                                                             "a","_overall",
                                                             "a","_overall",
                                                             "a","_overall")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "a",
                                                        "a",      "a",
                                                        "a",      "b"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "category")),
                   data.table::as.data.table(tibble::tribble(~id,  ~category,
                                                             "a",       "a",
                                                             "a",       "a",
                                                             "a",       "b",
                                                             "a","_overall",
                                                             "a","_overall",
                                                             "a","_overall")))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "a",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "meaning")),
                   data.table::as.data.table(tibble::tribble(~person_id,  ~meaning,
                                                             "a",       "a",
                                                             "b",       "a")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "a",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "category")),
                   data.table::as.data.table(tibble::tribble(~id,  ~category,
                                                             "a",       "a",
                                                             "b",       "a")))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "b",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "meaning")),
                   data.table::as.data.table(tibble::tribble(~person_id,  ~meaning,
                                                             "a",       "b",
                                                             "b",       "a",
                                                             "a","_overall",
                                                             "b","_overall")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "b",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "category")),
                   data.table::as.data.table(tibble::tribble(~id,  ~category,
                                                             "a",       "b",
                                                             "b",       "a",
                                                             "a","_overall",
                                                             "b","_overall")))
})

test_that("Argument category might be NULL", {

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "a",
                                                        "a",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~person_id,~meaning,
                                                             "a",     "a",
                                                             "a",     "a")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "a",
                                                        "a",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~id,~category,
                                                             "a",     "a",
                                                             "a",     "a")))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "a",
                                                        "a",      "b"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~person_id,  ~meaning,
                                                             "a",       "a",
                                                             "a",       "b")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "a",
                                                        "a",      "b"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~id,  ~category,
                                                             "a",       "a",
                                                             "a",       "b")))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "a",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~person_id,  ~meaning,
                                                             "a",       "a",
                                                             "b",       "a")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "a",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~id,  ~category,
                                                             "a",       "a",
                                                             "b",       "a")))

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",      "b",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~person_id,  ~meaning,
                                                             "a",       "b",
                                                             "b",       "a")))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",      "b",
                                                        "b",      "a"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = NULL)),
                   data.table::as.data.table(tibble::tribble(~id,  ~category,
                                                             "a",       "b",
                                                             "b",       "a")))
})


test_that("Arguments with same names or different ones 2", {

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "person_id","meaning"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "meaning")),
                   data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                             "person_id","meaning")))

  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "id","category"))
  expect_identical(suppressMessages(data_preparation_2(dataset = tmp_data, category = "category")),
                   data.table::as.data.table(tibble::tribble(~id, ~category,
                                                             "id","category")))
})

test_that("Other columns are kept", {

  tmp_data <- data.table::as.data.table(tibble::tribble(~person_id, ~meaning,
                                                        "a",       "a"))
  tmp_result <- suppressMessages(data_preparation_2(dataset = tmp_data, category = "meaning"))
  expect_identical(colnames(tmp_result), colnames(tmp_data))
  tmp_data <- data.table::as.data.table(tibble::tribble(~id, ~category,
                                                        "a",       "a"))
  tmp_result <- suppressMessages(data_preparation_2(dataset = tmp_data, category = "category"))
  expect_identical(colnames(tmp_result), colnames(tmp_data))

})


