test_that("double overlap", {
  expect_identical(test_overlap.internal_2(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20190101",       "a",
                                         "a",  "20110101", "20200101",       "b"),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category, ~category,~num_spell,
                               "a",           "20110101",          "20190101",     "a_b",         1))
})

test_that("triple overlap", {
  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20190101",       "a",
                                         "a",  "20140101", "20160101",       "b",
                                         "a",  "20110101", "20200101",       "c"),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category, ~category,~num_spell,
                               "a",           "20140101",          "20160101",     "a_b",         1,
                               "a",           "20110101",          "20190101",     "a_c",         1,
                               "a",           "20140101",          "20160101",     "b_c",         1))
})

test_that("triple overlap, two categories", {
  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20190101",       "a",
                                         "a",  "20130101", "20160101",       "b",
                                         "a",  "20170101", "20180101",       "b"),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category, ~category,~num_spell,
                               "a",           "20130101",          "20160101",     "a_b",         1,
                               "a",           "20170101",          "20180101",     "a_b",         2))
})

test_that("two persons double overlap", {
  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20190101",       "a",
                                         "a",  "20110101", "20200101",       "b",
                                         "b",  "20100101", "20140101",       "a",
                                         "b",  "20160101", "20200101",       "c"),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category, ~category,~num_spell,
                               "a",           "20110101",          "20190101",     "a_b",         1))
})


test_that("Arguments with same names or different ones", {

  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,  ~category,
                                         "id",  "20100101", "20190101","category",
                                         "id",  "20110101", "20200101",       "b"),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category,   ~category,~num_spell,
                               "id",          "20110101",          "20190101","category_b",         1))
})


test_that("check gap_allowed", {

  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20150101",       "a",
                                         "a",  "20150102", "20200101",       "b"),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category,   ~category,~num_spell,
                               "id",          "20150101",          "20150102",       "a_b",         1))

  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20150101",       "a",
                                         "a",  "20150103", "20200101",       "b"),
                   data.table::as.data.table(NULL))

  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20150101",       "a",
                                         "a",  "20150103", "20200101",       "b", gap_allowed = 2),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category,   ~category,~num_spell,
                               "id",          "20150101",          "20150103",       "a_b",         1))

  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20150101",       "a",
                                         "a",  "20150102", "20200101",       "b", gap_allowed = 0),
                   data.table::as.data.table(NULL))

  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20150101",       "a",
                                         "a",  "20150101", "20200101",       "b", gap_allowed = -1),
                   data.table::as.data.table(NULL))

  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20150101",       "a",
                                         "a",  "20141231", "20200101",       "b", gap_allowed = -1),
                   row_wise_dt(~id,~entry_spell_category,~exit_spell_category,   ~category,~num_spell,
                               "id",          "20141231",          "20150101",       "a_b",         1))

})

test_that("reorder periods", {
  expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20100101", "20190101",       "a",
                                         "a",  "20110101", "20200101",       "b"),
                   test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
                                         "a",  "20110101", "20200101",       "a",
                                         "a",  "20100101", "20190101",       "b"))
})

# test_that("perfect overlap", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20200101",
#                                          "a",  "20100101", "20200101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101"))
# })
#
# test_that("subset overlap", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20200101",
#                                          "a",  "20110101", "20190101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101"))
# })
#
# test_that("subset overlap with boundary matching", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20200101",
#                                          "a",  "20100101", "20110101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101"))
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20200101",
#                                          "a",  "20190101", "20200101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101"))
# })
#
# test_that("single day overlap", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20150101",
#                                          "a",  "20150101", "20200101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101"))
# })
#
# test_that("disjoint", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20110101",
#                                          "a",  "20190101", "20200101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20110101",
#                                "a",         2,           "20190101",          "20200101"))
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20190101", "20200101",
#                                          "a",  "20100101", "20110101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20110101",
#                                "a",         2,           "20190101",          "20200101"))
# })
#

#
# test_that("triple simple overlap", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20120101",
#                                          "a",  "20110101", "20190101",
#                                          "a",  "20180101", "20200101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101"))
# })
#
# test_that("simple overlap + subset", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20150101",
#                                          "a",  "20120101", "20190101",
#                                          "a",  "20140101", "20200101",
#                                          "a",  "20180101", "20200101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101"))
# })
#
# test_that("id independence", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20100101", "20200101",
#                                          "b",  "20100101", "20200101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20200101",
#                                "b",         1,           "20100101",          "20200101"))
# })
#
# test_that("id and meaning independence", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~meaning,
#                                          "a",  "20100101", "20200101",      "a",
#                                          "b",  "20100101", "20200101",      "a",
#                                          "a",  "20100101", "20200101",      "b",
#                                          "b",  "20100101", "20200101",      "b",
#                                          category = "meaning"),
#                    row_wise_dt(~id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",     "a",         1,           "20100101",          "20200101",
#                                "a",     "b",         1,           "20100101",          "20200101",
#                                "b",     "a",         1,           "20100101",          "20200101",
#                                "b",     "b",         1,           "20100101",          "20200101"))
# })
#
# test_that("reorder periods", {
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date,
#                                          "a",  "20190101", "20200101",
#                                          "a",  "20100101", "20110101"),
#                    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "a",         1,           "20100101",          "20110101",
#                                "a",         2,           "20190101",          "20200101",))
# })
#
# test_that("Arguments with same names or different ones", {
#
#   expect_identical(test_overlap.internal(~id, ~start_date,  ~end_date, ~category,
#                                          "id", "20100101", "20200101","category",
#                                          category = "category"),
#                    row_wise_dt(~id,  ~category,~num_spell,~entry_spell_category,~exit_spell_category,
#                                "id","category",         1,           "20100101",          "20200101"))
# })
