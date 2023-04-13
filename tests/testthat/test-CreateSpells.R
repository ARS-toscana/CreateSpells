test_that("single observation no meaning", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20200101"),
    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                "a",         1,           "20100101",          "20200101"))

  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date, ~meaning,
                                              "a",  "20100101", "20200101",      "a"),
    row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                "a",         1,           "20100101",          "20200101"))
})

test_that("single observation pass meaning", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date, ~meaning,
                                              "a",  "20100101", "20200101",      "a",
                                              category = "meaning"),
                   row_wise_dt(~id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",     "a",         1,           "20100101",          "20200101"))
})


test_that("simple overlap", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20110101", "20200101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("meanings dependence", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date, ~meaning,
                                              "a",  "20100101", "20150101",      "a",
                                              "a",  "20110101", "20200101",      "a",
                                              category = "meaning"),
                   row_wise_dt(~id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",     "a",         1,           "20100101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date, ~meaning,
                                              "a",  "20100101", "20150101",      "a",
                                              "a",  "20110101", "20200101",      "b",
                                              category = "meaning"),
                   row_wise_dt(~id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",     "a",         1,           "20100101",          "20150101",
                               "a",     "b",         1,           "20110101",          "20200101"))
})

test_that("perfect overlap", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20100101", "20200101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("subset overlap", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20110101", "20190101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("subset overlap with boundary matching", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20100101", "20110101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20190101", "20200101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("single day overlap", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150101", "20200101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("disjoint", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20110101",
                                              "a",  "20190101", "20200101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20110101",
                               "a",         2,           "20190101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20190101", "20200101",
                                              "a",  "20100101", "20110101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20110101",
                               "a",         2,           "20190101",          "20200101"))
})

test_that("check gap_allowed", {
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150102", "20200101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150103", "20200101"),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20150101",
                               "a",         2,           "20150103",          "20200101"))
  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150103", "20200101", gap_allowed = 2),
                   row_wise_dt(~id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})




# test_that("triple overlap", {
#   expect_identical(CreateSpells(dataset = row_wise_dt(
#     ~id,~start_date, ~end_date,~comment,
#     "a1", "20210101","20211231","single",
#     "a1", "20210601","20220601","single",
#     "a1", "20220101","20230101","single",
#   ), id = id, start_date = start_date , end_date = end_date),
#   row_wise_dt(
#     ~id,~start_date, ~end_date,~comment,
#     "a1", "20210101","20230101","single",
#   ))
# })
#
# test_that("inner overlap + simple + disjoint", {
#   expect_identical(CreateSpells(dataset = row_wise_dt(
#     ~id,~start_date, ~end_date,~comment,
#     "a1", "20210101","20220601","single",
#     "a1", "20210601","20213112","single",
#     "a1", "20220401","20220801","single",
#     "a1", "20223112","20230101","single",
#   ), id = id, start_date = start_date , end_date = end_date),
#   row_wise_dt(
#     ~id,~start_date, ~end_date,~comment,
#     "a1", "20210101","20220801","single",
#     "a1", "20223112","20230101","single",
#   ))
# })

