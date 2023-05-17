test_that("single observation no meaning", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20200101"),
    row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                "a",         1,           "20100101",          "20200101"))

  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                              "a",  "20100101", "20200101",      "a"),
    row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                "a",         1,           "20100101",          "20200101"))
})

test_that("single observation pass meaning", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                              "a",  "20100101", "20200101",      "a",
                                              category = "meaning"),
                   row_wise_dt(~person_id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",     "a",         1,           "20100101",          "20200101"))
})


test_that("simple overlap", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20110101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("meanings dependence", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                              "a",  "20100101", "20150101",      "a",
                                              "a",  "20110101", "20200101",      "a",
                                              category = "meaning"),
                   row_wise_dt(~person_id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",     "a",         1,           "20100101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                              "a",  "20100101", "20150101",      "a",
                                              "a",  "20110101", "20200101",      "b",
                                              category = "meaning"),
                   row_wise_dt(~person_id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",     "a",         1,           "20100101",          "20150101",
                               "a",     "b",         1,           "20110101",          "20200101"))
})

test_that("perfect overlap", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20100101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("subset overlap", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20110101", "20190101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("subset overlap with boundary matching", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20100101", "20110101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20200101",
                                              "a",  "20190101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("single day overlap", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("disjoint", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20110101",
                                              "a",  "20190101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20110101",
                               "a",         2,           "20190101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20190101", "20200101",
                                              "a",  "20100101", "20110101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20110101",
                               "a",         2,           "20190101",          "20200101"))
})

test_that("check gap_allowed", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150102", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150103", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20150101",
                               "a",         2,           "20150103",          "20200101"))

  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150102", "20200101", gap_allowed = 2),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150102", "20200101", gap_allowed = 0),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20150101",
                               "a",         2,           "20150102",          "20200101"))

  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20150101", "20200101", gap_allowed = -1),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20150101",
                               "a",         2,           "20150101",          "20200101"))

})

test_that("triple simple overlap", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20120101",
                                              "a",  "20110101", "20190101",
                                              "a",  "20180101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("simple overlap + subset", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20150101",
                                              "a",  "20120101", "20190101",
                                              "a",  "20140101", "20200101",
                                              "a",  "20180101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101"))
})

test_that("person_id independence", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20100101", "20200101",
                                              "b",  "20100101", "20200101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20200101",
                               "b",         1,           "20100101",          "20200101"))
})

test_that("person_id and meaning independence", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date, ~meaning,
                                              "a",  "20100101", "20200101",      "a",
                                              "b",  "20100101", "20200101",      "a",
                                              "a",  "20100101", "20200101",      "b",
                                              "b",  "20100101", "20200101",      "b",
                                              category = "meaning"),
                   row_wise_dt(~person_id,~meaning,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",     "a",         1,           "20100101",          "20200101",
                               "a",     "b",         1,           "20100101",          "20200101",
                               "b",     "a",         1,           "20100101",          "20200101",
                               "b",     "b",         1,           "20100101",          "20200101"))
})

test_that("reorder periods", {
  expect_identical(test_CreateSpells.internal(~person_id, ~op_start_date,  ~op_end_date,
                                              "a",  "20190101", "20200101",
                                              "a",  "20100101", "20110101"),
                   row_wise_dt(~person_id,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a",         1,           "20100101",          "20110101",
                               "a",         2,           "20190101",          "20200101",))
})

test_that("Arguments with same names or different ones", {

  expect_identical(test_CreateSpells.internal(~id, ~start_date,  ~end_date, ~category,
                                              "id", "20100101", "20200101","category",
                                              id = "id", start_date = "start_date", end_date = "end_date",
                                              category = "category"),
                   row_wise_dt(~id,  ~category,~num_spell,~entry_spell_category,~exit_spell_category,
                               "id","category",         1,           "20100101",          "20200101"))
})
