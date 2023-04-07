test_that("single observation no meaning", {
  expect_identical(CreateSpells(dataset = test_data[to_use_comment == "single"][, op_meaning := NULL],
                                id = "id", start_date = "op_start_date" , end_date = "op_end_date"),
                   row_wise_dt(~id ,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a1",         1,           "20100101",          "20200101"))
})

test_that("single observation no meaning", {
  expect_identical(CreateSpells(dataset = test_data[to_use_comment == "single"],
                                id = "id", start_date = "op_start_date" , end_date = "op_end_date"),
                   row_wise_dt(~id ,~num_spell,~entry_spell_category,~exit_spell_category,
                               "a1",         1,           "20100101",          "20200101"))
})

# row_wise_dt(
#   ~id,~start_date, ~end_date,~op_meaning,                  ~comment,
#   "a1", "20100101","20200101",        "a",                  "single",
#   "a2", "20100101","20200101",        "a",         "partial overlap",
#   "a2", "20150101",        NA,        "a",         "partial overlap",
#   "a3", "20100101","20200101",        "a",        "complete overlap",
#   "a3", "20150101","20160101",        "a",        "complete overlap",
#   "a4", "20100101","20150101",        "a",                "disjoint",
#   "a4", "20180101","20200101",        "a",                "disjoint",
#   "a5", "20100101","20200101",        "a",                   "mixed",
#   "a5", "20110101","20130101",        "a",                   "mixed",
#   "a5", "20140101","20160101",        "a",                   "mixed",
#   "a5", "20150101","20180101",        "a",                   "mixed",
#   "a5", "20170101","20210101",        "a",                   "mixed",
#   "a5", "20220101",        NA,        "a",                   "mixed",
#   "a6", "20100101","20200101",        "a",                 "mixed 2",
#   "a6", "20110101","20130101",        "a",                 "mixed 2",
#   "a6", "20140101","20210101",        "a",                 "mixed 2",
#   "b1", "20100101",        NA,        "b",          "not exist in a",
#   "a1", "20150101",        NA,        "b",  "exist, partial overlap",
#   "a2", "20120101","20200101",        "b", "exist, complete overlap",
#   "a3", "20210101",        NA,        "b",       "exist, no overlap",
#   "a4", "20160101","20170101",        "b", "exist, between disjoint",
#   "c1", "20100101","20200101",        "c",     "not exist in a or b",
#   "a1", "20100101","20200101",        "c",        "exist in a and b",
#   "b1", "20150101","20200101",        "c",         "exist only in b",
#
# )
