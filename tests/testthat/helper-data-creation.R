create_test_df <- function() {
  temp_df <- tibble::tribble(
    ~id, ~op_start_date,~op_end_date,~op_meaning,           ~to_use_comment,
    #---|--------------|------------|-----------|--------------------------|
    "a1",    "20100101",  "20200101",        "a",                  "single",
    "a2",    "20100101",  "20200101",        "a",         "partial overlap",
    "a2",    "20150101",          NA,        "a",         "partial overlap",
    "a3",    "20100101",  "20200101",        "a",        "complete overlap",
    "a3",    "20150101",  "20160101",        "a",        "complete overlap",
    "a4",    "20100101",  "20150101",        "a",                "disjoint",
    "a4",    "20180101",  "20200101",        "a",                "disjoint",
    "a5",    "20100101",  "20200101",        "a",                   "mixed",
    "a5",    "20110101",  "20130101",        "a",                   "mixed",
    "a5",    "20140101",  "20160101",        "a",                   "mixed",
    "a5",    "20150101",  "20180101",        "a",                   "mixed",
    "a5",    "20170101",  "20210101",        "a",                   "mixed",
    "a5",    "20220101",          NA,        "a",                   "mixed",
    "a6",    "20100101",  "20200101",        "a",                 "mixed 2",
    "a6",    "20110101",  "20130101",        "a",                 "mixed 2",
    "a6",    "20140101",  "20210101",        "a",                 "mixed 2",
    "b1",    "20100101",          NA,        "b",          "not exist in a",
    "a1",    "20150101",          NA,        "b",  "exist, partial overlap",
    "a2",    "20120101",  "20200101",        "b", "exist, complete overlap",
    "a3",    "20210101",          NA,        "b",       "exist, no overlap",
    "a4",    "20160101",  "20170101",        "b", "exist, between disjoint",
    "c1",    "20100101",  "20200101",        "c",     "not exist in a or b",
    "a1",    "20100101",  "20200101",        "c",        "exist in a and b",
    "b1",    "20150101",  "20200101",        "c",         "exist only in b"
  )
  return(data.table::as.data.table(temp_df))
}

test_data <- create_test_df()

create_test_df_1 <- function() {
  temp_df <- tibble::tribble(
    ~id, ~st,~en,~meaning,           ~to_use_comment,
    #---|---|---|--------|--------------------------|
    "a1", 10, 20,      "a",                  "single",
    "a2", 10, 20,      "a",         "partial overlap",
    "a2", 15, 25,      "a",         "partial overlap",
    "aa", 10, 20,      "a",               "identical",
    "aa", 10, 20,      "a",               "identical",
    "a3", 10, 20,      "a",                  "subset",
    "a3", 15, 16,      "a",                  "subset",
    "a4", 10, 15,      "a",                "disjoint",
    "a4", 18, 20,      "a",                "disjoint",
    "a5", 10, 20,      "a",                   "mixed",
    "a5", 11, 13,      "a",                   "mixed",
    "a5", 14, 16,      "a",                   "mixed",
    "a5", 15, 18,      "a",                   "mixed",
    "a5", 17, 21,      "a",                   "mixed",
    "a5", 22, NA,      "a",                   "mixed",
    "a6", 10, 20,      "a",                 "mixed 2",
    "a6", 11, 13,      "a",                 "mixed 2",
    "a6", 14, 21,      "a",                 "mixed 2",
    "b1", 10, NA,      "b",          "not exist in a",
    "a1", 15, NA,      "b",  "exist, partial overlap",
    "a2", 12, 20,      "b", "exist, complete overlap",
    "a3", 21, NA,      "b",       "exist, no overlap",
    "a4", 16, 17,      "b", "exist, between disjoint",
    "c1", 10, 20,      "c",     "not exist in a or b",
    "a1", 10, 20,      "c",        "exist in a and b",
    "b1", 15, 20,      "c",         "exist only in b"
  )
  return(data.table::as.data.table(temp_df))
}

test_data_1 <- create_test_df_1()
test_data_1 <- test_data_1[!(is.na(st)), st := as.integer(paste0("20", st, "0101"))][, st := lubridate::ymd(st)]
test_data_1 <- test_data_1[!(is.na(en)), en := as.integer(paste0("20", en, "0101"))][, en := lubridate::ymd(en)]
