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
  return(temp_df)
}

test_data <- create_test_df()
test_data_2 <- test_data[!is.na(test_data$op_end_date), ]
