create_test_df <- function() {
  temp_df <- tibble::tribble(
    ~person_id,~op_start_date,~op_end_date,~op_meaning,                  ~comment,
    #---------|--------------|------------|-----------|--------------------------|
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
          "b1",    "20150101",  "20200101",        "c",         "exist only in b",

  )
  return(data.table::as.data.table(temp_df))
}

# df_spells_calculated <- CreateSpellsV13(
#   dataset = initial_df,
#   id = "person_id",
#   start_date = "op_start_date",
#   end_date = "op_end_date",
#   category ="op_meaning",
#   replace_missing_end_date = lubridate::ymd(20250101),
#   gap_allowed = 1,
#   overlap = T,
#   dataset_overlap = "df_overlap_calculated"
# )

create_df_spells <- function() {
  temp_df <- tibble::tribble(
    ~person_id,~op_meaning,~num_spell,~entry_spell_category,~exit_spell_category,
    #---------|-----------|----------|---------------------|--------------------|
          "a1", "_overall",         1,           "20100101",          "20250101",
          "a1",        "a",         1,           "20100101",          "20200101",
          "a1",        "b",         1,           "20150101",          "20250101",
          "a1",        "c",         1,           "20100101",          "20200101",
          "a2", "_overall",         1,           "20100101",          "20250101",
          "a2",        "a",         1,           "20100101",          "20250101",
          "a2",        "b",         1,           "20120101",          "20200101",
          "a3", "_overall",         1,           "20100101",          "20200101",
          "a3", "_overall",         2,           "20210101",          "20250101",
          "a3",        "a",         1,           "20100101",          "20200101",
          "a3",        "b",         1,           "20210101",          "20250101",
          "a4", "_overall",         1,           "20100101",          "20150101",
          "a4", "_overall",         2,           "20160101",          "20170101",
          "a4", "_overall",         3,           "20180101",          "20200101",
          "a4",        "a",         1,           "20100101",          "20150101",
          "a4",        "a",         2,           "20180101",          "20200101",
          "a4",        "b",         1,           "20160101",          "20170101",
          "a5", "_overall",         1,           "20100101",          "20210101",
          "a5", "_overall",         2,           "20220101",          "20250101",
          "a5",        "a",         1,           "20100101",          "20210101",
          "a5",        "a",         2,           "20220101",          "20250101",
          "a6", "_overall",         1,           "20100101",          "20210101",
          "a6",        "a",         1,           "20100101",          "20210101",
          "b1", "_overall",         1,           "20100101",          "20250101",
          "b1",        "b",         1,           "20100101",          "20250101",
          "b1",        "c",         1,           "20150101",          "20200101",
          "c1", "_overall",         1,           "20100101",          "20200101",
          "c1",        "c",         1,           "20100101",          "20200101",
  )
  temp_df<- data.table::as.data.table(temp_df)
  temp_df[, entry_spell_category := lubridate::ymd(entry_spell_category)]
  temp_df[, exit_spell_category := lubridate::ymd(exit_spell_category)]
  return(temp_df)
}

create_df_overlap <- function() {
  temp_df <- tibble::tribble(
    ~person_id,~entry_spell_category,~exit_spell_category,~op_meaning,~num_spell,
    #---------|---------------------|--------------------|-----------|----------|
          "a1",           "20150101",          "20200101",      "a_b",         1,
          "a2",           "20120101",          "20200101",      "a_b",         1,
          "a1",           "20100101",          "20200101",      "a_c",         1,
          "a1",           "20150101",          "20200101",      "b_c",         1,
          "b1",           "20150101",          "20200101",      "b_c",         1,
  )
  temp_df<- data.table::as.data.table(temp_df)
  temp_df[, entry_spell_category := lubridate::ymd(entry_spell_category)]
  temp_df[, exit_spell_category := lubridate::ymd(exit_spell_category)]
  return(temp_df)
}

initial_df <- create_test_df()
df_spells <- create_df_spells()
df_overlap <- create_df_overlap()


#
# df_spells <- CreateSpells(
#   dataset = initial_df,
#   id = "person_id",
#   start_date = "op_start_date",
#   end_date = "op_end_date",
#   category ="op_meaning",
#   replace_missing_end_date = lubridate::ymd(20250101),
#   gap_allowed = 1,
#   overlap = T
# )

# save(initial_df, file = "examples/01_Quality of results/initial_df.RData")
# save(df_spells, file = "examples/01_Quality of results/df_spells.RData")
# save(df_overlap, file = "examples/01_Quality of results/df_overlap.RData")
