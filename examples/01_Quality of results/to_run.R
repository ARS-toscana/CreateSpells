library(data.table)

source("R/CreateSpells.R")
source("examples/01_Quality of results/creation of test.R")
initial_df <- initial_df[lubridate::ymd(op_start_date) > lubridate::ymd(20190101), op_start_date := NA]
df_spells_calculated <- CreateSpells(
  dataset = as.data.frame(initial_df),
  id = "id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning",
  replace_missing_end_date = 20250101,
  gap_allowed = 1,
  overlap = T,
  dataset_overlap = "df_overlap_calculated"
)

if (all.equal(df_spells_calculated, df_spells)) {
  print("Spells have been calculated correctly")
} else {
  stop("Spells have been calculated incorrectly")
}

if (all.equal(df_overlap_calculated, df_overlap)) {
  print("Overlaps have been calculated correctly")
} else {
  stop("Overlaps have been calculated incorrectly")
}
