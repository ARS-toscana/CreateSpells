library(data.table)

source("R/CreateSpells_v14.R")
source("examples/01_Quality of results/creation of test.R")

df_spells_calculated <- CreateSpells(
  dataset = initial_df,
  id = "id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning",
  replace_missing_end_date = lubridate::ymd(20250101),
  gap_allowed = 1,
  overlap = F,
  dataset_overlap = "df_overlap_calculated"
)

CreateSpells(
  dataset = copy(df_spells_calculated),
  id = "id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning",
  only_overlaps = T,
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
