library(data.table)

# source("R/CreateSpells_v13.R")
source("R/CreateSpells_v14.R")
source("examples/01_Quality of results/creation of test.R")

# oad("examples/01_Quality of results/initial_df.RData")
# load("examples/01_Quality of results/df_spells.RData")
# load("examples/01_Quality of results/df_overlap.RData")

df_spells_calculated <- CreateSpells(
  dataset = initial_df,
  id = "person_id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning",
  replace_missing_end_date = lubridate::ymd(20250101),
  gap_allowed = 1,
  overlap = T,
  dataset_overlap = "df_overlap_calculated"
)

if (all.equal(df_spells_calculated, df_spells)) {
  print("Spells have been calculated correctly")
}

if (all.equal(df_overlap_calculated, df_overlap)) {
  print("Overlaps have been calculated correctly")
}
