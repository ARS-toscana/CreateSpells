load("test/initial_df.RData")
load("test/df_spells.RData")
load("test/df_overlap.RData")

df_spells_calculated <- CreateSpells(
  dataset = initial_df,
  id = "person_id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning",
  replace_missing_end_date = ymd(20250101),
  gap_allowed = 1,
  overlap = T,
  dataset_overlap = "df_overlap_calculated"
)

all.equal(df_spells_calculated, df_spells)
all.equal(df_overlap_calculated, df_overlap)
