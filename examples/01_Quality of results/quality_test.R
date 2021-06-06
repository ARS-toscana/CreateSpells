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
