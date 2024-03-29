library(data.table)
source("R/CreateSpells_v15.R")

seq_id <- seq_len(as.integer(row_to_gen / mean_n_obs_each_id))
set.seed(123)
generate_ids <- sample(seq_id, row_to_gen, replace = T)
generated_dates <- sample(seq.Date(Sys.Date(), Sys.Date() + 1000, by = "day"), row_to_gen, replace = T)
generated_integers <- sample(seq_len(2000), row_to_gen, replace = T)
generated_meanings <- sample(as.character(seq_len(n_meanings)), row_to_gen, replace = T)
generated_origins <- sample(as.character(seq_len(n_origins)), row_to_gen, replace = T)

test_df <- data.table(person_id = generate_ids,
                      op_start_date = format(generated_dates, "%Y%m%d"),
                      op_end_date = generated_dates + generated_integers,
                      op_meaning = generated_meanings,
                      op_origin = generated_origins)

rm(row_to_gen, seq_id, generate_ids, generated_dates, generated_integers, generated_meanings, generated_origins)

print(system.time(CreateSpells(
  dataset = test_df,
  id = "person_id" ,
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning"
)))

rm(test_df)
