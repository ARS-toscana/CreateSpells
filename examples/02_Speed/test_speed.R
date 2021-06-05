library(data.table)
source("R/CreateSpells_v14.R")

row_to_gen <- 15000000
seq_id <- seq_len(row_to_gen / 10)
set.seed(123)
generate_ids <- sample(seq_id, row_to_gen, replace = T)
generated_dates <- sample(seq.Date(Sys.Date(), Sys.Date() + 1000, by = "day"), row_to_gen, replace = T)
generated_integers <- sample(seq_len(2000), row_to_gen, replace = T)
generated_meanings <- sample(c("aaa", "bbb", "ccc"), row_to_gen, replace = T)
generated_origins <- sample(c("123", "456", "789"), row_to_gen, replace = T)

test_df <- data.table(person_id = generate_ids,
                      op_start_date = generated_dates,
                      op_end_date = generated_dates + generated_integers,
                      op_meaning = generated_meanings,
                      op_origin = generated_origins)

rm(generate_ids, generated_dates, generated_integers, generated_meanings, generated_origins)

test <-  CreateSpells(
  dataset = test_df,
  id = "person_id" ,
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning"
)

fasttime::fastPOSIXct("2010-01-01")

as.Date(generated_dates, "%Y%m%d")

bench::mark(
  lubridate::ymd(generated_dates),
  as.Date(generated_dates, generated_dates)
)
