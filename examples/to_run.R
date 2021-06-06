progressr::handlers(global = TRUE)
progressr::handlers(progressr::handler_progress(format = "[:bar] :percent :message"))

options(progressr.clear = FALSE)

source("examples/01_Quality of results/to_run.R")

source("examples/02_Speed/to_run.R")

