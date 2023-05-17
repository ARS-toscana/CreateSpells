check_sanitize_inputs <- function(dataset, id, start_date, end_date, category = NULL, replace_missing_end_date = NULL,
                                  overlap = F, dataset_overlap = NA_character_, only_overlaps = F, gap_allowed = 1) {

  . <- NULL

  # Function to check if x is a date or is an input that ymd() can accept
  is.ymd_or_date <- function(x) {
    x <- tryCatch(lubridate::ymd(x[!is.na(x)]), error=function(e) F, warning=function(w) F)
    if (all(lubridate::is.Date(x))) x <- T
    return(x)
  }

  # Check if there are any missing dates
  token_missing_dates <- vetr::vet_token(!is.na(.),
                                         "%s is missing, please specify a valid date (Error 01)")

  # Check if x is/can be a date
  token_is_ymd_or_date <- vetr::vet_token(is.ymd_or_date(.),
                                          "%s should be a date or string/integer interpretable by lubridate::ymd (Error 02)")

  # Check if x is a column of dataset
  token_col <- vetr::vet_token(. %in% colnames(dataset),
                               paste("%s is not a column in", deparse(substitute(dataset)), "(Error 03)"))

  # Want to calculate overlap but no category specified
  token_exist_categories <- vetr::vet_token(!(isTRUE(.) && is.null(category)),
                                            "%s is set to TRUE, however the overlaps can not be computed as
                                            the category argument has not been specified (Error 04)")

  # Want to calculate overlap but insufficient categories
  token_n_categories <- vetr::vet_token(!(isTRUE(.) && length(unique(dataset[[category]])) < 2),
                                        "%s is set to TRUE, however the overlaps can not be computed as
                                        has less than two categories (Error 05)")

  # Check if any overlap is T or if dataset_overlap has default value
  token_overlap <- vetr::vet_token(I(is.na(.) || (isTRUE(overlap) || isTRUE(only_overlaps))),
                                   "it is set to %s, however neither overlap or only_overlaps
                                   argument are set to TRUE (Error 06)")

  # Check if dataset is a data.frame then transform it to data.table
  vetr::vetr(dataset = data.frame())
  dataset = data.table::as.data.table(dataset)

  vetr::vetr(
    id = character(1L) && token_col,
    start_date = character(1L) && token_col,
    end_date = character(1L) && token_col,
    category = NULL || character(1L) && token_col,
    replace_missing_end_date = NULL || (token_missing_dates && token_is_ymd_or_date),
    overlap = logical(1L) && token_exist_categories && token_n_categories,
    only_overlaps = logical(1L) && token_exist_categories && token_n_categories,
    dataset_overlap = character(1L) && token_overlap,
    gap_allowed = integer(1L)
  )

  # Check if there are any missing dates
  token_missing_start_dates <- vetr::vet_token(!is.na(.[[start_date]]),
                                               "Some start dates of %s are missing, please update those values or
                                               deleted the records (Error 07)")

  # Check if x is/can be a date
  token_is_ymd_or_start_date <- vetr::vet_token(is.ymd_or_date(.[[start_date]]),
                                                "All start dates in %s should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 08)")

  # Check if x is/can be a date
  token_is_ymd_or_end_date <- vetr::vet_token(is.ymd_or_date(.[[end_date]]),
                                              "All end dates in %s  should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 09)")

  # Check for periods with end date before start date
  vetr::vet(token_missing_start_dates && token_is_ymd_or_start_date && token_is_ymd_or_end_date, dataset, stop = T)

  # Check for periods with end date before start date
  token_impossible_period <- vetr::vet_token(all(.[[start_date]] <= .[[end_date]], na.rm = T),
                                             paste("Inside %s, there are observation period/s with",
                                                   deparse(substitute(start_date)),
                                                   "less than", deparse(substitute(end_date)), " (Error 10)"))
  vetr::vet(token_impossible_period, dataset, stop = T)

  return()
}


check_sanitize_inputs_2 <- function(dataset, id, start_date, end_date, category, gap_allowed) {

  . <- NULL

  # Function to check if x is a date or is an input that ymd() can accept
  is.ymd_or_date <- function(x) {
    x <- tryCatch(lubridate::ymd(x[!is.na(x)]), error=function(e) F, warning=function(w) F)
    if (all(lubridate::is.Date(x))) x <- T
    return(x)
  }

  # Function to check if dataset has overlaps within categories (unwanted)
  has.overlaps_within_categories <- function(dataset, id, start_date, end_date, category, gap_allowed) {
    dataset[, (end_date) := data.table::shift(get(..end_date)), by = c(id, category)]
    dataset[, (end_date) := get(..end_date) - gap_allowed]

    prev_env <- environment(NULL)
    return(nrow(dataset[!is.na(get(prev_env$end_date)) & get(prev_env$start_date) <= get(prev_env$end_date)]) == 0)
  }

  # Check if x is a column of dataset
  token_col <- vetr::vet_token(. %in% colnames(dataset),
                               paste("%s is not a column in", deparse(substitute(dataset)), "(Error 01)"))

  # Check if dataset is a data.frame then transform it to data.table
  vetr::vetr(dataset = data.frame())
  dataset = data.table::as.data.table(dataset)

  vetr::vetr(
    id = character(1L) && token_col,
    start_date = character(1L) && token_col,
    end_date = character(1L) && token_col,
    category = character(1L) && token_col,
    gap_allowed = integer(1L)
  )

  data.table::setorderv(dataset, c(id, start_date))

  # Check if there are any missing dates
  token_missing_start_dates <- vetr::vet_token(!is.na(.[[start_date]]),
                                               "Some start dates of %s are missing, please update those values or
                                               deleted the records (Error 02)")

  # Check if there are any missing dates
  token_missing_end_dates <- vetr::vet_token(!is.na(.[[end_date]]),
                                             "Some end dates of %s are missing, please update those values or
                                               deleted the records (Error 03)")

  # Check if x is/can be a date
  token_is_ymd_or_start_date <- vetr::vet_token(is.ymd_or_date(.[[start_date]]),
                                                "All start dates in %s should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 04)")

  # Check if x is/can be a date
  token_is_ymd_or_end_date <- vetr::vet_token(is.ymd_or_date(.[[end_date]]),
                                              "All end dates in %s  should be a date or string/integer
                                          interpretable by lubridate::ymd (Error 05)")

  # Check for periods with end date before start date
  vetr::vet(token_missing_start_dates && token_missing_end_dates && token_is_ymd_or_start_date && token_is_ymd_or_end_date,
            dataset, stop = T)

  dataset[, (start_date) := lubridate::ymd(get(..start_date))]
  dataset[, (end_date) := lubridate::ymd(get(..end_date))]

  # Check for periods with end date before start date
  token_impossible_period <- vetr::vet_token(all(.[[start_date]] <= .[[end_date]], na.rm = T),
                                             paste("Inside %s, there are observation period/s with",
                                                   deparse(substitute(start_date)),
                                                   "less than", deparse(substitute(end_date)), " (Error 06)"))
  vetr::vet(token_impossible_period, dataset, stop = T)

  # Check for overlapping periods with the same categories
  token_overlapping_period <- vetr::vet_token(has.overlaps_within_categories(., id, start_date, end_date,
                                                                             category, gap_allowed),
                                              "Inside %s, there are overlapping observation periods within categories (Error 07)")
  vetr::vet(token_overlapping_period, data.table::as.data.table(dataset), stop = T)

  return()
}
