check_sanitize_inputs <- function(dataset, id, start_date, end_date, category = NULL, replace_missing_end_date = NULL,
                                  overlap=F, dataset_overlap = "df_overlap", only_overlaps=F, gap_allowed = 1) {

  # Function to check if x is a date or is an input that ymd() can accept
  is.ymd_or_date <- function(x) {
    x <- tryCatch(lubridate::ymd(x), error=function(e) F, warning=function(w) F)
    if (lubridate::is.Date(x)) x <- T
    return(x)
  }

  # Check if there are any missing dates
  token_missing_dates <- vetr::vet_token(any(is.na(.)),
                                         "it is set to default value %s, however since there are missing dates a custom value should be set")

  # Check if x is/can be a date
  token_replace <- vetr::vet_token(!(!is.ymd_or_date(.) && token_missing_dates),
                                   "%s shoulde be a date or string/integer interpretable by lubridate::ymd")

  # Check if x is a column of dataset
  token_col <- vetr::vet_token(. %in% colnames(dataset),
                               paste("%s is not a column in", deparse(substitute(dataset))))

  # Want to calculate overlap but no category specified
  token_exist_categories <- vetr::vet_token(!(isTRUE(.) && is.null(category)),
                                            "%s is set to TRUE, however the overlaps can not be computed as the category argument has not been specified")

  # Want to calculate overlap but insufficient categories
  token_n_categories <- vetr::vet_token(!(isTRUE(.) && length(unique(dataset[[category]])) < 2),
                                        "%s is set to TRUE, however the overlaps can not be computed as has less than two categories")

  # Check if any overlap is T or if dataset_overlap has default value
  token_overlap <- vetr::vet_token(I(. == "df_overlap" || (isTRUE(overlap) || isTRUE(only_overlaps))),
                                   "it is set to %s, however neither overlap or only_overlaps argument are set to TRUE")

  # Check if dataset is a data.frame then trasform it to data.table
  vetr::vetr(dataset = data.frame())
  dataset = data.table::as.data.table(dataset)

  vetr::vetr(
    id = character(1L) && token_col,
    start_date = character(1L) && token_col,
    end_date = character(1L) && token_col,
    category = NULL || character(1L) && token_col,
    replace_missing_end_date = NULL || token_replace,
    overlap = logical(1L) && token_exist_categories && token_n_categories,
    only_overlaps = logical(1L) && token_n_categories,
    dataset_overlap = character(1L) && token_overlap,
    gap_allowed = integer(1L)
  )

  # Check for periods with end date before start date
  token_impossible_period <- vetr::vet_token(all(.[[start_date]] < .[[end_date]], na.rm = T),
                                             paste("Inside %s, there observation period/s with",
                                                   deparse(substitute(start_date)),
                                                   "less than", deparse(substitute(end_date))))
  vetr::vet(token_impossible_period, dataset, stop = T)

  return(dataset)
}