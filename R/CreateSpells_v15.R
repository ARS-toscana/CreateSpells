#' 'CreateSpells'
#'
#'
#'CreateSpells takes as input a dataset with multiple time windows per unit of observation. Multiple categories of time windows may be recorded per unit, and time windows of the same unit may overlap, even within the same category. The purpose of the function is creating a dataset where the time windows of the each person and category are disjoint (a time window which is disjoint form the others is called spell). Additionally, a category  '_overall' is added, where time windows are processed regardless of their category. As an option, overlap of pairs of categories are also processed: each pair will be associated to spells where both values are recorded.
#'
#' @param dataset name of dataset
#' @param id variable containing the identifier of the unit of observation
#' @param start_date variable containing the start date (the date must me ordered as Year Month Day)
#' @param end_date variable containing the end date (the date must me ordered as Year Month Day)
#' @param category (optional) categorical variable
#' @param replace_missing_end_date: (optional). When specified, it contains a date to replace end_date when it is missing.
#' @param overlap: (optional) default FALSE. If TRUE, overlaps of pairs of categories are processed as well.
#' @param dataset_overlap: (optional) if overlap TRUE, the name of the file containing the overlap dataset
#' @param only_overlaps: (optional) if only_overlaps TRUE, skip the calculation the spells
#' @param gap_allowed: (optional) Allowed gap in days between two observation periods after which they are counted as a different spell
#'
#' NOTE: Developed under R  4.0.3


CreateSpells <- function(dataset, id, start_date, end_date, category, replace_missing_end_date = NA_integer_,
                         overlap=F, dataset_overlap = "df_overlap", only_overlaps=F, gap_allowed = 1){
  library(data.table)
  library(vetr)

  is.ymd_or_date <- function(x) {
    x <- tryCatch(lubridate::ymd(x), error=function(e) F, warning=function(w) F)
    if (lubridate::is.Date(x)) x <- T
    return(x)
  }

  token_missing_dates <- vetr::vet_token(!(any(is.na(dataset[[end_date]])) && missing(.)),
                                         "it is set to default value %s, however since there are missing dates a custom value should be set")
  token_replace <- vetr::vet_token(is.ymd_or_date(.) && token_missing_dates,
                                   "%s shoulde be a date or string/integer interpretable by lubridate::ymd")
  token_col <- vetr::vet_token(. %in% colnames(dataset),
                               paste("%s is not a column in", deparse(substitute(dataset))))
  token_category <- vetr::vet_token(I(isTRUE(.) && length(unique(dataset[[category]])) >= 2 || isFALSE(.)),
                                    "%s is set to TRUE, however the overlaps can not be computed as has less than two categories")
  token_overlap <- vetr::vet_token(I(. == "df_overlap" || (isTRUE(overlap) || isTRUE(only_overlaps))),
                                    "it is set to %s, however neither overlap or only_overlaps argument are set to TRUE")

  vetr::vetr(
    dataset = data.frame(),
    id = character(1L) && token_col,
    start_date = character(1L) && token_col,
    end_date = character(1L) && token_col,
    category = character(1L) && token_col,
    replace_missing_end_date = token_replace,
    overlap = logical(1L) && token_category,
    only_overlaps = logical(1L) && token_category,
    dataset_overlap = character(1L) && token_overlap,
    gap_allowed = integer(1L)
  )

  token_impossible_period <- vetr::vet_token(all(.[[start_date]] < .[[end_date]], na.rm = T),
                                             paste("Inside %s, there observation period/s with", deparse(substitute(start_date)),
                                                   "less than", deparse(substitute(end_date))))
  vetr::vet(token_impossible_period, dataset, stop = T)

  if (only_overlaps==F) {
    dataset[, (start_date) := lubridate::ymd(get(..start_date))]
    dataset[, (end_date) := lubridate::ymd(get(..end_date))]

    if(!any(is.na(dataset[[start_date]]))) print("All start dates are not missing")
    else print("Some start dates are missing")
    if(!any(is.na(dataset[[end_date]]))) print("All end dates are not missing")
    else {print("Some end dates are missing")

      replace_missing_end_date <- lubridate::ymd(replace_missing_end_date)
      print(paste("Replacing missing end date/s with", replace_missing_end_date))
      dataset<-dataset[is.na(get(end_date)), (end_date) := replace_missing_end_date]

      #filter dataset
      dataset<-dataset[get(start_date) < get(end_date)]
    }


    #add level overall if category is given as input and has more than 1 category
    if (!missing(category)){
      if(length(unique(dataset[[category]]))>1) {
        tmp<-as.data.table(dataset)
        tmp[[category]]<-c("_overall")
        dataset <- rbindlist(list(dataset, tmp))
        rm(tmp)
        print("The level overall is added as the is more then one category")
      }
    }

    #group by and arrange the dataset
    if(!missing(category)) {
      setorderv(dataset, c(id, category, start_date, end_date))
    } else {
      setorderv(dataset, c(id, start_date, end_date))
    }

    #compute the number of spell
    year_1900 <- as.Date(lubridate::ymd(19000101))

    if (missing(category)) {
      grouping_vars <- id
      dataset<-dataset[, `:=`(row_id = rowid(get(..id)))]
    } else {
      grouping_vars <- c(id, category)
      dataset<-dataset[, row_id := rowid(get(..id), get(..category))]
    }

    dataset<-dataset[, `:=`(lag_end_date = fifelse(row_id > 1, shift(get(..end_date)), get(..end_date)))]
    dataset<-dataset[, `:=`(lag_end_date = as.integer(lag_end_date))]
    dataset<-dataset[, `:=`(lag_end_date = cummax(lag_end_date)), by = grouping_vars]
    dataset <- dataset[, `:=`(lag_end_date = as.Date(lag_end_date, "1970-01-01"))]
    dataset <- dataset[, `:=`(num_spell = fifelse(row_id > 1 & get(..start_date) <= lag_end_date + gap_allowed, 0, 1))]
    dataset<-dataset[, `:=`(num_spell = cumsum(num_spell)), by = grouping_vars]

    #group by num spell and compute min and max date for each one
    if(!missing(category)) {
      # dataset<-dataset[, c(entry_spell_category := min(get(..start_date)),exit_spell_category := max(get(..end_date))), by = c(id, category, "num_spell")]
      # dataset<-dataset[, .(entry_spell_category = min(get(..start_date)),
      #      exit_spell_category = max(get(..end_date))), keyby = .(id=get(..id), get(..category), num_spell)]
      myVector <- c(id,category,"num_spell","entry_spell_category","exit_spell_category")
      dataset <- unique(dataset[, c("entry_spell_category", "exit_spell_category") := list(min(get(..start_date)), max(get(..end_date))), by = c(id, category, "num_spell")][, ..myVector])
      #
    }else{
      myVector <- c(id,"num_spell","entry_spell_category","exit_spell_category")
      dataset<-unique(dataset[, .(entry_spell_category = min(get(..start_date)), exit_spell_category = max(get(..end_date))), by = c(id, "num_spell")][, ..myVector])
    }

    assign("output_spells_category", dataset)
  }

  #OPTIONAL SECTION REGARDING OVERLAPS

  if(overlap == T || only_overlaps == T){
    export_df <-data.table()
    dataset <- dataset[get(category) != "_overall",]

    #Create the list of pairs of categories
    permut <- RcppAlgos::comboGeneral(unique(dataset[[category]]), m = 2)

    #	For each pair of values A and B, create two temporary datasets
    #vec<-c(id)
    #dataset<-dataset[, .SD[length(unique(get(..category))) > 1], keyby = vec]

    for (i in seq_len(nrow(permut))) {

      p_1 <- permut[i, 1]
      p_2 <- permut[i, 2]

      if (is.na(p_1) | is.na(p_2)){
        next
      }

      ens_1 <- paste0("entry_spell_category_", p_1)
      exs_1 <- paste0("exit_spell_category_", p_1)
      ens_2 <- paste0("entry_spell_category_", p_2)
      exs_2 <- paste0("exit_spell_category_", p_2)

      outputA <- dataset[get(category) == p_1, ][, c("num_spell", category) := NULL]
      setnames(outputA, c("entry_spell_category", "exit_spell_category"), c(ens_1, exs_1))

      outputB <- dataset[get(category) == p_2, ][, c("num_spell", category) := NULL]
      setnames(outputB, c("entry_spell_category", "exit_spell_category"), c(ens_2, exs_2))
      #	Perform a join multi-to-multi of the two datasets

      CAT <- merge(outputA, outputB, by = c(id), all = T)
      CAT <- CAT[(get(ens_1) <= get(exs_2) + gap_allowed & get(exs_1) + gap_allowed >= get(ens_2)) | (get(ens_2) < get(exs_1) + gap_allowed & get(exs_2) + gap_allowed >= get(ens_1)), ]

      if (dim(CAT)[1] == 0) {
        next
      }

      CAT <- CAT[, `:=`(entry_spell_category = max(get(ens_1), get(ens_2)),
                        exit_spell_category = min(get(exs_1), get(exs_2))), by = id]
      CAT <- CAT[, (category) := paste(p_1, p_2, sep = "_")]
      # CAT<-CAT[!grepl("NA", category)]
      CAT <- CAT[order(c(id, "entry_spell_category"))][, c(..id, "entry_spell_category", "exit_spell_category", ..category)]
      CAT <- CAT[, num_spell := rowid(get(..id))]

      export_df <- rbind(export_df, CAT)
    }

    #save the second output
    #write_csv(export_df, path = paste0(dataset_overlap,".csv"))
    assign(dataset_overlap, export_df, envir = parent.frame())
  }

  if(only_overlaps == F){
    return(output_spells_category)
  }
}
