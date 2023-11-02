#-------------------------------
# Example 4: ...

rm(list=ls(all.names=TRUE))

# set the directory where the file is saved as the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir <- setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dirinput <- thisdir
diroutput <- paste0(thisdir,"/output/")

# load packages
if (!require("data.table")) install.packages("data.table")
library(data.table)
if (!require("vetr")) install.packages("vetr")
library(vetr)
if (!require("utils")) install.packages("utils")
library(utils)
if (!require("lubridate")) install.packages("lubridate")
library(lubridate)
if (!require("tibble")) install.packages("tibble")
library(tibble)

#load input
data_example <- fread(paste0(thisdir,"/input.csv"), sep = ",")

#load function
source(paste0(thisdir,"/../../R/CreateSpells.R"))
source(paste0(thisdir,"/../../R/utils.R"))
source(paste0(thisdir,"/../../R/check_sanitize_inputs.R"))


df_spells_calculated <- CreateSpells(
  dataset = data_example,
  id = "person_id",
  start_date = "op_start_date",
  end_date = "op_end_date",
  category ="op_meaning",
  overlap = T,
  dataset_overlap = "df_overlap_calculated"
)

