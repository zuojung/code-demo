##### Filter Data ######
# This file will:
# 1. Filter data per instruction
# 2. Counter ill-formed inputs and return reasonable error
# 3. Return the filtered data

Args = commandArgs(trailingOnly = TRUE)
file.input <-  read.csv(Args[1], stringsAsFactors = FALSE, strip.white = TRUE)
##################
library(RPostgreSQL)
con <- dbConnect(PostgreSQL(), user = "zuojung", password = "",
                 dbname = "zuojung", host = "pg.stat.cmu.edu")
source("crime_data_functions.R")

return.table = RepoFilter(file.input)
write.csv(return.table, row.names = FALSE)