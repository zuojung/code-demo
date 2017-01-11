###################### Patch Data ################################
########################  Init  ##################################
library(RPostgreSQL)
library(hash)
source("crime_data_functions.R")
con <- dbConnect(PostgreSQL(), user = "zuojung", password = "",
                 dbname = "zuojung", host = "pg.stat.cmu.edu")

patch.file.input <-  read.csv("stdin", stringsAsFactors = FALSE,
                              strip.white = TRUE, header = TRUE)

neighborhood.table <- dbFetch(dbSendQuery(con, "SELECT * FROM neighborhood;"))
section.table <- dbFetch(dbSendQuery(con, "SELECT * FROM section;"))

neighborhood.hash <- hash(keys = neighborhood.table$hood, 
                          values = neighborhood.table$neighborhood_id)
section.hash <- hash(keys = section.table$code, 
                     values = section.table$section_id)

counter <- dbFetch(dbSendQuery(con, "SELECT * FROM counter;"))
counter <- counter[nrow(counter),]
step.counter <<- StepCounter$new("patch", counter)


########################  Patch  ##################################
MainPatchData(patch.file.input, neighborhood.hash, section.hash)
counter.result <- step.counter$Retrive()
UpdateCounter(counter.result)