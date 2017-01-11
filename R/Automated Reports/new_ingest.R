##### Ingest Data ######
# This file will:
# Read in the STDOUT from filter_data and transform and load into sql database
#
# 1. Robust handling of errors when trying to load data into sql database
#    a. When encountering errors, print
#    b. Record the error message reasonably and save it in error_log.txt file
#    c. Carry on
#
# 2. Ignore duplicated entries
#    a."When encountering duplicated entries, use exceptions or conditions
#       to catch the psql error and indicate the ID is duplicated and 
#       handle it approiately"
#
# 3. Match mispelled neighborhoods
#    a. Identify the neighborhood that are not in the current table
#    b. Try to match it with a existing neighborhoods in the table
#       1. Case in-sensitive
#       2. Use SQL to retrive a list of matching neighborhoods
#       3. If mutiple matching, generate and record warning message
#          and also record the choice of substitution
# 
# 4. (In Question) Update database operating log
#    This log will maintain the transaction that happened while executing
#    the log has:
#    Time, script, inputfile,
#    The number of - 
#    Crimes loaded in the database
#    Crimes Skipped due to invalid data
#    Crimes whose data was corrected
#    Crimes whose records were patched
#    Crimes whose records were not pached due to invalid data

########################  Init  ##################################
library(RPostgreSQL)
library(hash)
source("crime_data_functions.R")
con <- dbConnect(PostgreSQL(), user = "zuojung", password = "",
                 dbname = "zuojung", host = "pg.stat.cmu.edu")

ingest.file.input <-  read.csv("stdin", stringsAsFactors = FALSE,
                               strip.white = TRUE, header = TRUE)

neighborhood.table <- dbFetch(dbSendQuery(con, "SELECT * FROM neighborhood;"))
section.table <- dbFetch(dbSendQuery(con, "SELECT * FROM section;"))

neighborhood.hash <- hash(keys = neighborhood.table$hood, 
                          values = neighborhood.table$neighborhood_id)
section.hash <- hash(keys = section.table$code, 
                     values = section.table$section_id)

counter <- dbFetch(dbSendQuery(con, "SELECT * FROM counter;"))
counter <- counter[nrow(counter),]
step.counter <<- StepCounter$new("ingest", counter)

########################  Ingest  ##################################
# Process and upDate
MainIngestData(ingest.file.input, neighborhood.hash, section.hash)
counter.result <- step.counter$Retrive()
UpdateCounter(counter.result)