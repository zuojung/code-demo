# Function Source
library(RPostgreSQL)
library(hash)
########################  Filter Data  ##################################
# Filter data per instruction
# 1. Only take offense 2.0 from the report_name column
#    a. Missing is counted as offense 2.0
#    b. Only take the offense 2.0 crimes that are included in the 
#       selected section of the penn law
# 2. And filter out those rows without zones
# 3. And filter out those rows without neighborhood
RepoFilter = function(df) {
  result = df
  # Change all empty reports to offense 2.0
  result[which(result[,"REPORT_NAME"] == ""),"REPORT_NAME"] = "OFFENSE 2.0"
  
  # Can directly query the code.list **IMPROVE**
  section.table.query <- dbSendQuery(con, "SELECT * FROM section;")
  section.table <- dbFetch(section.table.query)
  offense.code.list = section.table[, "code"]
  
  result = result[which(result[, "REPORT_NAME"] == "OFFENSE 2.0" &
                          result[, "SECTION"] %in% offense.code.list),]
  
  result = result[which(result[, "NEIGHBORHOOD"] != "" &
                          result[, "ZONE"] != ""),]
  
  result = result[which(!is.na(result[, "NEIGHBORHOOD"]) &
                          !is.na(result[, "ZONE"])),]
  return(result)
}
########################  Error Log  ##################################
#   Record the error message reasonably and save it in error_log.txt file
cat.error = function(msg) {
  cat(paste0(msg, "\n"), file = "error_log.txt", sep = " ",append = TRUE)
  print(msg)
}
############################ counter  ##################################
# Step counter Reference Class has two fields:
# They type of count (ingest vs patch), and the category it is counting
# because both ingest and patch both use counter, but tracks different counts
#
# The initialization will allow a counter to initialize based on the 
# current counter in SQL database, and the method update will tally 
# accordingly within the helper functions
#
# Retrive will grab the most current update, and we will use the upload
# function to update the counts in sql database
StepCounter = setRefClass("StepCounter", fields = c("type","counter"))
StepCounter$methods(
  initialize = function(type, counter) {
    type <<- type
    counter <<- counter
  },
  Update = function(field) {
    if (type == "ingest") {
      if (field == "load") {
        counter$total_loaded <<- counter$total_loaded + 1
      }
      else if (field == "skip") {
        counter$total_skipped <<- counter$total_skipped + 1
      }
      else if (field == "correct") {
        counter$total_corrected <<- counter$total_corrected + 1
      }
      else if (field == "unload") {
        counter$total_loaded <<- counter$total_loaded - 1
      }
    }
    else if (type == "patch") {
      if (field == "load") {
        counter$total_patched <<- counter$total_patched + 1
      }
      else if (field == "skip") {
        counter$total_not_patched <<- counter$total_not_patched + 1
      }
      else if (field == "unload") {
        counter$total_patched <<- counter$total_patched - 1
      }
    }
  },
  Retrive = function() {
    return(counter)
  }
)
########################  NeighborhoodMatch  ##################################
# Match mispelled neighborhoods
#    a. Identify the neighborhood that are not in the current table
#    b. Try to match it with a existing neighborhoods in the table
#       1. Case in-sensitive
#       2. Use SQL to retrive a list of matching neighborhoods
#       3. If mutiple matching, generate and record warning message
#          and also record the choice of substitution
NeighborhoodMatch <-  function(similiar.hood, hood.name, crime.id, 
                               neighborhood.hash) {
  match.count <- nrow(similiar.hood)
  
  if (match.count == 0) {
    # remove entry
    # ** IMPROVE LATER ** - add key into has map to 0
    # record the removal or addition
    result <- 0
    neighborhood.hash[[hood.name]] <- "Remove"
    error.message <- paste("Record",crime.id, 
                           "will be removed due to invalid neighborhood:",
                           hood.name, sep = " ")
    cat.error(error.message)
    step.counter$Update("skip")
  }
  else if (match.count == 1) {
    # rewrite neighborhood with sql table index
    # record the replacement
    result <- as.numeric(similiar.hood["neighborhood_id"])
    neighborhood.hash[[hood.name]] <- paste("Replace:",result,
                                            ":",similiar.hood["hood"],
                                            sep = "")
    update.message <- paste("Record",crime.id, 
                            "has its neighborhood replaced from:",
                            hood.name, "to", similiar.hood["hood"],
                            sep = " ")
    cat.error(update.message)
    step.counter$Update("correct")
  } else {
    # record the warning
    # choose the match that is closest to the current ||
    # the first choice (improve later)
    # rewrite neighborhood with sql table index
    # record the replacement
    result <- 0
    warning.message <- paste("Record",crime.id, 
                             "has mutiple neighborhood replacment options of:",
                             hood.name, sep = " ")
    cat.error(warning.message)
    neighborhood.hash[[hood.name]] <- "Warning"
    step.counter$Update("skip")
  }
  return(result)
}
#####################  NeighborhoodNameToIndex  ############################
#   Match Neighborhood name to ID index in database
#       find value matching key (neighborhood) in hash
#       if neighborhood of this row is not in hash table (is.na(value))
#           find entries in table in sql that are similiar to current key
#           Retrive neighborhood matching
#               if there are no matching
#                   (in Question) add neighborhood | remove entry
#                    record the removal or addition
#               elseif there is one match
#                   rewrite neighborhood with sql table index
#                   record the replacement
#               else (there are mutiple matches)
#                   record the warning
#                   choose the match that is closest to the current
#                   ||the first choice (improve later)
#                   rewrite neighborhood with sql table index
#                   record the replacement
#       else
#           rewrite neighborhood with has value matching key
NeighborhoodNameToIndex <- function(crime.incident, neighborhood.hash) {
  crime.id <- crime.incident["X_id"]
  hood.name <- as.character(crime.incident["NEIGHBORHOOD"])
  temp.value <- neighborhood.hash[[hood.name]]
  
  if (is.null(temp.value)) {
    temp.query <- gsub("REPLACE", hood.name,
                       "select * from neighborhood where LOWER(hood) like LOWER('%REPLACE%');")
    similiar.hood.contain <- dbFetch(dbSendQuery(con, temp.query))
    
    temp.query <- gsub("REPLACE", hood.name,
                       "select * from neighborhood where LOWER('REPLACE') 
                       like LOWER(format('%%%s%%', hood));")
    similiar.hood.substr <- dbFetch(dbSendQuery(con, temp.query))
    
    similiar.hood <- rbind(similiar.hood.contain, similiar.hood.substr)
    result <- NeighborhoodMatch(similiar.hood, hood.name, 
                                crime.id, neighborhood.hash)
  }
  else if (is.numeric(temp.value)) {
    result <- temp.value
  }
  else if (temp.value == "Remove") {
    error.message <- paste("Record",crime.id, 
                           "will be removed due to invalid neighborhood:",
                           hood.name, sep = " ")
    cat.error(error.message)
    result <- 0
    step.counter$Update("skip")
  }
  else if (temp.value == "Warning") {
    warning.message <- paste("Record",crime.id, 
                             "has mutiple neighborhood replacment options for:",
                             hood.name, sep = " ")
    cat.error(warning.message)
    result <- 0
    step.counter$Update("skip")
  }
  else if (grep("Replace",temp.value)) {
    update.message <- paste("Record",crime.id, 
                            "has its neighborhood replaced from:",
                            hood.name, "to",
                            (strsplit(temp.value,":")[[1]][3]),
                            sep = " ")
    cat.error(update.message)
    result <- as.numeric(strsplit(temp.value,":")[[1]][2])
    step.counter$Update("correct")
  }
  return(result)
}
#####################  SectionToIndex  ############################
# Match Section of the Law name to ID index in database
SectionToIndex <- function(crime.incident, section.hash) {
  crime.section <- as.character(crime.incident["SECTION"])
  result <- as.numeric(section.hash[[crime.section]])
  return(result)
}
#####################  Main Ingest Data  ############################
# Procedure Walkthrough
#
# use neighborhoods table form database to construct hash table
# use section table form database to construct hash table
#
# For every entry of the new data
#
#    if the data contains missing information
#        discard and record (STEP-1)
# 
#    else - **can be done seperately for function clarity**
#       NeighborhoodNameToIndex
#       SectionToIndex 
#
#    Try submit the entry to the sql database
#         record error
MainIngestData <- function(df, neighborhood.hash, section.hash) {
  for (index in 1:nrow(df)) {
    df[index, "NEIGHBORHOOD"] <- NeighborhoodNameToIndex(df[index,], 
                                                         neighborhood.hash)
    df[index, "SECTION"] <- SectionToIndex(df[index,], section.hash)
    if (df[index, "NEIGHBORHOOD"] != 0) {
      tryCatch({
        query <- paste("insert into crime (crime_id, report_name, ",
                       "section_id, description, arrest_time, address, ",
                       "neighborhood_id, zone) values (",
                       (df[index, "X_id"]),", '",
                       (df[index, "REPORT_NAME"]),"', ",
                       (df[index, "SECTION"]),", '",
                       (df[index, "DESCRIPTION"]),"', '",
                       (df[index, "ARREST_TIME"]),"', '",
                       (df[index, "ADDRESS"]),"', ",
                       (df[index, "NEIGHBORHOOD"]),", ",
                       (df[index, "ZONE"]),
                       ")", sep = "")
        dbSendQuery(con, query)
        # counter$total_loaded <<- counter$total_loaded + 1
        step.counter$Update("load")
        
      }, error = function(e){
        # ** IMPROVE LATER ** - readable error log
        cat.error(paste("Report", df[index,"X_id"], "has Error:",
                        conditionMessage(e)))
        # counter$total_loaded <<- counter$total_loaded
        step.counter$Update("unload")
      })
    }
  }
}
#####################  Main Ingest Data  ############################
# Ingesting Patching Data
# For every element in the patch row
# Transfrom neighborhood and filter
# Transform section
# if neighborhood != -
#   send query to see if the ID exists
#   if it does, remove the ID
#   insert the new ID
MainPatchData <- function(df, neighborhood.hash, section.hash) {
  for (index in 1:nrow(df)) {
    df[index, "NEIGHBORHOOD"] <- NeighborhoodNameToIndex(df[index,], 
                                                         neighborhood.hash)
    df[index, "SECTION"] <- SectionToIndex(df[index,], section.hash)
    if (df[index, "NEIGHBORHOOD"] != 0) {
      tryCatch({
        if.id.exist.query <-  paste("select crime_id from crime where crime_id =",
                                    df[index, "X_id"], ";")
        temp.crime.id  <-  dbFetch(dbSendQuery(con, if.id.exist.query))
        if (nrow(temp.crime.id) == 1) {
          del.query = paste("delete from crime where crime_id =",
                            df[index, "X_id"], ";")
          dbSendQuery(con, del.query)
        }
        else if (nrow(temp.crime.id) > 1) {
          cat(paste("Fatal Error:", temp.crime.id, 
                    "has mutiple cases in database"))
        }
        query <- paste("insert into crime (crime_id, report_name, ",
                       "section_id, description, arrest_time, address, ",
                       "neighborhood_id, zone) values (",
                       (df[index, "X_id"]),", '",
                       (df[index, "REPORT_NAME"]),"', ",
                       (df[index, "SECTION"]),", '",
                       (df[index, "DESCRIPTION"]),"', '",
                       (df[index, "ARREST_TIME"]),"', '",
                       (df[index, "ADDRESS"]),"', ",
                       (df[index, "NEIGHBORHOOD"]),", ",
                       (df[index, "ZONE"]),
                       ")", sep = "")
        dbSendQuery(con, query)
        # counter$total_patched <<- counter$total_patched + 1
        step.counter$Update("load")
      }, error =  function(e){
        cat.error(paste("Report", df[index,"X_id"], "has Error:",
                        conditionMessage(e)))
        # counter$total_patched <<- counter$total_patched - 1
        step.counter$Update("unload")
      })
    }
  }
}
###################### UpdateCounter #############################
# Update database operating log
#    This log will maintain the transaction that happened while executing
#    the log has:
#    Time, script, inputfile,
#    The number of - 
#    Crimes loaded in the database
#    Crimes Skipped due to invalid data
#    Crimes whose data was corrected
#    Crimes whose records were patched
#    Crimes whose records were not pached due to invalid data
UpdateCounter <- function(counter) {
  counter.query <- paste("insert into counter (total_loaded, total_skipped, ",
                         "total_corrected, total_patched, ",
                         "total_not_patched) values (",
                         (counter[1, "total_loaded"]),", ",
                         (counter[1, "total_skipped"]),", ",
                         (counter[1, "total_corrected"]),", ",
                         (counter[1, "total_patched"]),", ",
                         (counter[1, "total_not_patched"]),
                         ")", sep = "")
  dbSendQuery(con, counter.query)
}