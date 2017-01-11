# We will test the function that retrives information from psql
# For certain cases where we will compare(exisiting neighbourhoods)
# We create test after loading in crime_base

# For patching
# The only difference lies in the final execution????

# And after completetion of tests, we should clean the tables and run
# all reorganized code for sanity check

########################### Tests ###################################
library(RPostgreSQL)
library(testthat)
library(hash)
source("crime_data_functions.R")
con <- dbConnect(PostgreSQL(), user = "zuojung", password = "J10bE81xdRd242p",
                 dbname = "zuojung", host = "pg.stat.cmu.edu")

# Function Source

test_that("RepoFilter Filters Correctly", {
  section.table.query <- dbSendQuery(con, "SELECT * FROM section;")
  section.table <- dbFetch(section.table.query)
  
  tests <- list(test.1 = read.csv(file = "data/crime-base.csv"),
                test.2 = read.csv(file = "data/crime-week-1.csv"),
                test.3 = read.csv(file = "data/crime-week-2.csv"),
                test.4 = read.csv(file = "data/crime-week-3.csv"),
                test.5 = read.csv(file = "data/crime-week-4.csv"),
                test.6 = read.csv(file = "data/crime-week-1-patch.csv"),
                test.7 = read.csv(file = "data/crime-week-2-patch.csv"),
                test.8 = read.csv(file = "data/crime-week-3-patch.csv"),
                test.9 = read.csv(file = "data/crime-week-4-patch.csv"))
  
  for (df in tests) {
    test.filter.data <- RepoFilter(df)
    # All section in section table
    expect_true(all(test.filter.data$SECTION %in% section.table$code))
    # All Report Name is OFFENSE 2.0
    expect_true(all(test.filter.data$REPORT_NAME == "OFFENSE 2.0"))
    # All Neighborhood and Zone are not empty string
    expect_true(all(test.filter.data$NEIGHBORHOOD != ""))
    expect_true(all(test.filter.data$ZONE != ""))
    # All Neighborhood and Zone are not NA
    expect_true(all(!is.na(test.filter.data$NEIGHBORHOOD)))
    expect_true(all(!is.na(test.filter.data$ZONE)))
  }
})

test_that("Counter Updates Correctly", {
  counter <- dbFetch(dbSendQuery(con, "SELECT * FROM counter;"))
  counter <- counter[nrow(counter),]

  test.type.1 = "ingest"
  test.counter.1 = StepCounter$new(test.type.1, counter)
  
  test.counter.1$Update("load")
  test.result = test.counter.1$Retrive()
  expect_equal(test.result$total_loaded, (counter$total_loaded + 1))
  
  test.counter.1$Update("skip")
  test.result = test.counter.1$Retrive()
  expect_equal(test.result$total_skipped, (counter$total_skipped + 1))
  
  test.counter.1$Update("correct")
  test.result = test.counter.1$Retrive()
  expect_equal(test.result$total_corrected, (counter$total_corrected + 1))
  
  test.counter.1$Update("unload")
  test.result = test.counter.1$Retrive()
  expect_equal(test.result$total_loaded, (counter$total_loaded))
  
  test.type.2 = "patch"
  test.counter.2 = StepCounter$new(test.type.2, counter)
  
  test.counter.2$Update("load")
  test.result = test.counter.2$Retrive()
  expect_equal(test.result$total_patched, (counter$total_patched + 1))
  
  test.counter.2$Update("skip")
  test.result = test.counter.2$Retrive()
  expect_equal(test.result$total_not_patched, (counter$total_not_patched + 1))
  
  test.counter.2$Update("correct")
  test.result = test.counter.2$Retrive()
  expect_equal(test.result$total_corrected, (counter$total_corrected))
  
  test.counter.2$Update("unload")
  test.result = test.counter.2$Retrive()
  expect_equal(test.result$total_patched, (counter$total_patched))
})

test_that("NeighborhoodMatch Matches Misspelled correctly", {
  
  counter <- dbFetch(dbSendQuery(con, "SELECT * FROM counter;"))
  counter <- counter[nrow(counter),]
  step.counter <<- StepCounter$new("ingest", counter)
  
  neighborhood.table <- dbFetch(dbSendQuery(con, "SELECT * FROM neighborhood;"))
  neighborhood.hash <- hash(keys = neighborhood.table$hood, 
                            values = neighborhood.table$neighborhood_id)
  test.reuslt <- list()
  tests <- list(
    # Expect Remove - match.count == 0
    hood.name = "Centralas",
    hood.name = "Deer",
    
    # Expect Replace - match.count == 1
    hood.name = "Allentow",
    hood.name = "Esplen As",
    
    # Expect Mutiple - else
    hood.name = "Central",
    hood.name = "Squ"
  )
  for (case in tests) {
    hood.name = case
    temp.query <- gsub("REPLACE", hood.name,
                       "select * from neighborhood where LOWER(hood) like LOWER('%REPLACE%');")
    similiar.hood.contain <- dbFetch(dbSendQuery(con, temp.query))
    
    temp.query <- gsub("REPLACE", hood.name,
                       "select * from neighborhood where LOWER('REPLACE') 
                       like LOWER(format('%%%s%%', hood));")
    similiar.hood.substr <- dbFetch(dbSendQuery(con, temp.query))
    similiar.hood <- rbind(similiar.hood.contain, similiar.hood.substr)
    
    test.reuslt[[hood.name]] <- NeighborhoodMatch(similiar.hood, hood.name,
                                20000, neighborhood.hash)
  }
  # Check Results
  expect_equal(test.reuslt$Centralas, 0)
  expect_equal(test.reuslt$Deer, 0)
  expect_equal(test.reuslt$Allentow, 147)
  expect_equal(test.reuslt$`Esplen As`, 148)
  expect_equal(test.reuslt$Central, 0)
  expect_equal(test.reuslt$Squ, 0)
  
  # Check Hash Table
  expect_equal(neighborhood.hash[["Centralas"]], "Remove")
  expect_equal(neighborhood.hash[["Deer"]], "Remove")
  expect_equal(neighborhood.hash[["Allentow"]], "Replace:147:Allentown")
  expect_equal(neighborhood.hash[["Esplen As"]], "Replace:148:Esplen")
  expect_equal(neighborhood.hash[["Central"]], "Warning")
  expect_equal(neighborhood.hash[["Squ"]], "Warning")
  
  # Check counter
  test.counter.result = step.counter$Retrive()
  expect_equal(test.counter.result$total_skipped, 
               (counter$total_skipped + 4))
  expect_equal(test.counter.result$total_corrected, 
               (counter$total_corrected + 2))
})

test_that("NeighborhoodNameToIndex works correctly", {
  counter <- dbFetch(dbSendQuery(con, "SELECT * FROM counter;"))
  counter <- counter[nrow(counter),]
  step.counter <<- StepCounter$new("ingest", counter)
  
  neighborhood.table <- dbFetch(dbSendQuery(con, "SELECT * FROM neighborhood;"))
  neighbor.hash <- hash(keys = neighborhood.table$hood, 
                            values = neighborhood.table$neighborhood_id)
  
  crime.sample <- as.data.frame(cbind(c(1:12), rep(c("Centralas", "Deer", "Allentow",
                                                "Esplen As", "Central", "Squ"), 2)),
                               stringsAsFactors = FALSE)
  colnames(crime.sample) <- c("X_id", "NEIGHBORHOOD")
  
  result <- rep(NA, nrow(crime.sample))
  for (index in 1:nrow(crime.sample)) {
    result[index] <-  NeighborhoodNameToIndex(crime.incident = crime.sample[index,], 
                                              neighborhood.hash = neighbor.hash)
  }
  
  expect_equal(result[1:6], result[7:12])
  
  step.counter.result <- step.counter$Retrive()
  expect_equal(step.counter.result$total_skipped,
               (counter$total_skipped + 8))
  expect_equal(step.counter.result$total_corrected,
               (counter$total_corrected + 4))
})

test_that("SectionToIndex works correctly", {

  section.table <- dbFetch(dbSendQuery(con, "SELECT * FROM section;"))
  section.hash <- hash(keys = section.table$code, 
                       values = section.table$section_id)
  
  test.sample <- section.table
  colnames(test.sample) <- c("ID", "SECTION", "Crime")
  
  result <- rep(NA, nrow(test.sample))
  for (index in 1:nrow(test.sample)) {
   result[index] <- SectionToIndex(test.sample[index, ], section.hash)
  }
  expect_equal(result, test.sample$ID)
})