# Trie Tests
library(testthat)
library(liqueueR)
library(methods)
source("trie.R")

# Trie Method Tests
test_that("Trie Node Initialization returns default
          and stores values Properly", {
            expect_equal(Trie$new()$children,
                         list())
            expect_equal(Trie$new()$max.child.weight,
                         NULL)
            expect_equal(Trie$new()$end.of.word.status,
                         FALSE)
            expect_equal(Trie$new()$end.of.word.weight,
                         NULL)
            expect_equal(Trie$new(max.child.weight = 12)$max.child.weight,
                         12)
            expect_equal(Trie$new(end.of.word.status = TRUE)$end.of.word.status, 
                         TRUE)
            expect_equal(Trie$new(end.of.word.weight = 12)$end.of.word.weight,
                         12)
            expect_equal(Trie$new(children = Trie$new())$children,
                         Trie$new())
            })

test_that("Trie$IterInsertTrie Inserts Leafs with right structure", {
  case.1 <- Trie$new()
  case.1$IterInsertTrie(c("d", "o", "g"), 12)
  case.1$IterInsertTrie(c("a", "k"), 13)
  case.1$IterInsertTrie(c("a", "b", "c"), 15)
  case.1$IterInsertTrie(c("e"), 15)
  
  expect_equal(case.1$children$d$children$o$children$g$children,
               list())
  expect_equal(case.1$children$a$children$k$children,
               list())
  expect_equal(case.1$children$a$children$b$children$c$children,
               list())
  expect_equal(case.1$children$e$children,
               list())
  
})

test_that("Trie$IterInsertTrie Inserts Correctly", {
  # Case 1: Insert a Single Word
  case.1 <- Trie$new()
  case.1$IterInsertTrie(c("d", "o", "g"), 12)
  
  # Case 2: Insert Two Words with different Weights
  case.2 <- Trie$new()
  case.2$IterInsertTrie(c("d", "o", "g"), 12)
  case.2$IterInsertTrie(c("a"), 100)
  
  # Case 3: Insert Mutiple Words under the same prefix
  case.3 <- Trie$new()
  case.3$IterInsertTrie(c("d", "o", "g"), 12)
  case.3$IterInsertTrie(c("d", "o"), 4)
  case.3$IterInsertTrie(c("d", "o", "g", "e"), 2)
  case.3$IterInsertTrie(c("d", "o", "k"), 2)
  
  # Case 4: Insert Strings
  case.4 <- Trie$new()
  case.4$IterInsertTrie(c("d", " "), 12)
  case.4$IterInsertTrie(c("d", " ", "("), 4)
  case.4$IterInsertTrie(c("d", "'"), 2)
  case.4$IterInsertTrie(c("d", "'", "\""), 2)
  
  tests <- list(
    test.cases.1 <- list(
      list(node = case.1,
           result = list(c.names = "d",
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.1$children$d,
           result = list(c.names = "o",
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.1$children$d$children$o,
           result = list(c.names = "g",
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.1$children$d$children$o$children$g,
           result = list(c.names = NULL,
                         m.c.w = 12,
                         e.o.w.s = TRUE,
                         e.o.w.w = 12))
    ),
    test.cases.2 <- list(
      list(node = case.2,
           result = list(c.names = c("d", "a"),
                         m.c.w = 100,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.2$children$d,
           result = list(c.names = "o",
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.2$children$d$children$o,
           result = list(c.names = "g",
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.2$children$d$children$o$children$g,
           result = list(c.names = NULL,
                         m.c.w = 12,
                         e.o.w.s = TRUE,
                         e.o.w.w = 12)),
      list(node = case.2$children$a,
           result = list(c.names = NULL,
                         m.c.w = 100,
                         e.o.w.s = TRUE,
                         e.o.w.w = 100))
    ),
    test.cases.3 <- list(
      list(node = case.3,
           result = list(c.names = c("d"),
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.3$children$d,
           result = list(c.names = "o",
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.3$children$d$children$o,
           result = list(c.names = c("g", "k"),
                         m.c.w = 12,
                         e.o.w.s = TRUE,
                         e.o.w.w = 4)),
      list(node = case.3$children$d$children$o$children$g,
           result = list(c.names = "e",
                         m.c.w = 12,
                         e.o.w.s = TRUE,
                         e.o.w.w = 12)),
      list(node = case.3$children$d$children$o$children$g$children$e,
           result = list(c.names = NULL,
                         m.c.w = 2,
                         e.o.w.s = TRUE,
                         e.o.w.w = 2)),
      list(node = case.3$children$d$children$o$children$k,
           result = list(c.names = NULL,
                         m.c.w = 2,
                         e.o.w.s = TRUE,
                         e.o.w.w = 2))
    ),
    test.cases.4 <- list(
      list(node = case.4,
           result = list(c.names = c("d"),
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.4$children$d,
           result = list(c.names = c(" ", "'"),
                         m.c.w = 12,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.4$children$d$children[[" "]],
           result = list(c.names = "(",
                         m.c.w = 12,
                         e.o.w.s = TRUE,
                         e.o.w.w = 12)),
      list(node = case.4$children$d$children[[" "]]$children[["("]],
           result = list(c.names = NULL,
                         m.c.w = 4,
                         e.o.w.s = TRUE,
                         e.o.w.w = 4)),
      list(node = case.4$children$d$children[["'"]],
           result = list(c.names = '"',
                         m.c.w = 2,
                         e.o.w.s = TRUE,
                         e.o.w.w = 2)),
      list(node = case.4$children$d$children[["'"]]$children[["\""]],
           result = list(c.names = NULL,
                         m.c.w = 2,
                         e.o.w.s = TRUE,
                         e.o.w.w = 2))
    )
  )
  for (test.cases in tests) {
    for (case in test.cases) {
      info <-  paste(case, collapse = " -> ")
      expect_that(list(c.names = names(case$node$children),
                       m.c.w = case$node$max.child.weight,
                       e.o.w.s = case$node$end.of.word.status,
                       e.o.w.w = case$node$end.of.word.weight), 
                  equals(case$result),  info = info)
    }
  }
})

test_that("Trie$PrefixCheck Searches for Prefix Correctly", {
  case.1 <- Trie$new()
  case.1$IterInsertTrie(c("d", "o", "g"), 12)
  case.1$IterInsertTrie(c("d", "o"), 4)
  case.1$IterInsertTrie(c("d", "o", "g", "e"), 2)
  case.1$IterInsertTrie(c("d", "o", "k"), 2)
  
  case.2 <- Trie$new()
  case.2$IterInsertTrie(c("d", " "), 12)
  case.2$IterInsertTrie(c("d", " ", "("), 4)
  case.2$IterInsertTrie(c("d", "'"), 2)
  case.2$IterInsertTrie(c("d", "'", "\""), 2)
  
  tests <- list(
    test.cases.1 <- list(
      list(node = case.1$PrefixCheck("K"),
           result = list(path = NULL,
                         status = FALSE,
                         word.string = "K")),
      list(node = case.1$PrefixCheck("d"),
           result = list(path = case.1$children$d,
                         status = TRUE,
                         word.string = "d")),
      list(node = case.1$PrefixCheck("do"),
           result = list(path = case.1$children$d$children$o,
                         status = TRUE,
                         word.string = "do")),
      list(node = case.1$PrefixCheck("dog"),
           result = list(path = case.1$children$d$children$o$children$g,
                         status = TRUE,
                         word.string = "dog")),
      list(node = case.1$PrefixCheck("doge"),
           result = list(
             path = case.1$children$d$children$o$children$g$children$e,
             status = TRUE,
             word.string = "doge")),
      list(node = case.1$PrefixCheck("dok"),
           result = list(path = case.1$children$d$children$o$children$k,
                         status = TRUE,
                         word.string = "dok")),
      list(node = case.1$PrefixCheck("dokd"),
           result = list(path = NULL,
                         status = FALSE,
                         word.string = "dokd"))
    ),
    test.cases.2 <- list(
      list(node = case.2$PrefixCheck(" "),
           result = list(path = NULL,
                         status = FALSE,
                         word.string = " ")),
      list(node = case.2$PrefixCheck("d"),
           result = list(path = case.2$children$d,
                         status = TRUE,
                         word.string = "d")),
      list(node = case.2$PrefixCheck("d "),
           result = list(path = case.2$children$d$children[[" "]],
                         status = TRUE,
                         word.string = "d ")),
      list(node = case.2$PrefixCheck("d ("),
           result = list(
             path = case.2$children$d$children[[" "]]$children[["("]],
             status = TRUE,
             word.string = "d (")),
      list(node = case.2$PrefixCheck("d'"),
           result = list(path = case.2$children$d$children[["'"]],
                         status = TRUE,
                         word.string = "d'")),
      list(node = case.2$PrefixCheck("d'\""),
           result = list(
             path = case.2$children$d$children[["'"]]$children[["\""]],
             status = TRUE,
             word.string = "d'\""))
    )
  )
  for (test.cases in tests) {
    for (case in test.cases) {
      info <-  paste(case, collapse = " -> ")
      expect_that(list(path = case$node$path,
                       status = case$node$status,
                       word.string = case$node$word.string), 
                  equals(case$result),  info = info)
    }
  }
})

test_that("Trie$ConstructTrie Builds Trie Correctly - Sample Nodes", {
  case.terms.1 <- ReadDict("baby-names.txt")[1:2,]
  case.terms.2 <- ReadDict("movies.txt")[100001:100002,]
  
  case.1 <- Trie$new()
  case.1$ConstructTrie(case.terms.1)
  
  case.2 <- Trie$new()
  case.2$ConstructTrie(case.terms.2)
  
  tests <- list(
    test.cases.1 <- list(
      list(node = case.1,
           result = list(c.names = c("S", "E"),
                         m.c.w = 22175,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.1$children$S,
           result = list(c.names = "o",
                         m.c.w = 22175,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.1$children$E$children$m$children$m$children$a,
           result = list(c.names = NULL,
                         m.c.w = 20811,
                         e.o.w.s = TRUE,
                         e.o.w.w = 20811)),
      list(node = case.1$children$E$children$m$children$m,
           result = list(c.names = "a",
                         m.c.w = 20811,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL))
    ),
    test.cases.2 <- list(
      list(node = case.2,
           result = list(c.names = c("\"", "M"),
                         m.c.w = 0,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.2$children[["\""]],
           result = list(c.names = "M",
                         m.c.w = 0,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.2$children[["\""]]$children$M,
           result = list(c.names = "u",
                         m.c.w = 0,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL)),
      list(node = case.2$children$M$children$a$children$r$children$r,
           result = list(c.names = "i",
                         m.c.w = 0,
                         e.o.w.s = FALSE,
                         e.o.w.w = NULL))
    )
  )
  for (test.cases in tests) {
    for (case in test.cases) {
      info <-  paste(case, collapse = " -> ")
      expect_that(list(c.names = names(case$node$children),
                       m.c.w = case$node$max.child.weight,
                       e.o.w.s = case$node$end.of.word.status,
                       e.o.w.w = case$node$end.of.word.weight), 
                  equals(case$result),  info = info)
    }
  }
})

test_that("Trie$ConstructTrie Builds Trie Correctly - Prefix Search", {
  tests <- list(
    list(case.terms = ReadDict("baby-names.txt")[1:100,]),
    list(case.terms = ReadDict("movies.txt")[100001:100101,])
  )
  
  for (test.case in tests) {
    test.trie <- Trie$new()
    test.trie$ConstructTrie(test.case$case.terms)
    for (index in 1:nrow(test.case$case.terms)) {
      word.search <- test.case$case.terms[index, "term"]
      word.weight <- test.case$case.terms[index, "weight"]
      
      info <-  paste(test.case, collapse = " -> ")
      expect_that(c(test.trie$PrefixCheck(word.search)$status,
                  test.trie$PrefixCheck(word.search)$path$end.of.word.status,
                  test.trie$PrefixCheck(word.search)$path$end.of.word.weight),
                  equals(c(TRUE, TRUE, word.weight)),  info = info)
    }
  }
})

test_that("ReadTerms Builds Trie Correctly - Prefix Search", {
  tests <- list(
    list(case.file = "testdata/testbaby-names.txt",
         case.terms = ReadDict("testdata/testbaby-names.txt")),
    list(case.file = "testdata/testmovies.txt",
         case.terms = ReadDict("testdata/testmovies.txt")),
    list(case.file = "testdata/testpokemon.txt",
         case.terms = ReadDict("testdata/testpokemon.txt")),
    list(case.file = "testdata/testwiktionary.txt",
         case.terms = ReadDict("testdata/testwiktionary.txt"))
  )
  
  for (test.case in tests) {
    test.trie <- ReadTerms(test.case$case.file)
    for (index in 1:nrow(test.case$case.terms)) {
      word.search <- test.case$case.terms[index, "term"]
      word.weight <- test.case$case.terms[index, "weight"]
      
      info <-  paste(test.case, collapse = " -> ")
      expect_that(c(test.trie$PrefixCheck(word.search)$status,
                  test.trie$PrefixCheck(word.search)$path$end.of.word.status,
                  test.trie$PrefixCheck(word.search)$path$end.of.word.weight),
                  equals(c(TRUE, TRUE, word.weight)),  info = info)
    }
  }
})

test_that("Trie$UpdatePriorityQ updates priority Queue Correctly", {
  case <- Trie$new()
  case$IterInsertTrie(c("d", "o", "g"), 12)
  case$IterInsertTrie(c("d", "o"), 4)
  case$IterInsertTrie(c("d", "o", "g", "e"), 2)
  case$IterInsertTrie(c("d", "o", "k"), 2)
  
  # sample tests - Building new Queue from scratch
  result.1 <- case$PrefixCheck("do")
  test.queue <- result.1$path$UpdatePriorityQ(result.1$word.string)
  
  test.peek.1 = test.queue$peek(pos = 1)
  expect_equal(test.peek.1, case$children$d$children$o$children$g)
  expect_equal(attr(test.peek.1, "word.boolean"), FALSE)
  expect_equal(attr(test.peek.1, "word.string"), "dog")
  expect_equal(test.queue$priorities[1], 
               case$children$d$children$o$children$g$max.child.weight)
  
  test.peek.2 = test.queue$peek(pos = 2)
  expect_equal(test.peek.2, case$children$d$children$o)
  expect_equal(attr(test.peek.2, "word.boolean"), TRUE)
  expect_equal(attr(test.peek.2, "word.string"), "do")
  expect_equal(test.queue$priorities[2], 
               case$children$d$children$o$end.of.word.weight)
  
  test.peek.3 = test.queue$peek(pos = 3)
  expect_equal(test.peek.3, case$children$d$children$o$children$k)
  expect_equal(attr(test.peek.3, "word.boolean"), TRUE)
  expect_equal(attr(test.peek.3, "word.string"), "dok")
  expect_equal(test.queue$priorities[3], 
               case$children$d$children$o$children$k$end.of.word.weight)
  
  # Operation Example, pop top node and access the node, update the current
  # queue based on the next node
  next.node <- test.queue$pop()
  result.2 <- case$PrefixCheck(attr(next.node, "word.string"))
  test.queue <- result.2$path$UpdatePriorityQ(result.2$word.string, test.queue)
  
  test.peek.1 = test.queue$peek(pos = 1)
  expect_equal(test.peek.1, case$children$d$children$o$children$g)
  expect_equal(attr(test.peek.1, "word.boolean"), TRUE)
  expect_equal(attr(test.peek.1, "word.string"), "dog")
  expect_equal(test.queue$priorities[1], 
               case$children$d$children$o$children$g$max.child.weight)
  
  test.peek.2 = test.queue$peek(pos = 2)
  expect_equal(test.peek.2, case$children$d$children$o)
  expect_equal(attr(test.peek.2, "word.boolean"), TRUE)
  expect_equal(attr(test.peek.2, "word.string"), "do")
  expect_equal(test.queue$priorities[2], 
               case$children$d$children$o$end.of.word.weight)
  
  test.peek.3 = test.queue$peek(pos = 3)
  expect_equal(test.peek.3, case$children$d$children$o$children$k)
  expect_equal(attr(test.peek.3, "word.boolean"), TRUE)
  expect_equal(attr(test.peek.3, "word.string"), "dok")
  expect_equal(test.queue$priorities[3], 
               case$children$d$children$o$children$k$end.of.word.weight)
  
  test.peek.4 = test.queue$peek(pos = 4)
  expect_equal(test.peek.4, case$children$d$children$o$children$g$children$e)
  expect_equal(attr(test.peek.4, "word.boolean"), TRUE)
  expect_equal(attr(test.peek.4, "word.string"), "doge")
  expect_equal(test.queue$priorities[4], 
               case$children$d$children$o$children$k$end.of.word.weight)
})

test_that("FindWords retrives results Correctly", {
  # Since Autocomplete is calls this function, we are going
  # to do proof of concept tests here, and leave the randomized
  # and more thorough in the autocomplete tests to avoid redundancy
  test.node <- ReadTerms("testdata/testbaby-names.txt")
  test.terms <- ReadDict("testdata/testbaby-names.txt")
  
  tmp.result <- data.frame(matrix(ncol = 2, nrow = 12,
                                  dimnames = list(c(1:12), 
                                                  c("weight", "term"))))
  test.update = test.node$PrefixCheck("E")
  test.queue = test.update$path$UpdatePriorityQ(test.update$word.string)
  
  tmp.result <- FindWords(n = 12, count =  0, 
                          result = tmp.result, 
                          queue = test.queue)
  
  # We separate the columns because expect_equal also checks the rownames,
  # and regex results contains the actual row names from the column
  # Thus even if the values match, the row names doesn't
  expect_equal(tmp.result[, "weight"], 
               test.terms[grep("E", test.terms[, "term"])[1:12], "weight"])
  expect_equal(tmp.result[, "term"], 
               test.terms[grep("E", test.terms[, "term"])[1:12], "term"])
})

test_that("Trie$ConstructTrie Builds Movie Trie Correctly - Random Generate", {
  movie.terms = ReadDict("movies.txt")
  tests <- list(
    list(case.terms = movie.terms[sample(1:nrow(movie.terms), 200),]),
    list(case.terms = movie.terms[sample(1:nrow(movie.terms), 200),]),
    list(case.terms = movie.terms[sample(1:nrow(movie.terms), 200),]),
    list(case.terms = movie.terms[sample(1:nrow(movie.terms), 200),]),
    list(case.terms = movie.terms[sample(1:nrow(movie.terms), 200),])
  )
  
  for (test.case in tests) {
    test.trie <- Trie$new()
    test.trie$ConstructTrie(test.case$case.terms)
    for (index in 1:nrow(test.case$case.terms)) {
      word.search <- test.case$case.terms[index, "term"]
      word.weight <- test.case$case.terms[index, "weight"]
      
      info <-  paste(test.case, collapse = " -> ")
      expect_that(c(test.trie$PrefixCheck(word.search)$status,
                    test.trie$PrefixCheck(word.search)$path$end.of.word.status,
                    test.trie$PrefixCheck(word.search)$path$end.of.word.weight),
                  equals(c(TRUE, TRUE, word.weight)),  info = info)
    }
  }
})

test_that("Trie$Autocomplete retrieves results Correctly - Randomized", {
  # we want to somewhat randomly sample from the four data sets
  # and then randomize our autocomplete searches
  #
  # For each word bank, we sample 500 entries and construct a Trie from
  # the dataset, then we repeat the following process:
  # Draw a random sample from the current word, and then draw a prefix of
  # random size from the sample
  # Then for each word in the prefix sample, we compare the result
  # of autocomplete, and the result of prefix search using regex
  sample.size <- 20
  # Select the words from the word bank
  word.bank <- list(ReadDict("pokemon.txt"),
                    ReadDict("baby-names.txt"),
                    ReadDict("wiktionary.txt"))
  
  for (df in word.bank) {
    terms.1 <- df[sample(1:nrow(df), 500),]
    trie.1 <- Trie$new()
    trie.1$ConstructTrie(terms.1)
    
    for (i in 1:20) {
      # Then we randomly sample from the terms, couple prefixes that exists
      # Then we randomly select the number of responses that we would like to have
      # Then compare autocomplete of all possible outcome with regex results
      word.sample <- sample(terms.1$term, sample.size)
      prefix.sample <- rep(NA, sample.size)
      for (index in 1:sample.size) {
        prefix.sample[index] <- substr(word.sample[index], 1, 
                                       sample(1:nchar(word.sample[index]), 1))
      }
      
      for (index in 1:sample.size) {
        result.size <- 1000
        tmp.result <- trie.1$Autocomplete(prefix.sample[index], result.size)
        test.result <- na.omit(terms.1[grep(paste0("^",prefix.sample[index]), 
                                            terms.1[, "term"])[1:result.size],])
        
        # In cases where the order of the words is different between 
        # the two results with the same weights, we first confirm that
        # autocomplete weights are sorted in decreasing order
        # Then we sort both lists by their weight and then term
        # Then we compare the two results for validity
        if (all(diff(tmp.result$weight) >= 0) & 
            length(unique(tmp.result$weight)) > 1) {
          browser()
          stop("Error: Weights in Autocomplete Unsorted")
        } else{
          tmp.result <- tmp.result[order(-tmp.result[, "weight"], tmp.result[, "term"]), ]
          test.result <- test.result[order(-test.result[, "weight"], test.result[, "term"]), ]
          expect_equal(tmp.result[, "weight"], test.result[, "weight"])
          expect_equal(tmp.result[, "term"], test.result[,"term"])
        }
      }
    }
  }
})