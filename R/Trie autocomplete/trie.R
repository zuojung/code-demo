######## Import Text ##############
library(liqueueR)
library(assertthat)
library(methods)

# Input: .txt file of standard dictionary
# Reads in the word bank and convert it into a data.frame
# that contains two columns, weight and terms
# Output: dataframe
ReadDict <- function(in.file) {
  preparse.terms <- read.table(in.file, skip = 1, header = FALSE,
                               stringsAsFactors = FALSE,
                               strip.white = TRUE,
                               quote = "",
                               comment.char = "",
                               sep = "\t")
  colnames(preparse.terms) <- c("weight", "term")
  terms.count <- read.table(in.file, nrows = 1, header = FALSE)
  assert_that(terms.count == nrow(preparse.terms))
  return(preparse.terms)
}

# Input: character string (such as a word)
# Parse the word into characters, space and symbols included
# Output: a vector of individual characters
ParseTerms <- function(term) {
  return(unlist(strsplit(term, "")))
}
######## Trie Node ##########
# Trie: A reference class to represent a trie node
# Children: contains subsequent Trie nodes, with the pointers being
# named as the characters in the word
# max.child.weight: reflects the maximum weight of all child leafs
# (including this node)
# end.of.word.status: a Boolean variable that indicates whether this current
# node is the end of a word (thus to be considered as a end leaf as well)
# end.of.word.weight: numeric value that is reflect the leaf word's weight
Trie <- setRefClass("Trie", fields = c("children", "max.child.weight",
                                       "end.of.word.status",
                                       "end.of.word.weight"))

######## Insert Trie ##########
Trie$methods(
  initialize = function(children = list(), max.child.weight = NULL,
                        end.of.word.status = FALSE, end.of.word.weight = NULL) {
    "Default Values for an Trie node"
    .self$children <- children
    .self$max.child.weight <- max.child.weight
    .self$end.of.word.status <- end.of.word.status
    .self$end.of.word.weight <- end.of.word.weight
  },
  IterInsertTrie = function(parsed.word, word.weight) {
    "Insert a Parsed word into this Trie Iteratively"
    #
    # Input: parsed.word: a vector of characters from a word
    #       word.weight: frequency weight of the word
    # Iterate through the tree, add node if the path does not
    # exist, and update the max.child.weight when traversing
    # When finish adding all characters, update the node to
    # indicate that it is a leaf node
    current.node <- .self
    for (char in 1:length(parsed.word)) {
      current.char <- parsed.word[char]
      if (!(current.char %in% names(current.node$children))) {
        (current.node$children[[current.char]] <- 
           Trie$new(max.child.weight = word.weight))
      }
      current.node$max.child.weight <- max(current.node$max.child.weight,
                                           word.weight)
      current.node <- current.node$children[[current.char]]
    }
    if (current.node$end.of.word.status == FALSE) {
      current.node$end.of.word.status <- TRUE
      current.node$end.of.word.weight <- word.weight
    }
  },
  ConstructTrie = function(terms) {
    "Insert multiple words (ReadDict output) into this Trie"
    #
    # Input: Terms - data frame containing weight and terms
    # For every word, parse the term and iteratively insert the 
    # word into this trie
    for (index in 1:nrow(terms)) {
      terms.weight <- terms[index, "weight"]
      parsed.terms <- ParseTerms(terms[index, "term"])
      IterInsertTrie(parsed.terms, terms.weight)
    }
  },
  PrefixCheck = function(prefix){
    "Check if a current Prefix exist in Trie"
    #
    # Input: Prefix word
    # Output: status: Whether this prefix is in this Trie
    #         path: subtree of this Trie after traversing through
    #               all characters in prefix
    #         word.string: the prefix word in string format
    # 
    current.node <- .self
    parsed.prefix <- ParseTerms(prefix)
    for (char in 1:length(parsed.prefix)) {
      current.node <- current.node$children[[parsed.prefix[char]]]
    }
    return(list(status = !is.null(current.node), 
                path = current.node, 
                word.string = prefix))
  },
  UpdatePriorityQ = function(word.string, queue = PriorityQueue()) {
    "Update Priority Queue for searching based on current node"
    #
    # Input: word.string - word string of the current node in Trie
    #        queue - Priority Queue to update from the current node
    # For every child from our current node
    #     we push the child node into the queue, with max.child.weight as
    #     priority with node's attributes:
    #         1. whether we should treat this word as a word or node
    #            T - as a word, F - as a node
    #         2. the current string of the word, used when returning
    # Then we check if the current node is an end.of.word node in trie
    #     If so, we add the current node into the queue similarly
    #            to how we add the child, but set word.boolean to TRUE
    # Output: Updated Priority Queue on current node
    for (node in names(.self$children)) {
      temp.node <- .self$children[[node]]
      temp.node.weight <- temp.node$max.child.weight
      attr(temp.node, "word.boolean") <- (length(temp.node$children) == 0)
      attr(temp.node, "word.string") <- paste0(word.string, node)
      queue$push(temp.node, temp.node.weight)
    }
    if (.self$end.of.word.status == TRUE) {
      temp.node <- .self
      temp.node.weight <- temp.node$end.of.word.weight
      attr(temp.node, "word.boolean") <- TRUE
      attr(temp.node, "word.string") <- word.string
      queue$push(temp.node, temp.node.weight)
    }
    return(queue)
  }, 
  Autocomplete = function(prefix, n) {
    "Find n words in Trie that shares the prefix"
    # Input: Prefix of the word, n results to search
    # If the prefix exists, we continue to search
    #     First we build up initial Priority Queue based on the current node
    #     Then we use the FindWords helper to search for the n words
    #     Starting with the prefix in this Trie
    # Output: n words starting with the prefix in this Trie, ranked by weight
    assert_that(length(prefix) == 1)
    assert_that(is.character(prefix))
    assert_that(is.numeric(n))
    assert_that((n %% 1) == 0)
    assert_that(n > 0)
    
    check.result <- .self$PrefixCheck(prefix)
    if (check.result$status == FALSE) {
      result <- NULL
    } else {
      init.queue <- check.result$path$UpdatePriorityQ(check.result$word.string)
      init.result <- data.frame(matrix(ncol = 2, nrow = n,
                                       dimnames = list(c(), c("weight", "term"))))
      result <- FindWords(n, 0, init.result, init.queue)
    }
    return(result)
  }
)

# Input: n - number of words to look for
#        count - number of words that has been picked out
#        result - data frame of list of words with word.string and weight
#        queue - Pirority Queue for search algorithm
# We search only if we have not met the count or there are more nodes
# in the queue to search
# We pop the top node with the most weight
#   If this top word is a leaf, we add it to result and update our count
#   Else, we include all child nodes from this node into the priority queue
#         by calling UpdatePriorityQ
#   Then we recursively call the function with the updated queue
# Output: n words starting with the prefix in this Trie, ranked by weight,
#         with null values omitted
FindWords <- function(n, count, result, queue) {
  if (count < n && queue$size() > 0) {
    current.node <- queue$pop()
    current.word.string <- attr(current.node, "word.string")
    if (attr(current.node, "word.boolean") == TRUE) {
      count <- count + 1
      result[count, "weight"] <- current.node$end.of.word.weight
      result[count, "term"] <- current.word.string
    } else {
      queue <- current.node$UpdatePriorityQ(current.word.string, queue)
    }
    return(FindWords(n, count, result, queue))
  } else {
    return(na.omit(result))
  }
}

# Input: in.file - .txt file of word bank
# Output: Trie data structure of all the words
#         from the word bank
ReadTerms <- function(in.file) {
  init.trie <- Trie$new()
  init.trie$ConstructTrie(ReadDict(in.file))
  return(init.trie)
}