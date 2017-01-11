args = commandArgs(trailingOnly = TRUE)
source("trie.r")
trie <- ReadTerms(args[2])
trie$Autocomplete(args[1], as.numeric(args[3]))