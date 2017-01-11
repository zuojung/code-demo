########## Speed Test ##########
library(microbenchmark)
source("trie.R")

Rprof(babynames.trie <- ReadTerms("baby-names.txt"))
baby.names <- ReadDict("baby-names.txt")

Rprof(filename = "Rprof.out", babynames.trie$AutocompleteMe("K", 22))
summaryRprof()

# Unfortunately, regex is faster....
# However, mean is 158.08210 nanoseconds
microbenchmark(babynames.trie$AutocompleteMe("K", 22), 
               baby.names[grep("K", ReadDict("baby-names.txt")[, "term"])[1:22],])

#  mean is 83.84824 nanoseconds
microbenchmark(babynames.trie$AutocompleteMe("Ke", 20), 
               baby.names[grep("Ke", ReadDict("baby-names.txt")[, "term"])[1:20],])

# Big O
test.mb.1 <- microbenchmark(babynames.trie$AutocompleteMe("Ke", 20), 
                            babynames.trie$AutocompleteMe("Ke", 19),
                            babynames.trie$AutocompleteMe("Ke", 18),
                            babynames.trie$AutocompleteMe("Ke", 17),
                            babynames.trie$AutocompleteMe("Ke", 16),
                            babynames.trie$AutocompleteMe("Ke", 15),
                            times = 200)


babynames.trie.sub <- Trie$new()
babynames.trie.sub$ConstructTrie(baby.names[1:10000,])

babynames.trie.sub.sub <- Trie$new()
babynames.trie.sub.sub$ConstructTrie(baby.names[1:3333,])

test.mb.1.sub <- microbenchmark(babynames.trie.sub$AutocompleteMe("Ke", 20), 
                                babynames.trie.sub$AutocompleteMe("Ke", 19),
                                babynames.trie.sub$AutocompleteMe("Ke", 18),
                                babynames.trie.sub$AutocompleteMe("Ke", 17),
                                babynames.trie.sub$AutocompleteMe("Ke", 16),
                                babynames.trie.sub$AutocompleteMe("Ke", 15),
                                times = 200)



#
test.mb.2 <- microbenchmark(babynames.trie$AutocompleteMe("A", 1), 
                            babynames.trie$AutocompleteMe("B", 1),
                            babynames.trie$AutocompleteMe("C", 1),
                            babynames.trie$AutocompleteMe("D", 1),
                            babynames.trie$AutocompleteMe("E", 1),
                            babynames.trie$AutocompleteMe("F", 1),
                            times = 200)


test.mb.2.sub <- microbenchmark(babynames.trie.sub$AutocompleteMe("A", 1), 
                                babynames.trie.sub$AutocompleteMe("B", 1),
                                babynames.trie.sub$AutocompleteMe("C", 1),
                                babynames.trie.sub$AutocompleteMe("D", 1),
                                babynames.trie.sub$AutocompleteMe("E", 1),
                                babynames.trie.sub$AutocompleteMe("F", 1),
                                times = 200)

par(mfrow = c(1,2))
boxplot(test.mb.2, xaxt="n", xlab="", main = "All Baby Names", ylim = c(5,40))
axis(1, at=c(1:6),labels=LETTERS[1:6], col.axis=1, las=2)
boxplot(test.mb.2.sub, xaxt="n", xlab="", main = "Subset Baby Names", ylim = c(5,40))
axis(1, at=c(1:6),labels=LETTERS[1:6], col.axis=1, las=2)

# Single Prefix Search
test.mb.3 <- microbenchmark(babynames.trie$PrefixCheck("Emma"), 
                            babynames.trie$PrefixCheck("Isabella"),
                            babynames.trie$PrefixCheck("Mason"),
                            babynames.trie$PrefixCheck("Jacob"),
                            babynames.trie$PrefixCheck("Ethan"),
                            babynames.trie$PrefixCheck("Sophia"),
                            babynames.trie$PrefixCheck("Noah"),
                            babynames.trie$PrefixCheck("Olivia"),
                            times = 1000)


test.mb.3.sub <- microbenchmark(babynames.trie.sub$PrefixCheck("Emma"), 
                                babynames.trie.sub$PrefixCheck("Isabella"),
                                babynames.trie.sub$PrefixCheck("Mason"),
                                babynames.trie.sub$PrefixCheck("Jacob"),
                                babynames.trie.sub$PrefixCheck("Ethan"),
                                babynames.trie.sub$PrefixCheck("Sophia"),
                                babynames.trie.sub$PrefixCheck("Noah"),
                                babynames.trie.sub$PrefixCheck("Olivia"),
                                times = 1000)


par(mfrow = c(1,2))
boxplot(test.mb.3, xaxt="n", xlab="", main = "All Baby Names")
axis(1, at=c(1:8),labels=c("Emma", "Isabella", "Mason", "Jacob", "Ethan", "Sophia", "Noah", "Olivia"), col.axis=1, las=2)
boxplot(test.mb.3.sub, xaxt="n", xlab="",  main = "Subset of Baby Names")
axis(1, at=c(1:8),labels=c("Emma", "Isabella", "Mason", "Jacob", "Ethan", "Sophia", "Noah", "Olivia"), col.axis=1, las=2)


# 
test.mb.4 <- microbenchmark(babynames.trie$AutocompleteMe("Emma", 1), 
                            babynames.trie$AutocompleteMe("Isabella", 1),
                            babynames.trie$AutocompleteMe("Mason", 1),
                            babynames.trie$AutocompleteMe("Jacob", 1),
                            babynames.trie$AutocompleteMe("Ethan", 1),
                            babynames.trie$AutocompleteMe("Sophia", 1),
                            babynames.trie$AutocompleteMe("Noah", 1),
                            babynames.trie$AutocompleteMe("Olivia", 1),
                            times = 200)


test.mb.4.sub <- microbenchmark(babynames.trie.sub$AutocompleteMe("Emma", 1), 
                                babynames.trie.sub$AutocompleteMe("Isabella", 1),
                                babynames.trie.sub$AutocompleteMe("Mason", 1),
                                babynames.trie.sub$AutocompleteMe("Jacob", 1),
                                babynames.trie.sub$AutocompleteMe("Ethan", 1),
                                babynames.trie.sub$AutocompleteMe("Sophia", 1),
                                babynames.trie.sub$AutocompleteMe("Noah", 1),
                                babynames.trie.sub$AutocompleteMe("Olivia", 1),
                                times = 200)

test.mb.4.sub.sub <- microbenchmark(babynames.trie.sub.sub$AutocompleteMe("Emma", 1), 
                                    babynames.trie.sub.sub$AutocompleteMe("Isabella", 1),
                                    babynames.trie.sub.sub$AutocompleteMe("Mason", 1),
                                    babynames.trie.sub.sub$AutocompleteMe("Jacob", 1),
                                    babynames.trie.sub.sub$AutocompleteMe("Ethan", 1),
                                    babynames.trie.sub.sub$AutocompleteMe("Sophia", 1),
                                    babynames.trie.sub.sub$AutocompleteMe("Noah", 1),
                                    babynames.trie.sub.sub$AutocompleteMe("Olivia", 1),
                                    times = 200)

par(mfrow = c(1,2))
boxplot(test.mb.4, xaxt="n", xlab="", main = "All Baby Names")
axis(1, at=c(1:8),labels=c("Emma", "Isabella", "Mason", "Jacob", "Ethan", "Sophia", "Noah", "Olivia"), col.axis=1, las=2)
boxplot(test.mb.4.sub, xaxt="n", xlab="",  main = "Subset of Baby Names")
axis(1, at=c(1:8),labels=c("Emma", "Isabella", "Mason", "Jacob", "Ethan", "Sophia", "Noah", "Olivia"), col.axis=1, las=2)