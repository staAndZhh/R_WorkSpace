library(data.table)
library(tidyverse)
library(arules)
# list to apriori
a_list <- list(c("a","b","c"),c("a","b"), c("a","b","d"), c("c","e"), c("a","b","d","e"))
names(a_list) <- paste("Tr",c(1:5), sep = "")
trans1 <- as(a_list, "transactions")
summary(trans1)
image(trans1)

# matrix
a_matrix <- matrix(c(1,1,1,0,0, 1,1,0,0,0,1,1,0,1,0,0,0,1,0,1,1,1,0,1,1), ncol = 5)
dimnames(a_matrix) <- list(c("a","b","c","d","e"),paste("Tr",c(1:5), sep = ""))
trans2 <- as(a_matrix, "transactions")
inspect(trans2)
