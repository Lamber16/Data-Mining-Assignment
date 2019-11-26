# Import relevant packages
library(dplyr)
library(tidyverse)
library(arulesSequences)
​
minsup <- 0.5
filename <- "input.csv"
​
​
transactions = read.csv(filename)
write.table(transactions, "mytxtout.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
trans_matrix <- read_baskets("mytxtout.txt", sep = " ", info = c("sequenceID","eventID","SIZE"))
​
s1 <- cspade(trans_matrix, parameter = list(support = minsup), control = list(verbose = TRUE))
s1.df <- as(s1, "data.frame")
summary(s1)
​
r1 <- as(ruleInduction(s1, confidence = 0.5, control = list(verbose = TRUE)), "data.frame")
​
# Separate LHS and RHS rules
r1$rulecount <- as.character(r1$rule)
max_col <- max(sapply(strsplit(r1$rulecount,' => '),length))
r_sep <- separate(data = r1, col = rule, into = paste0("Time",1:max_col), sep = " => ")
r_sep$Time2 <- substring(r_sep$Time2,3,nchar(r_sep$Time2)-2)
​
# Strip LHS baskets
max_time1 <- max(sapply(strsplit(r_sep$Time1,'},'),length))
r_sep$TimeClean <- substring(r_sep$Time1,3,nchar(r_sep$Time1)-2)
r_sep$TimeClean <- gsub("\\},\\{", "zzz", r_sep$TimeClean)
r_sep_items <- separate(data = r_sep, col = TimeClean, into = paste0("Previous_Items",1:max_time1), sep = "zzz")
​
# Get cleaned temporal rules: time reads sequentially from left to right
​
r_shift_na <- r_sep_items
​
for (i in seq(1, nrow(r_shift_na))){
  for (col in seq(8, (6+max_time1))){
    if (is.na(r_shift_na[i,col])==TRUE){
      r_shift_na[i,col] <- r_shift_na[i,col-1]
      r_shift_na[i,col-1] <- NA  
    }
  }
}
names(r_shift_na)[2] <- "Predicted_Items"
​
cols <- c(7:(6+max_time1), 2:5)
temporal_rules <- r_shift_na[,cols]
temporal_rules <- temporal_rules[order(-temporal_rules$lift, -temporal_rules$confidence, 
                                       -temporal_rules$support, temporal_rules$Predicted_Items),]
​
write.csv(as.data.frame(temporal_rules), file = "TemporalRules.csv", row.names = FALSE, na="")
​
# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only <- temporal_rules[,1:(ncol(temporal_rules)-3)]
basket_mat <- as.vector(as.matrix(baskets_only))
freq_itemsets_in_rules <- unique(basket_mat[!is.na(basket_mat)])
write.csv(as.data.frame(freq_itemsets_in_rules), file = "FreqItemsetsInRules.csv", row.names = FALSE)
