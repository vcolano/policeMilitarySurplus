library(data.table)
library(ggplot2)
library(plyr)

# import data, make column names safe
data = fread("~/projects/policeMilitarySurplus/data/policeAquistionsOfMiliarySurplus2014")
colnames(data) = make.names(names(data))
attach(data)

# which items are purchased the most?
nsnFreq = count(data, "NSN")
nsnFreq$Item.Name = data$Item.Name[match(nsnFreq$NSN, data$NSN)]
nsnFreq = nsnFreq[with(nsnFreq, order(-freq)), ]
head(nsnFreq, n=10)

# Let's see if sorting on names cleans things up
itemFreq = count(data, "Item.Name")
itemFreq = itemFreq[with(itemFreq, order(-freq)), ]
head(itemFreq, n=10)
# It does and everything adds up so we'll use Item.Name

# Now let's see what the total expenditure on an item-level basis is
costs_df = itemFreq
costs_df$Total.Cost = data$Acquisition.Cost[match(costs_df$Item.Name, data$Item.Name)]
costs_df$Total.Cost = costs_df$Total.Cost*costs_df$freq
costs_df = costs_df[with(costs_df, order(-Total.Cost)), ]
costs_df = costs_df[!(costs_df$Item.Name == ""), ] # get rid of empty string value
head(costs_df, n=20)

