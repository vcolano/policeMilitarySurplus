library(data.table)
library(ggplot2)
library(scales)
library(ggmap)
library(statebins)
library(plyr)

# import data, make column names safe
data = fread("~/projects/policeMilitarySurplus/data/policeAcquisitionOfMilitarySurplus2014.csv")
colnames(data) = make.names(names(data)) # make column names R-friendly (remove spaces)
data$Item.Name = sapply(data$Item.Name, trimws) # trim whitespace
data = data[!(data$Item.Name == ""), ] # get rid of empty string value

colnames(data)
# NSN stands for NATO Stock Number, it's used to identify military supplies in the US and across NATO countries

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

# Now let's see what the total expenditure on an item-level basis is.
# Note that we ignore quantity here because it is usually 1 and we don't need to know the total
# expenditures precisely right now.
costs_df = itemFreq
costs_df$freq = costs_df$freq + (1 - data$Quantity[match(costs_df$Item.Name, data$Item.Name)])
costs_df$Total.Cost = data$Acquisition.Cost[match(costs_df$Item.Name, data$Item.Name)]
costs_df$Total.Cost = costs_df$Total.Cost*costs_df$freq
costs_df$NSN = data$NSN[match(costs_df$Item.Name, data$Item.Name)] # NSN will be useful to us
costs_df = costs_df[c("NSN", "Item.Name", "freq", "Total.Cost")] # reorder for readability
costs_df = costs_df[with(costs_df, order(-Total.Cost)), ]
head(costs_df, n=20)

# Just based on the names we can see that there are a lot of different kinds of supplies being purchased.
# Since examining the militarization of police in the US is the focus of the analysis we'll reduce the data
# to a subset that is explicitly military in nature, items which could serve little to no purpose in the policing
# of civilians. For simplicity I only examine the 100 items with the highest total cost and add items with keywords
# that indicate they would fit in this category.
# Full disclaimer: I do not have any significant domain knowledge on this issue. I determined what to include in 
# the subset based on the name and also gathering more information on each item by looking up its NSN number.
subset = c("TRUCK,UTILITY",  "MINE RESISTANT VEHICLE", "ONLY COMPLETE COMBAT/ASSAULT/TACTICAL WHEELED VEHICLES", 
           "RIFLE,5.56 MILLIMETER", "ILLUMINATOR,INFRARED", "TRUCK,CARGO", "TRUCK,VAN", 
           "CARRIER,PERSONNEL,FULL TRACKED", "TRUCK,ARMORED", "FORWARD LOOKING INFRARED IMAGING SYSTEM", 
           "TRUCK,WRECKER", "NIGHT VISION GOGGLE", "VIEWER,NIGHT VISION", "BOAT,BRIDGE ERECTION,INBOARD ENGINE", 
           "LIGHT ARMORED VEHICLE", "RANGE FINDER,LASER", "SIGHT,REFLEX", 
           "SHOP EQUIPMENT,AUTOMOTIVE VEHICLE", "TRUCK,STAKE", "RIFLE,7.62 MILLIMETER", "MINE RESISTANT VEHI", 
           "TRUCK,TANK", "SIGHT,NIGHT VISION SNIPERSCOPE", "SEMITRAILER,TANK")
subset = c(subset, unique(data$Item.Name[with(data, grepl("RIFLE", Item.Name))]))
subset = c(subset, unique(data$Item.Name[with(data, grepl("TANK", Item.Name))]))
subset = c(subset, unique(data$Item.Name[with(data, grepl("GRENADE", Item.Name))]))
subset = c(subset, unique(data$Item.Name[with(data, grepl("BAYONET", Item.Name))]))
subset = unique(subset) # remove duplicates
length(subset) # How many different items are there in our subset?

subset_df = data[data$Item.Name %in% subset]

subset_costs_df = count(subset_df, "Item.Name")
subset_costs_df = subset_costs_df[with(subset_costs_df, order(-freq)), ]
subset_costs_df$freq = subset_costs_df$freq + (1 - subset_df$Quantity[match(subset_costs_df$Item.Name, subset_df$Item.Name)])
subset_costs_df$Total.Cost = subset_df$Acquisition.Cost[match(subset_costs_df$Item.Name, subset_df$Item.Name)]
subset_costs_df$Total.Cost = subset_costs_df$Total.Cost*subset_costs_df$freq
subset_costs_df = subset_costs_df[with(subset_costs_df, order(-Total.Cost)), ]
head(subset_costs_df, n=10)
sum(subset_costs_df$Total.Cost)

# Reshape data onto a state level
states_df = aggregate(subset_df$Acquisition.Cost, by=list(state=subset_df$State), FUN=sum)
names(states_df)[names(states_df) == 'x'] = 'spending'
states_df$stateAbr = states_df$state
states_df$state = sapply(state.name[match(states_df$state, state.abb)], tolower)
states_df = states_df[complete.cases(states_df),]

# see who spent the most
ggplot(data = head(states_df[order(-states_df$spending), ], n=50), aes(reorder(state, -spending), spending)) + 
  geom_histogram(stat="identity", fill="#4b5320") + # army green seems appropriate
  scale_x_discrete() + 
  scale_y_continuous(labels = comma, limits = c(0, 50000000)) + 
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme_classic() +
  xlab("State") + ylab("Spending in USD")

# adjust for state population
states_pop = fread("~/projects/policeMilitarySurplus/data/statePopulation.csv")
states_df$spendingPerCapita = states_df$spending / states_pop[match(states_df$state, sapply(states_pop$state, tolower))]$pop_est_2014

ggplot(data = head(states_df[order(-states_df$spendingPerCapita), ], n=50), aes(reorder(state, -spendingPerCapita), spendingPerCapita)) + 
  geom_histogram(stat="identity", fill="#4b5320") +
  scale_x_discrete() + 
  scale_y_continuous(labels = comma, limits = c(0, 10)) + 
  theme_bw(base_size = 12, base_family = "Helvetica") +
  theme_classic() +
  xlab("State") + ylab("Spending per capita by state in USD")

# let's see this on a map
statebins_continuous(states_df, "stateAbr", "spending", legend_position = "bottom",
                     legend_title="Spending in USD", font_size = 5,
                     brewer_pal = "Greens", text_color = "black", 
                     plot_title = "Police aquisitions of combat-related military surplus equipment from 2006-2014",
                     title_position = "top")

statebins_continuous(states_df, "stateAbr", "spendingPerCapita", legend_position = "bottom",
                     legend_title="Spending per capita by state in USD", font_size = 5,
                     brewer_pal = "Greens", text_color = "black", 
                     plot_title = "Police aquisitions of combat-related military surplus equipment from 2006-2014",
                     title_position = "top")
