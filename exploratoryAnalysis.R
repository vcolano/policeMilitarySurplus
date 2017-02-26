library(data.table)
library(ggplot2)
library(scales)
library(ggmap)
library(statebins)
library(plyr)
library(readxl)
library(stringr)
library(stringi)
library(extrafont)

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

# appropriate theme
military_theme = theme_classic() + theme(plot.title=element_text(size=20, color="#0D0A0B", vjust=1, hjust=0.5, family="Courier New"),
                         text=element_text(size=13, hjust=0.5, family="Courier New"), 
                         plot.background=element_rect(color="#F3EFF5"))

# see who spent the most
ggplot(data = head(states_df[order(-states_df$spending), ], n=10), aes(reorder(state, -spending), spending)) + 
  geom_histogram(stat="identity", fill="#4b5320") + # army green seems appropriate
  scale_x_discrete() + 
  scale_y_continuous(labels = comma, limits = c(0, 50000000),  expand=c(0,0)) + 
  military_theme + theme(axis.text.x = element_text(angle=45, hjust=1)) +
  xlab("State") + ylab("Spending in USD") + ggtitle("Top 10 Spenders") 

# adjust for state population
states_pop = fread("~/projects/policeMilitarySurplus/data/statePopulation.csv")
states_df$spendingPerCapita = states_df$spending / states_pop[match(states_df$state, sapply(states_pop$state, tolower))]$pop_est_2014

ggplot(data = head(states_df[order(-states_df$spendingPerCapita), ], n=10), aes(reorder(state, -spendingPerCapita), spendingPerCapita)) + 
  geom_histogram(stat="identity", fill="#4b5320") +
  scale_x_discrete() + 
  scale_y_continuous(labels = comma, limits = c(0, 10), expand=c(0,0)) + 
  military_theme +
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 12)) +
  xlab("State") + ylab("Spending per capita by state in USD") + 
  ggtitle("Top 10 Spenders per Capita")

# let's see this on a map
statebins(states_df, "stateAbr", "spending", legend_position = "right", breaks=5,
                     labels=c("<$10m","<$20m","<$30m","<$40m","<$50m"), font_size = 5, legend_title="Spending in USD", 
                     brewer_pal = "Greens", text_color = "black", 
                     plot_title = "Spending from 2006-2014",
                     title_position = "top")

statebins(states_df, "stateAbr", "spendingPerCapita", legend_position = "right",
                     legend_title="Spending per capita by state in USD", font_size = 5,
                     breaks = 5, labels=c("<$2","<$4","<$6","<$8","<$10"),
                     brewer_pal = "Greens", text_color = "black", 
                     plot_title = "Spending per Capita from 2006-2014",
                     title_position = "top")

# now let's take a look at a county-level analysis
# Reshape data onto a county level
counties_df = aggregate(subset_df$Acquisition.Cost, by=list(county=subset_df$County), FUN=sum)
names(counties_df)[names(counties_df) == 'x'] = 'spending'
counties_df$state = subset_df$State[match(counties_df$county, subset_df$County)]
counties_df$county = sapply(counties_df$county, tolower)
counties_df$state = sapply(state.name[match(counties_df$state, state.abb)], tolower)
counties_df = counties_df[complete.cases(counties_df),]
counties_df = counties_df[with(counties_df, order(-spending)),]
head(counties_df, n=10)

# We'll introduce a new column to make matching easier with all of these census data sets
counties_df$Area_name = with(counties_df, paste0(stri_trans_totitle(county), ", ", sapply(state.abb[match(stri_trans_totitle(state), state.name)], toupper)))

# adjust for population 
pop_df = read_excel("~/projects/policeMilitarySurplus/data/censusCountyPopulationTotal1.xls", sheet = 1)
counties_df$spending[is.na(counties_df$spending)] = 0
counties_df$population = pop_df$POP010210D[match(counties_df$Area_name, pop_df$Area_name)] # POP010210D: resident population in 2010
counties_df$spendingPerCapita = counties_df$spending / counties_df$population
counties_df = counties_df[,c('county', 'state', 'population', 'spending', 'spendingPerCapita', 'Area_name')]
counties_df = counties_df[with(counties_df, order(-spendingPerCapita)),]

# note that this analysis does not take into account counties which have spent nothing on this militarized equipment
# There are 3144 counties in the US (https://en.wikipedia.org/wiki/List_of_counties_by_U.S._state)
print(3144 - length(counties_df$county)) # number of counties with no recorded spending

# Is population correlated with spending? 
spendPopCor = format(cor(counties_df$population, counties_df$spending, use = "complete"), digits = 4)
ggplot(counties_df, aes(x = log(population), y = log(spending))) + 
  military_theme + xlab("Population (log smoothed)") + 
  ylab("Spending (log smoothed)") +
  geom_point(colour="#4b5320") + geom_smooth(method='lm', formula=y~x, colour="#454955") +
  ggtitle(paste("Correlation between spending and population = ", spendPopCor))

summary(counties_df$spendingPerCapita) # Median and Mean imply a right skew 

# plot distribution of spending per capita
ggplot(data = counties_df, aes(x=counties_df$spendingPerCapita)) +
  stat_density(aes(y=..count..), color="#454955", fill="dark green", alpha=0.3) +
  stat_density(aes(y=..count..), color="#454955", fill="dark green", alpha=0.3) +
  scale_x_continuous(breaks=c(-1,0,1,2,3,4,5,10,30,100,300,500,1000), trans="log1p", expand=c(0,0), limits=c(0,1750)) +
  scale_y_continuous(breaks=c(0,100,200,300,400,500,600,700,750), expand=c(0,0), limits=c(0,800)) +
  military_theme + xlab("Spending Per Capita in USD") + ylab("Count") +
  ggtitle("Spending Per Capita Density on a County Level")
  
# plot the counties which spent the most per capita
ggplot(data = head(counties_df[order(-counties_df$spendingPerCapita), ], n=15), aes(reorder(county, -spendingPerCapita), spendingPerCapita)) + 
  geom_histogram(stat="identity", fill="#4b5320") +
  scale_x_discrete(labels=paste(counties_df$county, counties_df$state, sep=", ")) + 
  scale_y_continuous(labels = comma, limits = c(0, 1250)) + 
  military_theme + theme(axis.text.x = element_text(angle=45, hjust=1), plot.title = element_text(hjust = 0)) +
  xlab("County") + ylab("Spending per capita in USD") + 
  ggtitle("Highest spending per capita by county")

# Bring in some more county level data

# 2012 presidential election
election_df = read_excel("~/projects/policeMilitarySurplus/data/election2012CountyData.xls", sheet = 1)
election_df$Area_name = paste0(election_df$`County Name`, ", ", election_df$`State Postal`)
election_df = election_df[!duplicated(election_df[,"Area_name"]),]
election_df = election_df[!is.na(election_df$Area_name),]
counties_df$percentRomney = election_df$percentRomney[match(counties_df$Area_name, election_df$Area_name)]
republicanVoteSpendingCor = format(cor(counties_df$percentRomney, counties_df$spending, use = "complete"), digits = 4)
ggplot(counties_df, aes(x = percentRomney, y = log(spending))) + 
  military_theme + theme(plot.title = element_text(hjust = 0)) +
  geom_point(colour="#4b5320") + geom_smooth(method='lm', formula=y~x, colour="#454955") +
  xlab("Percent of Voters Who Voted for Romney in 2008") + scale_x_continuous(limits=c(0,100)) +
  ylab("Spending (log smoothed)") +
  ggtitle(label="Spending vs. Number of Romney Voters on a County Level",
          subtitle=paste("Correlation between Spending and Number of Romney Voters = ", republicanVoteSpendingCor))

# Population density in 2010
pop_dense_df = read_excel("~/projects/policeMilitarySurplus/data/censusCountyPopulationTotal1.xls", sheet = 3)
counties_df$popDensity = pop_dense_df$POP060210D[match(counties_df$Area_name, pop_dense_df$Area_name)]
spendPopDenseCor = format(cor(counties_df$popDensity, counties_df$spending, use = "complete"), digits = 4)
ggplot(counties_df, aes(x = log(popDensity), y = log(spending))) + 
  military_theme + theme(plot.title = element_text(hjust = 0)) +
  geom_point(colour="#4b5320") + geom_smooth(method='lm', formula=y~x, colour="#454955") +
  xlab("Population Density per Square Mile in 2010 (log smoothed)") + ylab("Spending (log smoothed)") +
  ggtitle(label="Spending vs. Population Density on a County Level",
          subtitle=paste("Correlation between spending and population density = ", spendPopDenseCor))

# Crime
crime_df = read_excel("~/projects/policeMilitarySurplus/data/censusCountyCrime1.xls", sheet = 3)
# Averge number of violent crimes per year known to police over 2006, 2007 and 2008
counties_df$crime= (crime_df$CRM110208D[match(counties_df$Area_name, crime_df$Areaname)] +
                    crime_df$CRM110207D[match(counties_df$Area_name, crime_df$Areaname)] +
                    crime_df$CRM110208D[match(counties_df$Area_name, crime_df$Areaname)]) / 3
spendCrimeCor = format(cor(counties_df$crime, counties_df$spending, use = "complete"), digits = 4)
ggplot(counties_df, aes(x = log(crime), y = log(spending))) + 
  military_theme + theme(plot.title = element_text(hjust = 0)) +
  geom_point(colour="#4b5320") + geom_smooth(method='lm', formula=y~x, colour="#454955") +
  xlab("Average Violent Crimes Known to Police per Year from 2006-2008") + ylab("Spending (log smoothed)") +
  ggtitle(label="Spending vs. Crime on a County Level",
    subtitle=paste("Correlation between spending and crime = ", spendCrimeCor))

# Poverty
income_pov_df = read_excel("~/projects/policeMilitarySurplus/data/censusCountyIncomeAndPovertyData.xls", sheet = 3)
# Average people of all ages in poverty per year from 2006-2009
counties_df$poverty = (income_pov_df$IPE110209D[match(counties_df$Area_name, income_pov_df$Areaname)] +
                         income_pov_df$IPE110208D[match(counties_df$Area_name, income_pov_df$Areaname)] +
                         income_pov_df$IPE110207D[match(counties_df$Area_name, income_pov_df$Areaname)] +
                         income_pov_df$IPE110206D[match(counties_df$Area_name, income_pov_df$Areaname)]) / 4
spendPovCor = format(cor(counties_df$poverty, counties_df$spending, use = "complete"), digits = 4)
ggplot(counties_df, aes(x = log(poverty), y = log(spending))) + 
  military_theme + theme(plot.title = element_text(hjust = 0)) +
  geom_point(colour="#4b5320") + geom_smooth(method='lm', formula=y~x, colour="#454955") +
  xlab("Average Number of People in Poverty per Year from 2006-2009 (log smoothed)") + ylab("Spending (log smoothed)") + 
  ggtitle(label="Spending vs. Poverty on a County Level", 
          subtitle=paste("Correlation between spending and poverty = ", spendPovCor))





