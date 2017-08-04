#####################
##### Libraries #####
#####################
library(dplyr)
library(noncensus)



########################
##### Loading Data #####
########################
results <- read.csv('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/PrimaryResults.csv')
demographics <- read.csv('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/CountyDemographics.csv')

data("zip_codes")



#####################
###### Results ######
#####################
length(unique(as.character(results$fips)))

# What are the weird fips codes (which states do they come from)
unique(results$state[which(nchar(as.character(results$fips)) > 5)])

results.data <- results[which(results$state != 'Alaska'), ]
results.data <- results.data[which(results.data$state != 'Connecticut'), ]
results.data <- results.data[which(results.data$state != 'Illinois'), ]
results.data <- results.data[which(results.data$state != 'Kansas'), ]
results.data <- results.data[which(results.data$state != 'Maine'), ]
results.data <- results.data[which(results.data$state != 'Massachusetts'), ]
results.data <- results.data[which(results.data$state != 'North Dakota'), ]
results.data <- results.data[which(results.data$state != 'Rhode Island'), ]
results.data <- results.data[which(results.data$state != 'Vermont'), ]
results.data <- results.data[which(results.data$state != 'Wyoming'), ]

results.data$fips <- str_pad(as.character(results.data$fips), 5, side = "left", pad = "0")

results.data <- results.data[ , 4:8]



### Save this for later
# # We have the cities, let's allocate them to their respective counties
# zips <- select(zip_codes, city, state, fips)
# zips <- zips[which(zips$state =='AL' | zips$state == 'CT' | zips$state == 'RI' | zips$state == 'ND'), ]
# zips <- zips[!duplicated(zips[, c('city', 'state')]), ]
# 
# results.data <- results[which(results$state == 'North Dakota' | 
#                                 results$state == 'Wisconsin' | 
#                                 results$state == 'New York' | 
#                                 results$state == 'Connecticut' | 
#                                 results$state == 'Delaware' | 
#                                 results$state == 'Maryland' | 
#                                 results$state == 'Pennsylvania' | 
#                                 results$state == 'Rhode Island' |
#                                 results$state == 'Indiana' | 
#                                 results$state == 'Nebraska' | 
#                                 results$state == 'West Virginia' | 
#                                 results$state == 'Kentucky' | 
#                                 results$state == 'Oregon' | 
#                                 results$state == 'Washington' | 
#                                 results$state == 'California' | 
#                                 results$state == 'Montana' | 
#                                 results$state == 'New Jersey ' | 
#                                 results$state == 'South Dakota' | 
#                                 results$state == 'New Mexico'), ]
# 
# 
# results.republican <- results.data[results.data$party == 'Republican',]
# results.democrat <- results.data[results.data$party == 'Democrat', ]
# 
# results.republican$state <- droplevels(results.republican$state)
# results.democrat$state <- droplevels(results.democrat$state)

##########################
###### Demographics ######
##########################
library(stringr)
# Need to fix the fips codes for the states codes up until 10 - they are missing a character.
demographics$fips <- str_pad(as.character(demographics$fips), 5, side = "left", pad = "0")

x <- which(demographics$state_abbreviation == '')
demographics <- demographics[-x, ]

y <- c(2, 3)
colnames(demographics)
demographics <- demographics[, -y]

demographics$year <- 2010

demographics <- demographics[ ,c(1, 53, 2:52)]

write.csv(demographics, 
          file = "~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/county_demos_fips.csv")
