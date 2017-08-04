library(twitteR)

# Let's look at these tweet dataframes!
setwd('~/Dropbox/DataMining/Scraping/Visualization/tweets')
tweet_files <- list.files()

for(i in 1:50){
  load(tweet_files[i])
}

# Doing something stackexchange told me to
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

# Again, obediently copying stackexchange.
data <- do.call(rbind, dfs)

# Removing the title rows for each data set
data <- data[-which(data$text == 0), ]

# Making the created unix timestamp readable
data$date <- as.POSIXct(data$created, origin = "1970-01-01")

# Adding the collection date as a column
data$date_collection <- substr(row.names(data), 1, 8)


data$text.clean <- twit.clean(data)

# Let's save this.
save(data, file = '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/tweets.rda')
load(file = '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/tweets.rda')


#### Let's play with the results to make them how we want! ####
results <- read.csv('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/PrimaryResults.csv')

results_dem <- results[which(results$party == 'Democrat'), ]
results_dem$candidate <- droplevels(results_dem$candidate)
results_rep <- results[which(results$party == 'Republican'), ]


# Democrats first.
# Did any other candidates win other than Bernie and Hillary? Let's see.
length(which(is.element(results_dem$candidate, c('Bernie Sanders', 'Hillary Clinton'))))
levels(results_dem$candidate)
length(which(results_dem$candidate == "Martin O'Malley"))
length(which(results_dem$candidate == " No Preference"))
length(which(results_dem$candidate == " Uncommitted"))


# Now let's look more at the winners. How to do this?
results_dem_mj <- data.frame(unique(results_dem$fips))
colnames(results_dem_mj) <- 'FIPS'
results_dem_mj[ ,levels(results_dem$candidate)] <- NA
results_dem_mj <- results_dem_mj[ ,c(1, 5, 4, 6, 2, 3)]
colnames(results_dem_mj) <- c('fips', 'Clinton', 'Sanders', 'OMalley', 'NoPref', 'Uncommitted')

results_dem_mj <- results_dem_mj[-which(is.na(results_dem_mj$fips)), ]


# for(i in 1:nrow(results_dem_mj)){
#   for(j in colnames(results_dem_mj))
#     f <- results_dem_mj$FIPS[i]
#     results_dem_mj[i, j] <- results_dem$fraction_votes[which(results_dem$fips == f & results_dem$candidate == j)]
# }

for(i in 1:nrow(results_dem_mj)){
  results_dem_mj$Clinton[i] <- results_dem$fraction_votes[which(results_dem$fips == results_dem_mj$fips[i] & 
                                                                  results_dem$candidate == 'Hillary Clinton')]
}
which(is.na(results_dem_mj$Clinton))

for(i in 1:nrow(results_dem_mj)){
  results_dem_mj$Sanders[i] <- results_dem$fraction_votes[which(results_dem$fips == results_dem_mj$fips[i] & 
                                                                  results_dem$candidate == 'Bernie Sanders')]
}


results_dem_mj$OMalley <- 0
results_dem_mj$NoPref <- 0
results_dem_mj$Uncommitted <- 0
for(i in 1:nrow(results_dem_mj)){
  results_dem_mj$OMalley[i] <- results_dem$fraction_votes[which(results_dem$fips == results_dem_mj$fips[i] & 
                                                                  results_dem$candidate == "Martin O'Malley")]
}

for(i in 1:nrow(results_dem_mj)){
  results_dem_mj$NoPref[i] <- results_dem$fraction_votes[which(results_dem$fips == results_dem_mj$fips[i] & 
                                                                  results_dem$candidate == " No Preference")]
}

for(i in 1:nrow(results_dem_mj)){
  results_dem_mj$Uncommitted[i] <- results_dem$fraction_votes[which(results_dem$fips == results_dem_mj$fips[i] & 
                                                                  results_dem$candidate == " Uncommitted")]
}
