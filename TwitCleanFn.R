#### TWITTER TEXT CLEANING FUNCTION ####
# Requires tm package - make sure to load
library(tm)
twit.clean = function(twit.dat){
  # Not sure if necessary, but fuck it removing it anyways
  twit.dat$text.clean = gsub('<.*?>', '', twit.dat$text)
  # Removing punctuation 
  twit.dat$text.clean = gsub('[[:punct:]]', '', twit.dat$text.clean)
  # Removing Emojis and making everything text
  twit.dat$text.clean = sapply(twit.dat$text.clean, function(row) iconv(row, "latin1", "ASCII", 
                                                                        sub = ""))
  # Making everything lower case
  twit.dat$text.clean = tolower(twit.dat$text.clean)
  # Removing stopwords
  twit.dat$text.clean = removeWords(twit.dat$text.clean, stopwords("en"))
}



##### CREATING URGENT COUNTIES SHIT #####
urgent


urgentresults = counties[is.element(counties$state, urgent), ]
urgent.all.dem = c(fl.dem, il.dem, mo.dem, nc.dem, oh.dem)
urgent.all.rep = c(fl.rep, il.rep, mo.rep, nc.rep, oh.rep)

urgentresults$dem = urgent.all.dem
urgentresults$dem = as.factor(urgentresults$dem)
levels(urgentresults$dem) = c("Bernie", "Hillary")
urgentresults$rep = urgent.all.rep
urgentresults$rep = as.factor(urgentresults$rep)
levels(urgentresults$rep) = c("Trump", "Kasich", "Cruz")

urgentresults$FIPS = c(rep(0, nrow(urgentresults)))

for(i in 1:nrow(urgentresults)){
  urgentresults$FIPS[i] = paste0(urgentresults$fips_state[i], urgentresults$fips_county[i])
}
urgentresults = urgentresults[, -(1:4)]


save(urgentresults, file = "~/Dropbox/DataMining/Scraping/Visualization/DataFunctions/urgentresults.Rda")


new.all.dem = c(az.dem, id.dem, ut.dem)
new.all.dem = as.character(new.all.dem)
levels(new.all.dem) = c("Bernie", "Hillary")
new.all.rep = c(az.rep, id.rep, ut.rep)
new.all.rep = as.character(new.all.rep)
levels(new.all.rep) = c("Trump", "Kasich", "Cruz")
new = c("Arizona", "Idaho", "Utah")
newresults = counties[is.element(counties$state, new), ]
newresults$dem = new.all.dem
newresults$rep = new.all.rep
newresults$fips = c(rep(0, nrow(newresults)))
for(i in 1:nrow(newresults)){
  newresults$fips[i] = paste0(newresults$fips_state[i], newresults$fips_county[i])
}
newresults = newresults[ , -(1:4)]
newresults$fips = as.character(newresults$fips)


for(i in 1:15){
  newresults$fips[i] = substr(newresults$fips[i], 2, 5)
}
newresults$dem = factor(newresults$dem)
levels(newresults$dem) = c("Bernie", "Hillary")

newresults$rep = factor(newresults$rep)
levels(newresults$rep) = c("Trump", "Kasich", "Cruz")
