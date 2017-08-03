#### MERGING SCRIPT ####

load("~/Dropbox/DataMining/Scraping/Visualization/TwitterDataiMac.Rda")
adam.1 = bigsetoftweets

load("~/Dropbox/DataMining/Scraping/Visualization/TwitterDataMacBook.Rda")
adam.2 = bigsetoftweets

load("~/Dropbox/DataMining/Scraping/Visualization/mikaeladirtyurgent.Rda")
mikaela = bigsetoftweets


twitter.data = rbind(adam.1, adam.2, mikaela)

twitter.data$text.clean = twat.clean(twitter.data)

# Adding Candidate to FIPS
tw.data = merge(twitter.data, urgentresults, by.y = "fips")

save(tw.data, file = "~/Dropbox/DataMining/Scraping/Visualization/tw.data.Rda")

tw.data$rep = as.factor(tw.data$rep)
levels(tw.data$rep) = c("Trump", "Kasich", "Cruz")
