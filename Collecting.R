library(twitteR)

setup_twitter_oauth(consumer_key="rejRHFs2W0JA41vLUxGdOJWXA",
                    consumer_secret ="ErnEMsyMLmDOlYNIGE6pD36qhTulnQUcHAG7RwGNIMbeaupZzG",
                    access_token="272953871-SjsyWzXRK5nfty6ZbcaRjiJIMRf59rDJggu0Aw6g",
                    access_secret="nCScvoxVQx1ZVaKUHqX2hDcbCU8h9dZOlo0yHpyaLLhEJ")


namesofstuff = c("text", "favorited", "favoriteCount", "replyToSN", "created", "truncated", "replyToSID",
                 "id", "replyToUID", "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted",
                 "longitude", "latitude")
bigsetoftweets = as.data.frame(rbind(rep(0,18)))
names(bigsetoftweets) = c(namesofstuff, "keyword", "fips")
urgent = c("Florida", "Illinois","Missouri", "North Carolina", "Ohio")

urgentthings = completething[is.element(completething$STATE_NAME, urgent),]

for(i in 6:10){
  for(j in 1:length(urgentthings$Latitude)){
    decentradius = floor(sqrt(urgentthings$SquareKm[j])/1.6)
    temp1 = searchTwitteR(searchString = Keywords$Keywords[i], n=1000, lang="en",
                          geocode = paste(toString(round(urgentthings$Latitude[j],6)),round(urgentthings$Longitude[j],6),
                                          paste(toString(decentradius),"mi",sep = "", collapse = ""),sep = ",", collapse = ""))
    if(length(temp1)>0){
      temp2 = cbind(twListToDF(temp1), keyword = Keywords$Keywords[i], fips = urgentthings$FIPS[j])
      bigsetoftweets = rbind(bigsetoftweets, temp2)
    }
    print(j)
  }
}

mikaela.tweet = bigsetoftweets
mikaela.tweet$text.clean = twat.clean(mikaela.tweet)

save(mikaela.tweet, file = "~/Dropbox/DataMining/Scraping/Visualization/DataFunctions/mikaelacleanurgent.Rda")
save(bigsetoftweets, file = "~/Dropbox/DataMining/Scraping/Visualization/DataFunctions/mikaeladirtyurgent.Rda")

############
