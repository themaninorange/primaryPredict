library(twitteR)
library(maps)

#Information for twitter app
#****
#****
#****
#****

#Connecting
setup_twitter_oauth(key, secret, token, token_secret)

#Searching for "Bernie"
search.results=searchTwitteR("Bernie",n=1001,lang="en")

search.results=searchTwitteR("Bernie",n=1001,lang="en", locale=J)

temp=twListToDF(search.results)

temp=temp$text

temp = gsub('<.*?>', '', temp)

#Removing Punctuation
temp = gsub('[[:punct:]]','',temp)

#Converting to Lower Case
temp = tolower(temp)

countieslist = read.csv(file = "../Data Mining 2 Final/bdfw/Export_county_centers.txt",header = TRUE)
stringulus = readChar(con = "../Data Mining 2 Final/bdfw/countyarea.txt",nchars = file.info("../Data Mining 2 Final/bdfw/countyarea.txt")$size)
stringulus = gsub(pattern = "[ ]{2,}",replacement = ",",stringulus)
write(file = "../Data Mining 2 Final/bdfw/cleancountyarea.txt",x = stringulus)
attempt = read.csv("../Data Mining 2 Final/bdfw/cleancountyarea.txt",header = TRUE,sep = ",")
View(attempt)
states = read.csv("../Data Mining 2 Final/bdfw/state names.txt",header = FALSE)

countiesarea = read.csv(file = "../Data Mining 2 Final/bdfw/countyarea.txt",header = TRUE,sep = "",row.names = FALSE)
View(countiesarea)

head(states.counties)
vec = 1
for(i in 1:length(states.counties$no.count)){
  vec[i] = sum(states.counties$no.count[1:i])
}
states.counties = cbind(states.counties, Index = vec)

attempt$Name = as.character(attempt$Name)
states.counties$State = as.character(states.counties$State)

vec = vec + 1:51
vec = c(0,vec)

statevec = c("dangus")
for(i in 1:length(states.counties$Index)){
  statevec[(vec[i]+2):vec[i+1]] = states.counties$State[i]
}

attempt = attempt[complete.cases(attempt[,7]),]

attempt$State=as.character(attempt$State)
completething = cbind(countieslist[order(countieslist[,3], countieslist[,2]),], SquareKm = attempt[order(attempt[,7],attempt[,6]),]$Sq.Ki.)

#write.csv(x = completething[,2:9],sep = ",",file = "../Data Mining 2 Final/bdfw/allofit.csv",row.names = FALSE)

completething = read.csv("../Data Mining 2 Final/bdfw/allofit.csv",header = TRUE,sep = ",")


search.results=searchTwitteR("Rumors of Spring",n=100,lang="en",geocode = '37.7833, -122.4167, 20mi')
sanfrantw = twListToDF(search.results)

search.results=searchTwitteR(searchString = "Rumors of Spring",n=100,lang="en",geocode = '36.6200, -94.3500, 100mi')
rednecktw = twListToDF(search.results)

sf.donald = searchTwitter("election", n = 100, lang = "en", geocode='37.781157,-122.39720,100mi')
sfdonald = twListToDF(sf.donald)
View(sfdonald)
############



############
library(twitteR)

namesofstuff = c("text", "favorited", "favoriteCount", "replyToSN", "created", "truncated", "replyToSID",
                 "id", "replyToUID", "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted",
                 "longitude", "latitude")
bigsetoftweets = as.data.frame(rbind(rep(0,18)))
names(bigsetoftweets) = c(namesofstuff, "keyword", "fips")
urgent = c("Florida", "Illinois","Missouri", "North Carolina", "Ohio")

urgentthings = completething[is.element(completething$STATE_NAME, urgent),]

for(i in 1:5){
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
############ 
