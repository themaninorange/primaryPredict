library(twitteR)
library(lubridate)
library(curl)

  #Information for twitter api
  #Make a list containing the private information as
  #
  #  twauthkeys = list(consumer_key    = "R0fLjk...",
  #                    consumer_secret = "3Af5m3...",
  #                    access_token    = "sChd0g...",
  #                    access_secret   = "57UfFz...")
  #
  #... except use your information instead of that stuff.
  #Then save that in your working directory with the names keys.list
  #
  #  save(twauthkeys, file = "keys.list")
  #
  #And you'll be good to run.
setwd("Data Mining 2/Final")
load("twauthkeys.list")
do.call(setup_twitter_oauth, twauthkeys)

namesofstuff = c("text", "favorited", "favoriteCount", "replyToSN", "created", "truncated", "replyToSID",
                 "id", "replyToUID", "statusSource", "screenName", "retweetCount", "isRetweet", "retweeted",
                 "longitude", "latitude")

trynCatch = function(expr, finally, ntimes, preerror, finalerror,  ...){
  success = FALSE
  for(i in 1:(ntimes-1)){
    print(c(i, " try from tryncatch..."))
    tryCatch(expr = parse(text=paste(
                                      expr, 
                                      "\n success = TRUE \n", 
                                      expression({break}))),
             finally = finally,
             error = preerror, ...)
  }
  if(!success){
    print("*sad trombone*")
    tryCatch(expr = expr, finally = finally, error = finalerror, ...)
  }
  print(c("Done tryn and catchn: ", i))
}

load(file = "completething.Rda")
load(file = "Keywords.Rda")

for(state in names(table(completething$STATE_NAME))[3:52]){
  #Currently iterating over state
  
  #Reset statetweets to be empty, with the appriopriate names.
  statetweets = as.data.frame(rbind(rep(0,18)))
  names(statetweets) = c(namesofstuff, "keyword", "fips")
  
  #Sets the name of the data frame as it will be saved, then the name of the file
  nameofdf = paste(c(as.character(month(Sys.Date(),label = TRUE)),day(Sys.Date()),year(Sys.Date()),"_", state),sep = "",collapse = "")
  nameoffile = paste(c("tweets/", as.character(Sys.Date()),"_", state, ".Rda"),sep = "",collapse = "")
  
  #Iterate over each entry for this state
  #In case of loop break: for(j in 1264:1312){
  for(j in ((1:(dim(completething)[1]))[completething$STATE_NAME==state])){
    #Currently iterating over counties
    
    for(i in 1:length(Keywords$Keywords)){
      decentradius = floor(sqrt(completething$SquareKm[j])/1.6)
      
      #Try to get some tweets.
      tryCatch(
        {temp1 = searchTwitteR(searchString = Keywords$Keywords[i], n=2000, lang="en",
                              geocode = paste(toString(round(completething$Latitude[j],6)),round(completething$Longitude[j],6),
                                              paste(toString(decentradius),"mi",sep = "", collapse = ""),sep = ",", collapse = ""))
        },
        error = function(cond){
          #If there's an error, do a test on the next county, and try again upon success.
          tryCatch(
            {neverevergivethisnametoavariable = searchTwitteR(searchString = Keywords$Keywords[i], n=2000, lang="en",
                                geocode = paste(toString(round(completething$Latitude[j+1],6)),round(completething$Longitude[j+1],6),
                                                paste(toString(decentradius),"mi",sep = "", collapse = ""),sep = ",", collapse = ""))
            rm(neverevergivethisnametoavariable)
            tryCatch(
              {temp1 = searchTwitteR(searchString = Keywords$Keywords[i], n=2000, lang="en",
                                   geocode = paste(toString(round(completething$Latitude[j],6)),round(completething$Longitude[j],6),
                                                   paste(toString(decentradius),"mi",sep = "", collapse = ""),sep = ",", collapse = ""))
              }, error = function(cond){
               print(c("There's a serious error with county", j))
              }
            )
            }, error = function(cond){
              #If there's an error on that test, try the original county 25 more times once every 5 minutes.
              trynCatch(expr = {
                  test = searchTwitteR(searchString = Keywords$Keywords[i], n=2000, lang="en",
                                      geocode = paste(toString(round(completething$Latitude[j],6)),round(completething$Longitude[j],6),
                                                      paste(toString(decentradius),"mi",sep = "", collapse = ""),sep = ",", collapse = ""))
                }, preerror = function(cond){
                  print("Temporary error.  Trying again.")
                  Sys.sleep(300)
                }, finalerror = function(cond){
                temp1 = NA
                print(paste(c("Error in ", state, " ", j, " for keyword, '",
                              Keywords$Keywords[i],"'.  First county to be subject to new regulations under the rabid ferret regime."),
                              sep = "", collapse = ""))
                }, ntimes = 25)
            }
          )
        }
      )
#      temp1 = searchTwitteR(searchString = Keywords$Keywords[i], n=2000, lang="en",
#                            geocode = paste(toString(round(completething$Latitude[j],6)),round(completething$Longitude[j],6),
#                                            paste(toString(decentradius),"mi",sep = "", collapse = ""),sep = ",", collapse = ""))
      if(length(temp1)>0){
        temp2 = cbind(twListToDF(temp1), keyword = Keywords$Keywords[i], fips = completething$FIPS[j])
        statetweets = rbind(statetweets, temp2)
      }
      
    }
    print(c(state, j)) 
    Sys.sleep(1)
  }
  
  #The following two lines set a dataframe of the name in the string nameofdf, 
  # and save that dataframe to a file of the name in the string nameoffile
  eval(parse(text = paste(c(nameofdf, " = statetweets"), sep = "", collapse = "")))
  eval(parse(text = paste(c("save(", nameofdf, ", file = nameoffile)"), sep = "", collapse = "")))
  print(paste(c("Saved ", state, " tweets to dataframe: ", nameoffile),collapse = "", sep = ""))
}