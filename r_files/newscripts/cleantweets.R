#Process tweets collected from multiple states from multiple dates.
#  - filter by dates of primaries to ensure no backward contamination
#  - create floating model which checks accuracy for each relevant date.

library(tm)
library(stringr)
library(pROC)
library(randomForest)
library(dclone)

setwd(dir = "~/Dropbox/Visualization/")
load("statenames.list")
primaryschedule = read.table("primaryschedule.txt",  sep="\t", header=TRUE)
normdate = function(v){
  thing = c()
  for(i in 1:length(v)){
    thing[i] = as.character(as.Date(as.character(paste(c(substr(v[i], 6, nchar(v[i])), 
                                      ", 2016"), collapse = '')), format = "%b %d, %Y"))
  }
  return(thing)
}

cleandates = normdate(as.character(primaryschedule$Date))
primaryschedule$Date = cleandates

dirtyframes = list.files(path = "./tweets/", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

load("tweets/2016-04-04_Alabama.Rda")
templatedf = Apr42016_Alabama[FALSE,]

#loop through all states and load items from dirtyframes
for(state in statenames){
  
  
  primdate = grep(pattern = state, x= primaryschedule$State)
  if(length(primdate)>1){
    primdate = primdate[!is.na(match(x = primdate, table = grep(pattern = "(R)", x = primaryschedule$State)))]
  }
  primdate = primaryschedule$Date[primdate]
  for(filenum in grep(pattern = state, x = dirtyframes)){
    
    if(as.Date(substr(dirtyframes[filenum], start = 0,stop = 10))< as.Date(primdate)){
      print(dirtyframes[filenum])
      
      framename = substr(dirtyframes[filenum], 0, nchar(dirtyframes[filenum])-4)
      
      eval(expr = parse(text = paste(c())))
      
    }
  }
}

#  - filter by date TBD.
#  - produce clean text.
#  - stick a fips code on it, and store it in one large data frame.