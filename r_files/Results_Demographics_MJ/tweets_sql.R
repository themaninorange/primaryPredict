source('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/sql_functions.R')

#### Playing with the Data ####
library(twitteR)
# Let's look at these tweet dataframes!
setwd('~/Dropbox/DataMining/Scraping/Visualization/tweets')
tweet_files <- list.files()

for(i in 1:51){
  load(tweet_files[i])
}


dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

data <- do.call(rbind, dfs)
data <- data[-which(data$text == 0), ]

rm(list=ls(pattern="Apr"))
rm(dfs)
rm(tweet_files)
rm(i)

data$date <- substr(row.names(data), 1, 8)
row.names(data) <- NULL


colnames(data.tweets) <- c("tweet_id", "keyword", "text", "fips_code", "rt", "user_id", "tweet_date", "orig_id")

data.tweets <- data.tweets[, c(1:7, 10, 8:9)]


keep <- c(1:5, 7:9, 13:14, 29:38, 53:59, 61:64)

data.tweets <- data[keep, c(8, 17, 1, 18, 13, 11, 19)]

data.tweets$orig_id <- data.tweets$id
data.tweets$orig_id[1:3] <- c(data.tweets$id[4], data.tweets$id[4], data.tweets$id[4])
data.tweets$orig_id[7] <- data.tweets$id[8]
data.tweets$orig_id[11:14] <- c(data.tweets$id[15], data.tweets$id[15], data.tweets$id[15], data.tweets$id[15])
data.tweets$orig_id[16:19] <- c(data.tweets$id[20], data.tweets$id[20], data.tweets$id[20], data.tweets$id[20])


data.tweets$date <- paste0(substr(tolower(data.tweets$date), 1, 3), '-', '0', substr(data.tweets$date, 4, 4), '-', '2016')

data.tweets$text_clean <- twit.clean(data.tweets)
data.tweets <- data.tweets[, c(1:2, 9, 4:8)]
colnames(data.tweets)[3] <- 'text'



#### Creating the table ####
out_file <- '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/tweets.txt'
columns <- c('VARCHAR(18)', 'VARCHAR(20)', 'VARCHAR(140)', 'VARCHAR(5)', 'NUMBER(1)', 'VARCHAR(20)', 'DATE', 'VARCHAR(18)')
constraints <- 'CONSTRAINT tweets_pk PRIMARY KEY (tweet_id)'

table.creation(out_file, data.tweets, columns, constraints, 'tweets')




#### OLD ####
sink(output_filename)
cat('CREATE TABLE tweets ( \n')
sink()

sink(output_filename, append = TRUE)
for(i in 1:n){
  col <- paste(colnames(data.tweets)[i], columns[i], sep = '   ')
  if(i <= n-1){
    col <- paste0(col, ',')
  }
  cat(col,sep = '\n', append = TRUE)
}
cat(');\n', append = TRUE)
sink()

sink(output_filename, append = TRUE)
for(i in 1:r){
  row.val <- 'INSERT INTO tweets'
  row.val <- paste(row.val, 'VALUES(', sep = '\n\t')
  tweetid <- paste0('"', data.tweets[i, 1], '"')
  keyword <- paste0('"', data.tweets[i, 2], '"')
  text <- paste0('"', data.tweets[i, 3], '"')
  fips <- paste0('"', data.tweets[i, 4], '"')
  origid <- paste0('"', data.tweets[i, 8], '"')
  userid <- paste0('"', data.tweets[i,6], '"')
  tweetdate <- paste0('TO_DATE(', '"', data.tweets[i, 7], '"', ',', '"', 'mon-dd-YYYY', '"')
  row.val <- paste(row.val, tweetid, ',', keyword, ',', text, ',', fips, ',', data.tweets[i, 5], ',', userid, ',', 
                   tweetdate,',', origid, ');')
  cat(row.val, sep = '\n', append = TRUE)
}
sink()

