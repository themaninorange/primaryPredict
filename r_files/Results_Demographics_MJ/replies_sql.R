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

keep <- c(1:5, 7:9, 13:14, 29:38, 53:59, 61:64)

data.replies <- data[keep, c(8, 7, 4)]
data.replies <- data.replies[which(is.na(data.replies$replyToSID) == FALSE), ]

colnames(data.replies) <- c("tweet_id", "reply_to_id", "reply_to_uid")






#### Creating the table ####
out_file <- '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/replies.txt'
columns <- c('VARCHAR(18)', 'VARCHAR(18)', 'VARCHAR(20)')
constraints <- 'CONSTRAINT replies_pk PRIMARY KEY (tweet_id)'

table.creation(out_file, data.replies, columns, constraints, 'replies')




### OLD ####
sink(output_filename)
cat('CREATE TABLE replies ( \n')
sink()

sink(output_filename, append = TRUE)
for(i in 1:n){
  col <- paste(colnames(data.replies)[i], columns[i], sep = '   ')
  if(i <= n-1){
    col <- paste0(col, ',')
  }
  cat(col,sep = '\n', append = TRUE)
}
cat(');\n', append = TRUE)
sink()

sink(output_filename, append = TRUE)
for(i in 1:r){
  row.val <- 'INSERT INTO replies'
  row.val <- paste(row.val, 'VALUES(', sep = '\n\t')
  tweetid <- paste0('"', data.replies[i, 1], '"')
  replytoid <- paste0('"', data.replies[i, 2], '"')
  replytouid <- paste0('"', data.replies[i, 3], '"')
  row.val <- paste(row.val, tweetid, ',', replytoid, ',', replytouid, ');')
  cat(row.val, sep = '\n', append = TRUE)
}
sink()

