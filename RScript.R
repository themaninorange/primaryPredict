library(tm)
# install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer)
library(twitteR)

## Twitter Setup Stuff ##
#setup_twitter_oauth(*********************)


## Scraping Tweets, Cleaning Tweets, Etc ##
# Searching Twitter for Democrat Tweets
democrat = searchTwitteR("Democrat", n=10, lang = "en")

# Converting Democrat Tweets into Data Frame and Cleaning Tweets
dem.df = twListToDF(democrat)
dem.df$text.clean = gsub('<.*?>', '', dem.df$text)
dem.df$text.clean = gsub('[[:punct:]]', '', dem.df$text.clean)
dem.df$text.clean <- sapply(dem.df$text.clean, function(row) iconv(row, "latin1", "ASCII", 
                                                                   sub = ""))
dem.df$text.clean = tolower(dem.df$text.clean)
dem.df$text.clean = removeWords(dem.df$text.clean, stopwords("en"))
remove(democrat)

dem.corpus <- Corpus(DataframeSource(data.frame(dem.df[, 17])))
tdm <- TermDocumentMatrix(dem.corpus)
dem.tdm <- as.matrix(tdm)
v <- sort(rowSums(dem.tdm), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
pal <- brewer.pal(9, "Blues")
pal <- pal[-(1:4)]
png(file = '~/Dropbox/DataMining/Scraping/Visualization/wcdemwithRT.png')
wordcloud(d$word, d$freq, scale = c(5, 1), min.freq = 10, max.words = 50, random.order = T, 
          rot.per = 0.15, colors = pal)
dev.off()


# Without Retweets
dem.df.wo = dem.df[dem.df$isRetweet == FALSE, ]

dem.wo.corpus <- Corpus(DataframeSource(data.frame(dem.df.wo[, 17])))
tdm <- TermDocumentMatrix(dem.wo.corpus)
dem.wo.tdm <- as.matrix(tdm)
v <- sort(rowSums(dem.wo.tdm), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
pal <- brewer.pal(9, "Blues")
pal <- pal[-(1:4)]
png(file = '~/Dropbox/DataMining/Scraping/Visualization/wcdemwithoutRT.png')
wordcloud(d$word, d$freq, scale = c(5, .8), min.freq = 10, max.words = 50, random.order = T, 
          rot.per = 0.15, colors = pal, )
dev.off()

# Searching Twitter for Rep Tweets
rep = searchTwitteR("Republican", n = 2000, lang = "en")

# Converting Rep Tweets into DF and Cleaning Tweets
rep.df = twListToDF(rep)
rep.df$text.clean = gsub('<.*?>', '', rep.df$text)
rep.df$text.clean = gsub('[[:punct:]]', '', rep.df$text.clean)
rep.df$text.clean <- sapply(rep.df$text.clean, function(row) iconv(row, "latin1", "ASCII", sub = ""))
rep.df$text.clean = tolower(rep.df$text.clean)
rep.df$text.clean = removeWords(rep.df$text.clean, stopwords('en'))
remove(rep)

rep.corpus <- Corpus(DataframeSource(data.frame(rep.df[, 17])))
tdm <- TermDocumentMatrix(rep.corpus)
rep.tdm <- as.matrix(tdm)
v <- sort(rowSums(rep.tdm), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
pal <- brewer.pal(9, "Reds")
pal <- pal[-(1:3)]

png(file = '~/Dropbox/DataMining/Scraping/Visualization/wcrepwithRT')
wordcloud(d$word, d$freq, scale = c(4.5, .5), min.freq = 50, max.words = 100, random.order = T, 
          rot.per = 0.15, colors = pal)
dev.off()


# Without Retweets
rep.df.wo = rep.df[rep.df$isRetweet == FALSE, ]

rep.wo.corpus <- Corpus(DataframeSource(data.frame(rep.df.wo[, 17])))
tdm <- TermDocumentMatrix(rep.wo.corpus)
rep.wo.tdm <- as.matrix(tdm)
v <- sort(rowSums(rep.wo.tdm), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
pal <- brewer.pal(9, "Reds")
pal <- pal[-(1:3)]
png(file = '~/Dropbox/DataMining/Scraping/Visualization/wcrepwithoutRT.png')
wordcloud(d$word, d$freq, scale = c(5, 1), min.freq = 10, max.words = 50, random.order = T, 
          rot.per = 0.15, colors = pal)
dev.off()


# Function that searches twitter for keyword tweets, cleans texts, and creates wordcloud

twitter.word.cloud = function(keyword, n, scale, min.freq, color, ncolor, max.words,
                              isRetweet = TRUE){
  temp = searchTwitteR("keyword", n = n, lang = "en")
  temp.df = twListToDF(temp)
  temp.df$text.clean = gsub('<.*?>', '', temp.df$text)
  temp.df$text.clean = gsub('[[:punct:]]', '', temp.df$text.clean)
  temp.df$text.clean <- sapply(temp.df$text.clean, function(row) iconv(row, "latin1", "ASCII", sub = ""))
  temp.df$text.clean = tolower(temp.df$text.clean)
  temp.df$text.clean = removeWords(temp.df$text.clean, stopwords("en"))
  if(isRetweet == FALSE){
    temp.df = dem.df[dem.df$isRetweet == FALSE, ]
  }
  corpus <- Corpus(DataframeSource(data.frame(temp.df[ , ncol(temp.df)])))
  tdm <- TermDocumentMatrix(corpus)
  tdm <- as.matrix(tdm)
  v <- sort(rowSums(tdm), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  pal <- brewer.pal(ncolor, color)
  pal <- pal[-(1:3)]
  wordcloud(d$word, d$freq, scale = scale, min.freq = min.freq, max.words = max.words, 
                   random.order = T, rot.per = 0.15, colors = pal)
}
# Bernie
png(file = '~/Dropbox/DataMining/Scraping/Visualization/wcberniewoRT')
twitter.word.cloud(Bernie, 2000, scale = c(7, .3), min.freq = 20, color = "Blues", ncolor = 9, max.words = 100,
                   isRetweet = FALSE)
dev.off()

# Hillary
png(file = '~/Dropbox/DataMining/Scraping/Visualization/wchillarywoRT')
twitter.word.cloud(Hillary, 2000, scale = c(8, .3), min.freq = 20, color = "Blues", ncolor = 9, max.words = 100,
                   isRetweet = FALSE)
dev.off()

# Trump
png(file = '~/Dropbox/DataMining/Scraping/Visualization/wctrumpwoRT')
twitter.word.cloud(Trump, 2000, scale = c(5, .3), min.freq = 20, color = "Reds", ncolor =9, max.words = 100, 
                   isRetweet = FALSE)
dev.off()

# Cruz
png(file = '~/Dropbox/DataMining/Scraping/Visualization/wccruzwoRT')
twitter.word.cloud(Cruz, 2000, scale = c(5, .3), min.freq = 20, color = "Reds", ncolor = 9, max.words = 100, 
                   isRetweet = FALSE)
dev.off()

# rep.vect = apply(rep.tdm, 1, sum)
# dem.vect = apply(dem.tdm, 1, sum)
# rep.vect = as.data.frame(as.character(rownames(rep.vect)), rep.vect)
# dem.vect = as.data.frame(dem.vect)
# 
# tf = merge(rep.vect, dem.vect, by = , all = TRUE)


temp.df$text.clean = gsub('<.*?>', '', temp.df$text)
temp.df$text.clean = gsub('[[:punct:]]', '', temp.df$text.clean)
temp.df$text.clean <- sapply(temp.df$text.clean, function(row) iconv(row, "latin1", "ASCII", sub = ""))
temp.df$text.clean = tolower(temp.df$text.clean)
temp.df$text.clean = removeWords(temp.df$text.clean, stopwords("en"))
# Cleaning Text Function
clean.text = function(x){
  x = gsub("rt", "", x)
  x = gsub("@\\w+", "", x)
  x = gsub("[[:punct:]]", "", x)
  x = gsub("[[:digit:]]", "", x)
  x = gsub("http\\w+", "", x)
  # x = gsub("[ |\t]{2, }", "", x)
  x = gsub("^ ", "", x)
  x = gsub(" $", "", x)
  x = sapply(x, function(x) iconv(x, "latin1", "ASCII", sub = ""))
  x = tolower(x)
  x = removeWords(x, stopwords("en"))
}

# Getting and Cleaning Text
dem.text = sapply(democrat, function(x) x$getText())
rep.text = sapply(rep, function(x) x$getText())
dem.clean = clean.text(dem.text)
rep.clean = clean.text(rep.text)

# Joining individual vectos
dem = paste(dem.clean, collapse = '')
republican = paste(rep.clean, collapse = '')

# Combining
all = c(dem, republican)

# Creating Corpus
corpus = Corpus(VectorSource(all))

# Creating Term Document Matrix
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = c("Democrat", "Republican")

# Plotting Comparison Word Cloud Democrat versus Republican
comparison.cloud(tdm, scale = c(5, 1), random.order = FALSE, colors = c("blue", "red"), 
                 title.size = 1.5, max.words = 50)
commonality.cloud(tdm, scale = c(5, 1), random.order = FALSE, colors = brewer.pal(8, "Dark2"), 
                  title.size = 1.5, max.words = 50)


# Function that creates comparison and commonality cloud for two keywords
comp.com.cloud = function(keyword1, n1, keyword2, n2, scale, color1, color2, title.size, 
                          max.words){
  temp1 = searchTwitteR("keyword1", n = n1, lang = "en")
  temp2 - searchTwitteR("keyword2", n = n2, lang = "en")
  temp1.text = sapply(temp1, function(x) x$getText())
  temp2.text = sapply(temp2, function(x) x$getText())
  temp1.clean = clean.text(temp1.text)
  temp2.clean = clean.text(temp2.text)
  temp1.paste = paste(temp1.clean, collapse = ' ')
  temp2.paste = paste(temp2.clean, collapse = ' ')
  all = c(temp1.paste, temp2.paste)
  corpus = Corpus(VectorSource(all))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = c("keyword1", "keyword2")
  return(comparison.cloud(tdm, scale = scale, random.order = FALSE, colors = color1, 
                          title.size = title.size, max.words = max.words), 
         commonality.cloud(tdm, scale = scale, random.order = FALSE, colors = color2, 
                           title.size = title.size, max.words = max.words))
}
comp.com.cloud(Bernie, 10, Hillary, 10, scale = c(5, 1), color1 = c("blue", "magenta"), 
               color2 = brewer.pal(8, "Dark2"), title.size = 1.5, max.words = 50)

#calculate standardized MDS coordinates
dat <- sweep(USArrests,2,colMeans(USArrests))
dat <- sweep(dat,2,sqrt(diag(var(dat))),"/")
loc <- cmdscale(dist(dat))
#plot with no overlap
textplot(loc[,1],loc[,2],rownames(loc))
#scale by urban population size
textplot(loc[,1],loc[,2],rownames(loc),cex=USArrests$UrbanPop/max(USArrests$UrbanPop))
#x limits sets x bounds of plot, and forces all words to be in bounds
textplot(loc[,1],loc[,2],rownames(loc),xlim=c(-3.5,3.5))
#compare to text (many states unreadable)
plot(loc[,1],loc[,2],type="n")
text(loc[,1],loc[,2],rownames(loc))
