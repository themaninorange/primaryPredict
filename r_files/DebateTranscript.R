filename = "~/Dropbox/BDFW/BernDownForWhat/dem1.txt"
dem1 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/dem2.txt"
dem2 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/dem3.txt"
dem3 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/dem4.txt"
dem4 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/dem5.txt"
dem5 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/dem6.txt"
dem6 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/dem7.txt"
dem7 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/dem8.txt"
dem8 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep1.txt"
rep1 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep2.txt"
rep2 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep3.txt"
rep3 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep4.txt"
rep4 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep5.txt"
rep5 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep6.txt"
rep6 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep7.txt"
rep7 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep8.txt"
rep8 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep9.txt"
rep9 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep10.txt"
rep10 = readChar(filename, file.info(filename)$size)
filename = "~/Dropbox/BDFW/BernDownForWhat/rep11.txt"
rep11 = readChar(filename, file.info(filename)$size)


library(tm)



### CREATING DEMOCRATIC DEBATE TEXT DATA ###
dem.deb.data = rbind(dem1, dem2, dem3, dem4, dem5, dem6, dem7, dem8)

### CLEANING DEMOCRATIC DEBATE TEXT DATA ###
clean.deb.text = function(text){
  text = gsub("[[:punct:]]", "", text)
  text = gsub("[[:digit:]]", "", text)
  text = tolower(text)
  text = removeWords(text, stopwords("en"))
}

dem.deb.clean = clean.deb.text(dem.deb.data)



dem.deb.corpus = Corpus(DataframeSource(data.frame(dem.deb.clean[ , 2])))
tdm = TermDocumentMatrix(dem.deb.corpus)
tdm = as.matrix(tdm)
v <- sort(rowSums(tdm), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
pal <- brewer.pal(9, "Blues")
pal <- pal[-(1:4)]
wordcloud(d$word, d$freq, scale = c(5, 1), min.freq = 10, max.words = 50, random.order = T, 
          rot.per = 0.15, colors = pal)

#### CREATING REPUBLIC DEBATE TEXT DATA ### 
rep.deb.data = rbind(rep1, rep2, rep3, rep4, rep5, rep6, rep7, rep8, rep9, rep10, rep11)

rep.deb.clean = clean.deb.text(rep.deb.data)

rep.deb.corpus = Corpus(DataframeSource(data.frame(rep.deb.clean)))
tdm = TermDocumentMatrix(rep.deb.corpus)
tdm = as.matrix(tdm)
v <- sort(rowSums(tdm), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
pal <- brewer.pal(9, "Reds")
pal <- pal[-(1:4)]
wordcloud(d$word, d$freq, scale = c(5, 1), min.freq = 10, max.words = 50, random.order = T, 
          rot.per = 0.15, colors = pal)
