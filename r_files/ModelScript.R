# MODELS # 
library(tm)
library(stringr)
library(pROC)



# Word Frequency Function
word.freq <- function(document.vector, sparsity = .999){
  # construct corpus
  temp.corpus <- Corpus(VectorSource(document.vector))
  # construct tf matrix and remove sparse terms
  temp.tf <- DocumentTermMatrix(temp.corpus, control = list(stopwords = stopwords('english'),removeNumbers = T))
  temp.tf <- removeSparseTerms(temp.tf, sparsity)
  temp.tf <- as.matrix(temp.tf)
  # construct word freq df
  freq.df <- colSums(temp.tf)
  freq.df <- data.frame(word = names(freq.df), freq = freq.df)
  rownames(freq.df) <- NULL
  return(freq.df)
}

# NDSI Frequency Function
library(stringr)
ndsi.freq = function(x){
  str_count(x, freq.imdb.all$word)
}



# TF IDF SHIT
# Two data frames::::
# One for Republican
# Tweet with clean text
# FIPS code for location of each tweet
# Majority winner for that county
# One for Democrat
# Tweet with clean text
# FIPS code for location of each tweet
# Majority winner for that county

# We want to get word frequencies for each candidate, using this function
# Need to label each county (fips) as Bernie/Hillary/Trump/Kasich/Cruz
word.freq.urgent.trump = word.freq(tw.data$text.clean[tw.data$rep == "Trump"], sparsity = 0.999)
word.freq.urgent.kasich = word.freq(tw.data$text.clean[tw.data$rep == "Kasich"], sparsity = 0.999)
word.freq.urgent.cruz = word.freq(tw.data$text.clean[tw.data$rep == "Cruz"], sparsity = 0.999)

word.freq.urgent.all = merge(word.freq.urgent.trump, word.freq.urgent.kasich, by = "word", all = TRUE)
word.freq.urgent.all = merge(word.freq.urgent.all, word.freq.urgent.cruz, by = "word", all = TRUE)
names(word.freq.urgent.all) = c("word", "freq.trump", "freq.kasich", "freq.cruz")

# Then remove NA's

word.freq.urgent.all$freq.trump[is.na(word.freq.urgent.all$freq.trump)] = 0 
word.freq.urgent.all$freq.kasich[is.na(word.freq.urgent.all$freq.kasich)] = 0 
word.freq.urgent.all$freq.cruz[is.na(word.freq.urgent.all$freq.cruz)] = 0 

word.freq.urgent.all$freq.trump = word.freq.urgent.all$freq.trump/length(which(tw.data$rep == "Trump"))
word.freq.urgent.all$freq.kasich = word.freq.urgent.all$freq.kasich/length(which(tw.data$rep == "Kasich"))
word.freq.urgent.all$freq.cruz = word.freq.urgent.all$freq.cruz/length(which(tw.data$rep == "Cruz"))


# First, let's try naive Bayes with DTM need one against one, one against rest
library(e1071)
b.o.w = DocumentTermMatrix(Corpus(VectorSource(tw.data$text.clean)))
b.o.w = removeSparseTerms(b.o.w, 0.99)
b.o.w = as.matrix(b.o.w)
dtm.class.data = data.frame(rep = tw.data$rep, b.o.w)
train = trainsample(dtm.class.data, .7)

# One against One
traindata = dtm.class.data[train, ]

# Trump/Kasich
tr.kas.train = traindata[(traindata$rep == "Trump") | (traindata$rep == "Kasich"), ]
tr.kas.model = naiveBayes(rep~., data = tr.kas.train)
tr.kas.pred = predict(tr.kas.model, newdata = dtm.class.data[-train, ], type = "class")

# Trump/Cruz
tr.cr.train = traindata[(traindata$rep == "Trump") | (traindata$rep == "Cruz"), ]
tr.cr.model = naiveBayes(rep~., data = tr.cr.train)
tr.cr.pred = predict(tr.cr.model, newdata = dtm.class.data[-train, ], type = "class")

# Cruz/Kasich
cr.kas.train = traindata[(traindata$rep == "Cruz") | (traindata$rep == "Kasich"), ]
cr.kas.model = naiveBayes(rep~., data = cr.kas.train)
cr.kas.pred = predict(cr.kas.model, newdata = dtm.class.data[-train, ], type = "class")


# Votes
tr.votes = (tr.kas.pred == "Trump") + (tr.cr.pred == "Trump") + (cr.kas.pred == "Trump")
cr.votes = (tr.kas.pred == "Cruz") + (tr.cr.pred == "Cruz") + (cr.kas.pred == "Cruz")
kas.votes = (tr.kas.pred == "Kasich") + (tr.cr.pred == "Kasich") + (cr.kas.pred == "Kasich")

votes = cbind(tr.votes, cr.votes, kas.votes)
apply(votes, 1, which.max)

one.v.one.pred = c("Trump", "Cruz", "Kasich")[apply(votes, 1, which.max)]

confmatrix(dtm.class.data$rep[-train], one.v.one.pred)  # 0.5703396 accuracy


# One against rest
# relevel
levels(dtm.class.data$rep) = c(levels(dtm.class.data$rep), "other")

traindata = dtm.class.data[train, ]

# Trump vs. Rest
trump = dtm.class.data
trump$rep[(trump$rep != "Trump")] = "other"
trump.train = traindata
trump.train$rep[(trump.train$rep != "Trump")] = "other"
trump.model = naiveBayes(rep~., data = trump.train)
trump.pred = predict(trump.model, newdata = trump[-train, ], type = "raw")

# Cruz vs. Rest
cruz.train = traindata
cruz.train$rep[(cruz.train$rep != "Cruz")] = "other"
cruz.model = naiveBayes(rep~., data = cruz.train)
cruz.pred = predict(cruz.model, newdata = dtm.class.data[-train, ], type = "class")

# Kasich vs. Rest
kasich.train = traindata
kasich.train$rep[(kasich.train$rep != "Kasich")] = "other"
kasich.model = naiveBayes(rep~., data = kasich.train)
kasich.pred = predict(kasich.model, newdata = dtm.class.data[-train, ], type = "class")

# Votes
trump.votes = (trump.pred == "Trump") + (cruz.pred == "other") + (kasich.pred == "other")
cruz.votes = (trump.pred == "other") + (cruz.pred == "Cruz") + (kasich.pred == "other")
kasich.votes = (trump.pred == "other") + (cruz.pred == "other") + (kasich.pred == "Kasich")

votes2 = cbind(trump.votes, cruz.votes, kasich.votes)
one.v.rest.pred = c("Trump", "Cruz", "Kasich")[apply(votes2, 1, which.max)]

confmatrix(dtm.class.data$rep[-train], one.v.rest.pred)  ## 0.7196812 accuracy



# Neural Network

#createfolds function
createfolds=function(n,K){
  reps=ceiling(n/K)
  folds=sample(rep(1:K,reps))
  return(folds[1:n])
}
folds = createfolds(nrow(dtm.class.data), 10)
accmatrix = matrix(nrow = 10, ncol = 10)

for(netsize in 1:10){
  for(k in 1:10){
    temptest = dtm.class.data[folds == k, ]
    temptrain = dtm.class.data[folds != k, ]
    
    tempnnet = nnet(rep~., data = temptrain, size = netsize)
    accmatrix[netsize, k] = confmatrix(temptest$rep, predict(tempnnet, newdata = temptest, 
                                                             type = "class"))$accuracy
  }
}

accvector = apply(accmatrix, 1, mean)
plot(1:10, accvector)
which.max(accvector)


nnet.model = nnet(rep~., data = dtm.class.data[train, ], size = 1)
pred.rep = predict(nnet.model, newdata = dtm.class.data[-train, ], type = "class")

confmatrix(dtm.class.data$rep[-train], pred.rep)  ### 0.8853084
plot(nnet.model)



x = is.na(dtm.class.data$rep)
# Random Forest with only DTM
rf.model = randomForest(rep~., data = dtm.class.data[train, ])


#GOAL:
#   build a data frame with columns(Candidate, #tweets with term, and this score) for each county.
#   use this as the data matrix for models.



# Use to calculate term frequency of data sets
# Calculate inverse document frequency
inv.doc.freq = log(3/rowSums(sign(word.freq.urgent.all[, 2:4])))
inv.doc.freq[is.infinite(inv.doc.freq)] = 0 

tf.idf = word.freq.urgent.all[, 2:4] * inv.doc.freq
colnames()

# Random Forest Model 
# Classification Accuracy
# ROC Curve