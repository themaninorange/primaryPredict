load('~/Dropbox/DataMining/Scraping/Visualization/newstatesdata.Rda')
newstates.df = AZ_ID_UTtweets
newstates.df$text.clean = twat.clean(AZ_ID_UTtweets)

newstates.df = merge(newstates.df, newresults, by.y = "fips")


# Democrats stuff now THX

bownew = DocumentTermMatrix(Corpus(VectorSource(c(newstates.df$text.clean, tw.data$text.clean))))
bownew = removeSparseTerms(bownew, 0.99)
bownew = as.matrix(bownew)

dtm.new.dem = data.frame(dem = c(newstates.df$dem, tw.data$dem), bownew)
dtm.new = data.frame(dem = c(newstates.df$dem, tw.data$dem), rep = c(newstates.df$rep, tw.data$rep), bownew)
trainind = trainsample(dtm.new.dem, 0.7)

# Naive Bayes
nb.new.mod = naiveBayes(dem~., data = dtm.new.dem[trainind, ])
nb.new.pred = predict(nb.new.mod, newdata = dtm.new.dem[-trainind, ], type = "class")
confmatrix(dtm.new.dem$dem[-trainind], nb.new.pred)
phat = predict(nb.dem.mod, newdata = dtm.new.dem[-trainind, ], type = "raw")
plot(roc(dtm.new.dem$dem[-trainind], phat[ ,2]))


# Neural Network
folds = createfolds(nrow(dtm.new.dem), 10)
accmatrix = matrix(nrow = 19, ncol = 10)

for(netsize in 1:19){
  for(k in 1:10){
    temptest = dtm.class.data.dem[folds == k, ]
    temptrain = dtm.class.data.dem[folds != k, ]
    
    tempnnet = nnet(dem~., data = temptrain, size = netsize)
    accmatrix[netsize, k] = confmatrix(temptest$dem, predict(tempnnet, newdata = temptest, 
                                                             type = "class"))$accuracy
  }
}
accvector = apply(accmatrix, 1, mean)
plot(1:19, accvector)
which.max(accvector)

accvector[10]



nnet.new.mod = nnet(dem~., data = dtm.new.dem[train, ], size = 10)
pred.nnet.new = predict(nnet.new.mod, newdata = dtm.new.dem[-train, ], type = "class")
confmatrix(dtm.new.dem$dem[-train], pred.nnet.new)

phat = predict(nnet.new.mod, newdata = dtm.new.dem[-train, ], type = "raw")

plot(roc(dtm.new.dem$dem[-train], phat[ ,1]))

# SVM
svm.new.mod = svm(dem~., data = dtm.new.dem[trainind, ], kernel = "linear", cost = 1000)
svm.new.pred = predict(svm.new.mod, newdata = dtm.new.dem[-train, ], type = "class")
confmatrix(dtm.new.dem$dem[-train], svm.new.pred)

phat = predict(svm.new.mod, newdata = dtm.new.dem[-train, ], type = "raw")


