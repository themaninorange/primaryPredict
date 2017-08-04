dtm.class.data.dem = data.frame(dem = tw.data$dem, b.o.w)


# Naive Bayes

nb.dem.mod = naiveBayes(dem~., data = dtm.class.data.dem[train, ])
nb.dem.pred = predict(nb.dem.mod, newdata = dtm.class.data.dem[-train, ], type = "class")

confmatrix(dtm.class.data.dem$dem[-train], nb.dem.pred) # 0.4965555

phat = predict(nb.dem.mod, newdata = dtm.class.data.dem[-train, ], type = "raw")
plot(roc(dtm.class.data$dem[-train], phat[ ,2]))


folds = createfolds(nrow(dtm.class.data.dem), 10)
accmatrix = matrix(nrow = 10, ncol = 10)

for(netsize in 1:10){
  for(k in 1:10){
    temptest = dtm.class.data.dem[folds == k, ]
    temptrain = dtm.class.data.dem[folds != k, ]
    
    tempnnet = nnet(dem~., data = temptrain, size = netsize)
    accmatrix[netsize, k] = confmatrix(temptest$dem, predict(tempnnet, newdata = temptest, 
                                                             type = "class"))$accuracy
  }
}

accvector = apply(accmatrix, 1, mean)
plot(1:10, accvector)
which.max(accvector)


nnet.model = nnet(dem~., data = dtm.class.data.dem[train, ], size = 7)
pred.dem = predict(nnet.model, newdata = dtm.class.data.dem[-train, ], type = "class")

confmatrix(dtm.class.data.dem$dem[-train], pred.dem)  ### 0.7886288
plot(nnet.model)


# SVM ~~~~~
svm.mod = svm(dem~., data = dtm.class.data.dem[train, ], kernel = "linear", cost = 1000)
svm.pred = predict(svm.mod, newdata = dtm.class.data.dem[-train, ], type = "class")
confmatrix(dtm.class.data.dem$dem[-train], svm.pred) # 0.7752508


