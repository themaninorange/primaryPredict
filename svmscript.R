##########################
# For use on Republicans #
##########################

library(tm)
library(stringr)
library(pROC)
library(e1071)
library(dclone)

load("../../Dropbox/Visualization/tw.data.Rda")
load("../../Dropbox/Visualization/newstates.Rda")

nalesstw = tw.data[!is.na(tw.data$rep),]
corpus=Corpus(VectorSource(c(nalesstw$text.clean,newstates.df$text.clean)))
tfsvm = DocumentTermMatrix(corpus, control = list(stopwords = stopwords('english'),
                                               removeNumbers = T))
tfsvm = removeSparseTerms(tfsvm, .999)
tfsvmdf = as.data.frame(as.matrix(tfsvm))

rtrainsvm = cbind(Republican.Candidate = c(nalesstw$rep), tfsvmdf[1:length(nalesstw$rep),])
rtestsvm = cbind(Republican.Candidate = c(newstates.df$rep), tfsvmdf[-(1:length(nalesstw$rep)),])

#Function for calling svm one against one.
one.against.one.svm = function(variable.name, traindata, testdata,
                               ...){
  x = dclone(traindata)
  y = testdata
  target = which.min(is.na(match(names(x),variable.name)))
  classes = names(table(x[target]))
  votes = matrix(0,dim(y)[1],length(classes))
  for(i in 1:(length(classes)-1)){
    for(j in (i+1):length(classes)){
      tempx = dclone(x[x[target]==
                         classes[i]|x[target]==classes[j],])
      tempmodel = svm(as.formula(paste(variable.name,"~.")),data=
                        tempx)
      temppred = predict(tempmodel, newdata=testdata, type="class")
      votes[,i] = votes[,i] + (temppred == classes[i])
      votes[,j] = votes[,j] + (temppred == classes[j])
    }
  }
  which.max_break.ties = function(x){
    if(min(x) == max(x)){
      return(sample(length(x),1))
    }
    return(which.max(x))
  }
  which.max_no.ties = function(x){
    if(min(x) == max(x)){
      return(NA)
    }
    return(which.max(x))
  }
  tally=factor(classes[na.omit(apply(votes,1,which.max_no.ties))],
               levels(y[[target]]))
  which.max_break.ties2 = function(x){
    if(min(x) == max(x)){
      return(sample(length(x),1, prob = as.numeric(table(tally))/
                      sum(table(tally))))
    }
    return(which.max(x))
  }
  if(TRUE %in% list(...)){
    predictions = classes[apply(votes,1,which.max_break.ties2)]
  } else {
    predictions = classes[apply(votes,1,which.max_break.ties)]
  }
  if(is.factor(x[[target]])){
    predictions = as.factor(predictions)
    predictions = factor(predictions,levels(y[[target]]))
  }
  return(predictions)
}

predictions = one.against.one.svm(variable.name = "Republican.Candidate",traindata = rtrainsvm,testdata = rtestsvm)
confmatrix(rtestsvm$Republican.Candidate,predictions)

r2trainsvm = dclone(rtrainsvm)
r2trainsvm$Republican.Candidate = -1*(r2trainsvm$Republican.Candidate==1)+2

r2testsvm = dclone(rtestsvm)
r2testsvm$Republican.Candidate = -1*(r2testsvm$Republican.Candidate==1)+2

r2svm = svm(formula = Republican.Candidate~. , x = r2trainsvm,cost = 1)
r2predsvm = predict(r2svm,newdata = r2testsvm)
confmatrix(r2testsvm$Republican.Candidate,r2predsvm)
sum(r2testsvm$Republican.Candidate==1)/length(r2testsvm$Republican.Candidate)

trumpystuff = (1:length(r2trainsvm$Republican.Candidate))[r2trainsvm$Republican.Candidate==1]
otherstuff =  (1:length(r2trainsvm$Republican.Candidate))[r2trainsvm$Republican.Candidate!=1]
r2boottrain = r2trainsvm[c(sample(trumpystuff,size = 50000,replace = TRUE),sample(otherstuff,size = 50000,replace = TRUE)),]

r2bootsvm = svm(formula = Republican.Candidate~. , x = r2boottrain)
r2bootpredsvm = predict(r2bootsvm,newdata = r2testsvm)
confmatrix(r2testsvm$Republican.Candidate,r2bootpredsvm)
sum(r2testsvm$Republican.Candidate==1)/length(r2testsvm$Republican.Candidate)