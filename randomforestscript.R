##########################
# For use on Republicans #
##########################

install.packages("tm", dependencies=TRUE)
install.packages("dclone", dependencies=TRUE)

library(tm)
library(stringr)
library(pROC)
library(randomForest)
library(dclone)

load("../../Dropbox/Visualization/tw.data.Rda")
load("../../Dropbox/Visualization/newstates.Rda")

statenames = strsplit("Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming",split = ", ")

nalesstw = tw.data[!is.na(tw.data$rep),]
corpus=Corpus(VectorSource(c(nalesstw$text.clean,newstates.df$text.clean)))
tfm = DocumentTermMatrix(corpus, control = list(stopwords = stopwords('english'),
                                                  removeNumbers = T))
tfm = removeSparseTerms(tfm, .999)
tfdf = as.data.frame(as.matrix(tfm))

###
names(tfdf)[names(tfdf)=='else']='Else'
names(tfdf)[names(tfdf)=='next']='Next'
termzeroes = unname(which(colSums(tfdf[1:nrow(nalesstw),])!=0))
tfdf = tfdf[,termzeroes]
###

#Takes some relevant tweet information and binds those columns to DTF matrix.
rtrain = cbind(Republican.Candidate = nalesstw$rep, Time.Stamp = nalesstw$created,
               Retweet.Count = nalesstw$retweetCount, Is.Retweet = nalesstw$isRetweet,
               Retweeted = nalesstw$retweeted, Keyword = nalesstw$keyword,
               tfdf[1:length(nalesstw$rep),])    # <- this only keeps the part that went with nalesstw.
rtest  = cbind(Republican.Candidate = newstates.df$rep, Time.Stamp = newstates.df$created,
               Retweet.Count = newstates.df$retweetCount, Is.Retweet = newstates.df$isRetweet,
               Retweeted = newstates.df$retweeted, Keyword = newstates.df$keyword,
               tfdf[-(1:length(nalesstw$rep)),]) # <- this only keeps the negation of nalesstw rows.


#Changing class labels to "Trump" and "Not Trump"
r2train = dclone(rtrain)
levels(r2train$Republican.Candidate) = c("Trump", "Not Trump", "Cruz", "Kasich")
r2train$Republican.Candidate[r2train$Republican.Candidate!="Trump"]="Not Trump"
r2train$Republican.Candidate = droplevels(r2train$Republican.Candidate)

r2test = dclone(rtest)
levels(r2test$Republican.Candidate) = c("Trump", "Not Trump", "Cruz", "Kasich")
r2test$Republican.Candidate[r2test$Republican.Candidate!="Trump"]="Not Trump"
r2test$Republican.Candidate = droplevels(r2test$Republican.Candidate)

#Models!
r2rf = randomForest(formula = Republican.Candidate~.,data = r2train[,!(names(r2train) == "Keyword")], ntree = 10)
x = ( 1:100 )/100
y = 1:100
for(i in 1:100){
  r2predrf = predict(r2rf,newdata = r2test[,!(names(r2test) == "Keyword")], 'prob')[,2]< i/100
  y[i] = confmatrix(r2test$Republican.Candidate=="Trump",r2predrf)$accuracy
  print(i)
}
png(filename = "../../Dropbox/Visualization/Beamer Presentation/RFmodelaccuracy250.png")
plot(x,y, xlab = "p-threshold for Classification", ylab = "Classification Accuracy", main = "250 Tree Model", ylim = c(0,1))
abline(coef = c(mean(r2train$Republican.Candidate=="Trump"), 0), col = rgb(0.7,0,0))
abline(coef = c(mean(r2test$Republican.Candidate=="Trump"), 0), col = rgb(0,0,0.7))
dev.off()

goodpred = predict(r2rf,newdata = r2test[,!(names(r2test) == "Keyword")], 'prob')[,2]< which.max(y)/100
goodconf = confmatrix(r2test$Republican.Candidate=="Trump", goodpred)


###png(filename = "../../Dropbox/Visualization/Beamer Presentation/RFmodelaccuracy.png")
###plot(x,y, xlab = "p-threshold for Classification", ylab = "Classification Accuracy")
###abline(coef = c(alltrump, 0), col = rgb(0.7,0,0))
###dev.off()

alltrump = sum(r2test$Republican.Candidate=="Trump")/length(r2test$Republican.Candidate)

###

#number of tweets from each county
fipscount1 = as.data.frame(cbind(
  fips = as.numeric(as.character(names(table(nalesstw$fips)))), 
  Freq = as.numeric(table(nalesstw$fips))))
fipscount2 = as.data.frame(cbind(
  fips = as.numeric(as.character(names(table(newstates.df$fips)))), 
  Freq = as.numeric(table(newstates.df$fips))))

#clones of training and testing data
trainbalance = dclone(r2train)
testbalance = dclone(r2test)
trainbalance = cbind(fips = c(nalesstw$fips), trainbalance)
testbalance = cbind(fips = c(newstates.df$fips), testbalance)

#for each fips code, take the corresponding terms and divide by the number of tweets from that county.
for(i in c(fipscount1$fips,fipscount2$fips)){
  trainbalance[trainbalance$fips==i,8:ncol(trainbalance)] = 
    trainbalance[trainbalance$fips==i,8:ncol(trainbalance)]/fipscount1$Freq[fipscount1$fips==i]
  testbalance[testbalance$fips==i,8:ncol(testbalance)] = 
    testbalance[testbalance$fips==i,8:ncol(testbalance)]/fipscount2$Freq[fipscount2$fips==i]
}

levels(trainbalance$Keyword) = unique(c(levels(trainbalance$Keyword), levels(testbalance$Keyword)))
levels(testbalance$Keyword) = unique(c(levels(trainbalance$Keyword), levels(testbalance$Keyword)))

r3rf = randomForest(formula = Republican.Candidate~. -fips,data = trainbalance, ntree = 10)
r3predrf = predict(r3rf,newdata = testbalance)
confmatrix(testbalance$Republican.Candidate,r3predrf)

primarycutoff = 1458662400


#######################
#Filter tweets by date#
#######################

priortw = newstates.df[newstates.df$created < primarycutoff,]

priorcorpus=Corpus(VectorSource(c(nalesstw$text.clean,priortw$text.clean)))
priortf = DocumentTermMatrix(priorcorpus, control = list(stopwords = stopwords('english'),
                                                  removeNumbers = T))
priortf = removeSparseTerms(priortf, .999)
priortf = as.data.frame(as.matrix(priortf))
names(priortf)[names(priortf)=='else']='Else'
names(priortf)[names(priortf)=='next']='Next'

rtrainprior = cbind(Republican.Candidate = c(nalesstw$rep), priortf[1:length(nalesstw$rep),])
rtestprior = cbind(Republican.Candidate = c(priortw$rep), priortf[-(1:length(nalesstw$rep)),])

##
priorzeroes = unname(which(colSums(rtrainprior)!=0))-1
##

priortf = priortf[,priorzeroes]
rtrainprior = rtrainprior[,priorzeroes]
rtestprior = rtestprior[,priorzeroes]

##
fipscount1prior = as.data.frame(cbind(
  fips = as.numeric(as.character(names(table(nalesstw$fips)))), 
  Freq = as.numeric(table(nalesstw$fips))))
fipscount2prior = as.data.frame(cbind(
  fips = as.numeric(as.character(names(table(priortw$fips)))), 
  Freq = as.numeric(table(priortw$fips))))
##

rtrainprior = cbind(fips = c(nalesstw$fips), rtrainprior)
rtestprior = cbind(fips = c(priortw$fips), rtestprior)


for(i in c(fipscount1prior$fips,fipscount2prior$fips)){
  rtrainprior[rtrainprior$fips==i,3:ncol(rtrainprior)] = 
    rtrainprior[rtrainprior$fips==i,3:ncol(rtrainprior)]/fipscount1prior$Freq[fipscount1prior$fips==i]
  rtestprior[rtestprior$fips==i,3:ncol(rtestprior)] = 
    rtestprior[rtestprior$fips==i,3:ncol(rtestprior)]/fipscount2prior$Freq[fipscount2prior$fips==i]
}


#########
#2-class#
#########
r2trainprior = dclone(rtrainprior)
r2trainprior$Republican.Candidate = -1*(rtrainprior$Republican.Candidate==1)+2

r2testprior = dclone(rtestprior)
r2testprior$Republican.Candidate = -1*(r2testprior$Republican.Candidate==1)+2

r2rfprior = randomForest(formula = Republican.Candidate~. - fips,data = r2trainprior, ntree = 10)
r2priorpredrf = predict(r2rfprior,newdata = r2testprior)>1.5
confmatrix(r2testprior$Republican.Candidate,r2priorpredrf)
alltrumpprior = sum(r2testprior$Republican.Candidate==1)/length(r2testprior$Republican.Candidate)

#########
#3-class#
#########

  #Copy of only tweets prior to the primary.
r3trainprior = dclone(rtrainprior)

  #I guess it wasn't a factor, before?
r3trainprior$Republican.Candidate =  as.factor(rtrainprior$Republican.Candidate)

  #Ditto with testing set.
r3testprior = dclone(rtestprior)
r3testprior$Republican.Candidate = as.factor(rtestprior$Republican.Candidate)

  
r3rfprior = randomForest(formula = Republican.Candidate~.,data = r3trainprior, ntree = 10)
r3priorpredrf = predict(r3rfprior,newdata = r3testprior)
confmatrix(r3testprior$Republican.Candidate,r3priorpredrf)
  #This accuracy seems very high.  Let's check on that later.

########################
# For Use on Democrats #
########################

priortw = newstates.df[newstates.df$created < primarycutoff,]

priorcorpus=Corpus(VectorSource(c(nalesstw$text.clean,priortw$text.clean)))
priortf = DocumentTermMatrix(priorcorpus, control = list(stopwords = stopwords('english'),
                                                         removeNumbers = T))
priortf = removeSparseTerms(priortf, .999)
priortf = as.data.frame(as.matrix(priortf))
names(priortf)[names(priortf)=='else']='Else'
names(priortf)[names(priortf)=='next']='Next'

dtrainprior = cbind(Democratic.Candidate = c(nalesstw$dem), priortf[1:length(nalesstw$dem),])
dtestprior = cbind(Democratic.Candidate = c(priortw$dem), priortf[-(1:length(nalesstw$dem)),])

##
priorzeroes = unname(which(colSums(dtrainprior)!=0))-1
##

dtrainprior$Democratic.Candidate = as.factor(dtrainprior$Democratic.Candidate)
dtestprior$Democratic.Candidate = as.factor(dtestprior$Democratic.Candidate)

priortf = priortf[,priorzeroes]
dtrainprior = dtrainprior[,priorzeroes]
dtestprior = dtestprior[,priorzeroes]

##
fipscount1prior = as.data.frame(cbind(
  fips = as.numeric(as.character(names(table(nalesstw$fips)))), 
  Freq = as.numeric(table(nalesstw$fips))))
fipscount2prior = as.data.frame(cbind(
  fips = as.numeric(as.character(names(table(priortw$fips)))), 
  Freq = as.numeric(table(priortw$fips))))
##

dtrainprior = cbind(fips = c(nalesstw$fips), dtrainprior)
dtestprior = cbind(fips = c(priortw$fips), dtestprior)


for(i in c(fipscount1prior$fips,fipscount2prior$fips)){
  dtrainprior[dtrainprior$fips==i,3:ncol(dtrainprior)] = 
    dtrainprior[dtrainprior$fips==i,3:ncol(dtrainprior)]/fipscount1prior$Freq[fipscount1prior$fips==i]
  dtestprior[dtestprior$fips==i,3:ncol(dtestprior)] = 
    dtestprior[dtestprior$fips==i,3:ncol(dtestprior)]/fipscount2prior$Freq[fipscount2prior$fips==i]
}

drfprior = randomForest(formula = Democratic.Candidate~.,data = dtrainprior, ntree = 5)
dpriorpredrf = predict(drfprior,newdata = dtestprior)
confmatrix(dtestprior$Democratic.Candidate,dpriorpredrf)
confmatrix(dtrainprior$Democratic.Candidate,predict(drfprior,newdata = dtrainprior))


########################################
# Make predictions at the county level #
########################################

#The following block creates the data frame and stitches all of the tweets together.
countydf = nalesstw[FALSE,][,c('fips', 'text.clean','dem','rep')]
uniqfips = unique(nalesstw$fips)
for(i in 1:length(uniqfips)){
  countydf[i,] = c(0, "", 0, 0)
  temptw = nalesstw[nalesstw$fips==uniqfips[i],]
  countydf$fips[i] = uniqfips[i]
  countydf$text.clean[i] = paste(temptw$text.clean, collapse = " ")
  countydf$dem[i] = temptw$dem[1]
  countydf$rep[i] = temptw$rep[1]
}

countydf2 = newstates.df[FALSE,][,c('fips', 'text.clean','dem','rep')]
uniqfips2 = unique(newstates.df$fips)
for(i in 1:length(uniqfips2)){
  countydf2[i,] = c(0, "", 0, 0)
  temptw = newstates.df[newstates.df$fips==uniqfips2[i],]
  countydf2$fips[i] = uniqfips2[i]
  countydf2$text.clean[i] = paste(temptw$text.clean, collapse = " ")
  countydf2$dem[i] = temptw$dem[1]
  countydf2$rep[i] = temptw$rep[1]
}

corpus=Corpus(VectorSource(c(countydf$text.clean,countydf2$text.clean)))
tfm = DocumentTermMatrix(corpus, control = list(stopwords = stopwords('english'),
                                                removeNumbers = T))
tfm = removeSparseTerms(tfm, .999)
tfdf = as.data.frame(as.matrix(tfm))

###
dotify = function(x){
  return(paste(c('.', x), collapse = ''))
}
names(tfdf) = sapply(tolower(names(tfdf)), FUN = dotify)

termzeroes = unname(which(colSums(tfdf[1:nrow(countydf),])!=0))
tfdf = tfdf[,termzeroes]
###



#Takes some relevant tweet information and binds those columns to DTF matrix.
rcountytrain = cbind(Republican.Candidate = countydf$rep, 
                     tfdf[1:length(countydf$rep),])
rcountytest  = cbind(Republican.Candidate = countydf2$rep,
                     tfdf[-(1:length(countydf$rep)),]) # <- this only keeps the negation of nalesstw rows.

#Changing class labels to "Trump" and "Not Trump"
r2train = dclone(rcountytrain)
levels(r2train$Republican.Candidate) = c("Trump", "Not Trump", "Cruz", "Kasich")
r2train$Republican.Candidate[r2train$Republican.Candidate!="Trump"]="Not Trump"
r2train$Republican.Candidate = droplevels(r2train$Republican.Candidate)

r2test = dclone(rcountytest)
levels(r2test$Republican.Candidate) = c("Trump", "Not Trump", "Cruz", "Kasich")
r2test$Republican.Candidate[r2test$Republican.Candidate!="Trump"]="Not Trump"
r2test$Republican.Candidate = droplevels(r2test$Republican.Candidate)

#Models!
r2rf = randomForest(formula = Republican.Candidate~.,data = r2train, ntree = 100)
predr2rf = predict(r2rf, newdata = r2test)
confmatrix(r2test$Republican.Candidate, predr2rf)
