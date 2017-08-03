##########################
# For use on Republicans #
##########################

#install.packages("tm", dependencies=TRUE)
#install.packages("dclone", dependencies=TRUE)
#install.packages("randomForest", dependencies=TRUE)

library(tm)
library(stringr)
library(pROC)
library(randomForest)
library(dclone)

load("../../Dropbox/Visualization/tw.data.Rda")
load("../../Dropbox/Visualization/newstates.Rda")

statenames = strsplit("Alabama, Alaska, Arizona, Arkansas, California, Colorado, Connecticut, Delaware, District of Columbia, Florida, Georgia, Hawaii, Idaho, Illinois, Indiana, Iowa, Kansas, Kentucky, Louisiana, Maine, Maryland, Massachusetts, Michigan, Minnesota, Mississippi, Missouri, Montana, Nebraska, Nevada, New Hampshire, New Jersey, New Mexico, New York, North Carolina, North Dakota, Ohio, Oklahoma, Oregon, Pennsylvania, Rhode Island, South Carolina, South Dakota, Tennessee, Texas, Utah, Vermont, Virginia, Washington, West Virginia, Wisconsin, Wyoming",split = ", ")

#tw.data is a dataframe of cleaned tweets from before the date we're looking at.
#Here, we are removing any rows where there is missing republican data, creating a corpus,
#   creating a document term frequency matrix, and renaming columns that have are keywords
#   in the R scripting language.
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

#We also want to predict on the tweet metadata.
#Here, we take some relevant tweet information and binds those columns to term frequency matrix.
#We also separate into training and testing data.  The training set is made of tweets from 
#    before the date of primaries, and the testing set is made of tweets afterward.
rtrain = cbind(Republican.Candidate = nalesstw$rep, Time.Stamp = nalesstw$created,
               Retweet.Count = nalesstw$retweetCount, Is.Retweet = nalesstw$isRetweet,
               Retweeted = nalesstw$retweeted, Keyword = nalesstw$keyword,
               tfdf[1:length(nalesstw$rep),])    # <- this only keeps the part that went with nalesstw.
rtest  = cbind(Republican.Candidate = newstates.df$rep, Time.Stamp = newstates.df$created,
               Retweet.Count = newstates.df$retweetCount, Is.Retweet = newstates.df$isRetweet,
               Retweeted = newstates.df$retweeted, Keyword = newstates.df$keyword,
               tfdf[-(1:length(nalesstw$rep)),]) # <- this only keeps the negation of nalesstw rows.


#Changing class labels to "Trump" and "Not Trump", because Cruz and Kasich are small.
r2train = dclone(rtrain)
levels(r2train$Republican.Candidate) = c("Trump", "Not Trump", "Cruz", "Kasich")
r2train$Republican.Candidate[r2train$Republican.Candidate!="Trump"]="Not Trump"
r2train$Republican.Candidate = droplevels(r2train$Republican.Candidate)

r2test = dclone(rtest)
levels(r2test$Republican.Candidate) = c("Trump", "Not Trump", "Cruz", "Kasich")
r2test$Republican.Candidate[r2test$Republican.Candidate!="Trump"]="Not Trump"
r2test$Republican.Candidate = droplevels(r2test$Republican.Candidate)

#Let's run a model!  Predict on the training data.  (~1 minute on 10 trees)
#    (Currently without the term "Keyword" because it's not working and nontrivial to fix.)
r2rf = randomForest(formula = Republican.Candidate~.,data = r2train[,!(names(r2train) == "Keyword")], ntree = 10)

#Let's make predictions on our test data!
r2predrf = predict(r2rf,newdata = r2test[,!(names(r2test) == "Keyword")], 'prob')[,2]< 0.5
confmatrix(r2test$Republican.Candidate=="Trump",r2predrf)

#That seems like an acceptable result.  What was the distribution for votes, though?
sum(r2test$Republican.Candidate == "Trump")/dim(r2test)[1]

#Dang.  We were better off guessing Trump for everything.  We should look at how sure we
#    would have to have been to make that call before beating average.
x = ( 1:100 )/100
y = 1:100
for(i in 1:100){
  r2predrf = predict(r2rf,newdata = r2test[,!(names(r2test) == "Keyword")], 'prob')[,2]< i/100
  y[i] = confmatrix(r2test$Republican.Candidate=="Trump",r2predrf)$accuracy
  print(i)
}

png(filename = "/home/joseph/Dev/Data Mining 2 Final/primaryPredict/Beamer Presentation/RFmodelaccuracy250.png")
plot(x,y, xlab = "p-threshold for Classification", ylab = "Classification Accuracy", main = "250 Tree Model", ylim = c(0,1))

abline(coef = c(mean(r2train$Republican.Candidate=="Trump"), 0), col = rgb(0.7,0,0))
abline(coef = c(mean(r2test$Republican.Candidate=="Trump"), 0), col = rgb(0,0,0.7))
dev.off()

#This is not a good situation.  It does not seem that these variables can be predictive of a Trump victory.
#Let's try a model where we divide values by the number of tweets from a county
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

#And that looks like a success!  We have a very very narrowly higher accuracy than guessing Trump
#   for every county.


#######################
#Filter tweets by date#
#######################

#Time stamp at which we cut off tweet input for training data.
primarycutoff = 1458662400

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

r2trainprior = dclone(rtrainprior)
r2trainprior$Republican.Candidate = -1*(rtrainprior$Republican.Candidate==1)+2

r2testprior = dclone(rtestprior)
r2testprior$Republican.Candidate = -1*(r2testprior$Republican.Candidate==1)+2

r2rfprior = randomForest(formula = Republican.Candidate~. - fips,data = r2trainprior, ntree = 10)
r2priorpredrf = predict(r2rfprior,newdata = r2testprior)>1.5
confmatrix(r2testprior$Republican.Candidate,r2priorpredrf)
alltrumpprior = sum(r2testprior$Republican.Candidate==1)/length(r2testprior$Republican.Candidate)

#Another model more accurate than the ALLTRUMP model.
#Let's try doing this at the county level instead of the tweet level.

#########################################
# Combine tweet stats into county stats #
#########################################

#Time stamp at which we cut off tweet input for training data.
primarycutoff = 1458662400

priortw = newstates.df[newstates.df$created < primarycutoff,]

priorcorpus=Corpus(VectorSource(c(nalesstw$text.clean,priortw$text.clean)))
priortf = DocumentTermMatrix(priorcorpus, control = list(stopwords = stopwords('english'),
                                                         removeNumbers = T))
priortf = removeSparseTerms(priortf, .999)
r3priortf = as.data.frame(as.matrix(priortf))

#           Combine by    fips code
#           |             |
#           |             |     Term frequencies            ->                                                    Sum and divide by total.
#           |             |     |                                                                                 |
r3priortf = aggregate(. ~ fips, cbind(as.data.frame(as.matrix(priortf)), fips = c( nalesstw$fips, priortw$fips)), mean)

names(r3priortf)[names(r3priortf)=='else']='Else'
names(r3priortf)[names(r3priortf)=='next']='Next'


#              Stitch together
#              |
#              |     The word frequencies
#              |     |          
#              |     |          Winners and fips codes
#              |     |          |
r3trainprior = merge(r3priortf, aggregate(rep~fips, nalesstw, FUN=head, 1), by="fips")
#              |     |          |
r3testprior =  merge(r3priortf, aggregate(rep~fips, priortw,  FUN=head, 1), by="fips")

names(r3trainprior)[names(r3trainprior) == 'rep.x'] = 'Republican.Candidate'
names(r3testprior )[names(r3testprior ) == 'rep.x'] = 'Republican.Candidate'

r3trainprior$Republican.Candidate = replace(r3trainprior$Republican.Candidate, r3trainprior$Republican.Candidate==0, "Trump")
r3trainprior$Republican.Candidate = replace(r3trainprior$Republican.Candidate, r3trainprior$Republican.Candidate !="Trump", "Not Trump")

r3testprior$Republican.Candidate = replace(r3testprior$Republican.Candidate, r3testprior$Republican.Candidate==0, "Trump")
r3testprior$Republican.Candidate = replace(r3testprior$Republican.Candidate, r3testprior$Republican.Candidate !="Trump", "Not Trump")

##
numericcols = which(sapply(r3trainprior, typeof) == "double")
priornonzeroes = unname(which(colSums(r3trainprior[numericcols])!=0))-1
##

r3priortf = r3priortf[,priornonzeroes]
r3trainprior = r3trainprior[,priornonzeroes]
r3testprior = r3testprior[,priornonzeroes]

r3trainprior$Republican.Candidate = as.factor(r3trainprior$Republican.Candidate)
r3testprior$Republican.Candidate  = as.factor(r3testprior$Republican.Candidate )

r3rfprior = randomForest(formula = Republican.Candidate~. - fips, data = r3trainprior, ntree = 10)
r3priorpredrf = predict(r3rfprior,newdata = r3testprior)
confmatrix(r3testprior$Republican.Candidate,r3priorpredrf)

#Hey!  A model which actually predicts ALLTRUMP!
#That's new!
#I want to see my training accuracy...

r3priorpredrf_train = predict(r3rfprior,newdata = r3trainprior)
confmatrix(r3trainprior$Republican.Candidate,r3priorpredrf_train)

#huh.  Trump just wins everything.  Let's do some democrats... 
#    they should be more interesting.

########################
# For Use on Democrats #
########################
#########################################
# Combine tweet stats into county stats #
#########################################

#Let's just use all of the overhead stuff from before.
#HILLARY IS 2!!!@!#$%#@$#!@$#
d3priortf = as.data.frame(as.matrix(priortf))

#           Combine by    fips code
#           |             |
#           |             |     Term frequencies            ->                                                    Sum and divide by total.
#           |             |     |                                                                                 |
d3priortf = aggregate(. ~ fips, cbind(as.data.frame(as.matrix(priortf)), fips = c( nalesstw$fips, priortw$fips)), mean)

names(d3priortf)[names(d3priortf)=='else']='Else'
names(d3priortf)[names(d3priortf)=='next']='Next'

#              Stitch together
#              |
#              |     The word frequencies
#              |     |          
#              |     |          Winners and fips codes
#              |     |          |
d3trainprior = merge(d3priortf, aggregate(dem~fips, nalesstw, FUN=head, 1), by="fips")
#              |     |          |
d3testprior =  merge(d3priortf, aggregate(dem~fips, priortw,  FUN=head, 1), by="fips")

names(d3trainprior)[names(d3trainprior) == 'dem'] = 'Democratic.Candidate'
names(d3testprior )[names(d3testprior ) == 'dem'] = 'Democratic.Candidate'

d3trainprior$Democratic.Candidate = replace(d3trainprior$Democratic.Candidate, d3trainprior$Democratic.Candidate==2, "Hillary")
d3trainprior$Democratic.Candidate = replace(d3trainprior$Democratic.Candidate, d3trainprior$Democratic.Candidate != "Hillary", "Bernie")

##
numericcols = which(sapply(d3trainprior, typeof) == "double")
priorzeroes = unname(which(colSums(d3trainprior[numericcols])==0))
##

d3priortf = d3priortf[,-priorzeroes]
d3trainprior = d3trainprior[,-priorzeroes]
d3testprior = d3testprior[,-priorzeroes]

d3testprior$Democratic.Candidate   = as.factor(d3testprior$Democratic.Candidate )
d3trainprior$Democratic.Candidate  = as.factor(d3trainprior$Democratic.Candidate)

d3rfprior = randomForest(formula = Democratic.Candidate~. - fips, data = d3trainprior, ntree = 10)
d3priorpredrf = predict(d3rfprior,newdata = d3testprior)
confmatrix(d3testprior$Democratic.Candidate,d3priorpredrf)

#Wow, that actually looks pretty good.
#What does the average look like?

sum(d3testprior$Democratic.Candidate == "Bernie")/length(d3testprior$Democratic.Candidate)

#We're winning by 2.4% !!!!!!

##########
# NGRAMS #
##########

####
#START HERE NEXT
####

get_ngrams = function(document, n=2){
  
  temp = strsplit(document, "\\s+")[[1]]
  ngrams = list()
  for(i in 1:(length(temp) - n)){
    ngram = c()
    for(j in 1:n){
      ngram = c(ngram, temp[i + j])
    }
    ngrams[[i]] = ngram
  }
  
  return(ngrams)
}

strumkis = list(lapply(list(1,2,3,4,53,2,4,2,1,3,4,2,4,422,3,3,4,3,4,3,3,3,4,3), as.String),
                lapply(list(2,3,4,6,4,3,45,56,6,7,5,45,5,7,4,3,2,3,4,5,6,4,3,4,5), as.String))

table(unlist(lapply(strumkis, unlist)))

aggregate_ngram_count = function(data, aggregate_column = "fips", text_column = "text", n = 2){
  #(Consider using a formula, as well, to aggregate multiple columns of text)
}

n3gramfreq <- tm::TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))
# put into data frame
freq.trigram.twitter <- data.frame(word = trigram.twitterTdm$dimnames$Terms, frequency = trigram.twitterTdm$v)
# reorder by descending frequency
freq.trigram.twitter <- plyr::arrange(freq.trigram.twitter, -frequency)

primarycutoff = 1458662400

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

r2trainprior = dclone(rtrainprior)
r2trainprior$Republican.Candidate = -1*(rtrainprior$Republican.Candidate==1)+2

r2testprior = dclone(rtestprior)
r2testprior$Republican.Candidate = -1*(r2testprior$Republican.Candidate==1)+2

