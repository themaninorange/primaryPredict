Predicting Primary Results Using Twitter
==========

Sentiment analysis revolves around establishing whether a document expresses positive, negative, or neutral feelings.  My partners and I wanted to establish political sentiment by correlating tweets with existing primary data.  Are there key patterns in tweets from counties which prefered Hillary Clinton to Bernie Sanders?  Can those same patterns be used to predict the results of later primaries?

Word Cloud by Party | ~
:-------------------------:|:-------------------------:
![alt text](https://github.com/themaninorange/primaryPredict/blob/master/visuals/demwordcloud.png "Democrat Word Cloud")  | ![alt text](https://github.com/themaninorange/primaryPredict/blob/master/visuals/repwordcloud.png "Republican Word Cloud")



## Gathering Data

The `gatherer.R` script cycles through every county in the Untited States and calls the twitter search API as frequently as it is allowed to get tweets that match the geographic criteria.

To run, it requires:

 - `twauthkeys.list`, the user's Twitter authorization information.
 - `completething.Rda`, a data frame of county information.
 
 This script will then create dataframe correspinding to the date the searches were made and the state that the searches belonged to.
 
## Cleaning Data

The `TwitCleanFn.R` script contains a function which takes "unclean" tweets and removes punctuation, weird symbols, and makes everything lowercase.  This is then used to create a new column in data frames for clean tweets.

## Processing Data

The `randomforestscript.R` script is one script which runs a machine learning algorithm on the cleaned tweet data.  The first blocks of code run a Bag of Words method, using only word frequencies to make the prediction.  The last blocks of code run an ngram method, preserving the information contained in word ordering.

Using the cleaned tweet data and the `tm` library, we create a corpus and document term matrix for all tweets.  We first try to determine if the tweets can be traced to a particular class of county.  We then try to predict the result of a county itself.  We made mixed success.  With the particular wave of primaries being investigated, Trump won 40 out of 43 counties, and Kasich won none.  Because of this, we separate into Trump vs Not Trump.  Our model typically predicts that Trump wins all 43 of these counties.

. | . |prediction|. 
---|---|---|---
. | . |Not Trump | Trump
actual  |Not Trump | 0 | 3
.      |Trump     | 0 | 40

While this is an accuracy of 93%, it seems like a weak result.  Fortunately, the Democrats are a bit more interesting.  Here are a few sample tables from models using 100 trees.


<table>
<tr><td>

. | . |prediction|. 
---|---|---|---
. | . |Bernie | Hillary
actual | Bernie  |   14   |   17
. | Hillary      |   0    |   12

</td><td> 

. | . |prediction|. 
---|---|---|---
. | . |Bernie | Hillary
actual | Bernie  |   0   |   31
. | Hillary      |   0    |   12

</td></tr>
<tr><td>

. | . |prediction|. 
---|---|---|---
. | . |Bernie | Hillary
actual | Bernie  |   6   |   25
. | Hillary      |   0    |   12

</td><td> 

. | . |prediction|. 
---|---|---|---
. | . |Bernie | Hillary
actual | Bernie  |   1   |   30
. | Hillary      |   0    |   12

</td></tr> 
</table>

The models have a wide variance in predicting Democrats because of the makeup of the training and testing sets.  The training set had disproportionate counties going to Hillary, while the testing set had more Bernie wins.  We had decided to investigate a random forest because of the large number of columns.  However, the random selection of terms to make predictions on means that the "correct" terms will sometimes be selected.  The pattern, here, is to bias toward predicting a Hillary win since that is what the model was primarily based on, the each individual term, by itself, has a relatively small amount of predictive power.

While not strictly a success, this gives us hope that a model like SVM, where we can adjust the "stiffness" of the model will be more successful.
