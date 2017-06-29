library(tm)
tw.cleantext = function(twit.dat){
  # Not sure if necessary.
  twit.dat$text.clean = gsub('<.*?>', '', twit.dat$text)
  # Removing punctuation 
  twit.dat$text.clean = gsub('[[:punct:]]', '', twit.dat$text.clean)
  # Removing Emojis and making everything text
  twit.dat$text.clean = sapply(twit.dat$text.clean, function(row) iconv(row, "latin1", "ASCII", 
                                                                        sub = ""))
  # Making everything lower case
  twit.dat$text.clean = tolower(twit.dat$text.clean)
  # Removing stopwords
  twit.dat$text.clean = removeWords(twit.dat$text.clean, stopwords("en"))
}