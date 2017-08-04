library(stringr)

stringulus = readChar("../Data Mining 2 Final/bdfw/counties list",
                      file.info("../Data Mining 2 Final/bdfw/counties list")$size)

#Makes a wikipedia query for the first version of an article of names in x, and returns timestamps.
firstarticledate  = function(x){
  datevec = ""
  for(i in 1:nrow(gameRatingsDates)){
    tempurl =paste("https://en.wikipedia.org/w/api.php?action=query&format=json&prop=revisions&titles=",
                   x[i],
                   "&rvlimit=1&rvprop=timestamp|content&rvparse=1&rvdir=newer", collapse = "")
    tempmyplace = "Sony/temp"
    download.file(tempurl,tempmyplace,method = "wget", quiet = TRUE)
    doc = readLines(tempmyplace,warn = FALSE)
    tempn = str_locate_all(doc, "timestamp")[[1]][2]
    datevec[i] = substr(doc, tempn+4, tempn+13)
  }
  return(datevec)
}

