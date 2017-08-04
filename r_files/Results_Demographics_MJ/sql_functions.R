create.statement <- function(filename, data, columns, constraints, tablename){
  n <- ncol(data)
  r <- nrow(data)
  
  sink(filename)
  cat('CREATE TABLE', tablename,  '(\n')
  sink()
  
  sink(filename, append = TRUE)
  for(i in 1:ncol(data)){
    col <- paste(colnames(data)[i], columns[i], sep = '\t')
    col <- paste0(col, ',')
    cat(col,sep = '\n', append = TRUE)
  }
  cat(constraints, sep = '\n', append = TRUE)
  
  cat(');', sep = '\n', append = TRUE)
  sink()
}



insert.statement <- function(filename, columns, data, tablename){
  chars <- which(grepl('char', columns, ignore.case = TRUE) == TRUE)
  n <- ncol(data)
  m <- n-1
  r <- nrow(data)
  sink(filename, append = TRUE)
  for(i in 1:r){
    row.val <- paste('INSERT INTO', tablename, 'VALUES(')
    for(j in 1:m){
      ifelse(is.element(j, chars), row.val <- paste0(row.val, "'", data[i, j], "'", ','),
             ifelse(columns[j] == 'DATE', row.val <- paste0(row.val, 'TO_DATE(', "'", data.tweets[i, j], "'", ',', "'", 'mon-dd-YYYY', "'", '),'),
                    row.val <- paste0(row.val, data[i, j], ',')))
    }
    if(is.element(n, chars)) row.val <- paste0(row.val, "'", data[i, n], "'", ');') else row.val <- paste0(row.val, 
                                                                                                           data[i, n], ');')
    cat(row.val, sep = '\n', append = TRUE)
  }
    
  sink()  
}




table.creation <- function(filename, data, columns, constraints, tablename){
  create.statement(filename, data, columns, constraints, tablename)
  insert.statement(filename, columns, data, tablename)
}


  

