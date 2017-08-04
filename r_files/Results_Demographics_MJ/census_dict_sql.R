
#### Getting the Data ####
infile <- '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/county_facts_dictionary.csv'
data <- read.csv(infile, header = TRUE)


#### table Creation ####
out_file <- '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/census_dictionary.txt'
columns <- c('VARCHAR(9)', 'VARCHAR(100)')
constraints <- ''
table.creation(out_file, data, columns, constraints, 'census_dictionary')


#### old ####
sink(output_filename)
cat('CREATE TABLE census_dictionary ( \n')
sink()

sink(output_filename, append = TRUE)
for(i in 1:n){
  col <- paste(colnames(data)[i], columns[i], sep = '   ')
  if(i <= n-1){
    col <- paste0(col, ',')
  }
  cat(col,sep = '\n', append = TRUE)
}
cat(');', append = TRUE)
sink()

sink(output_filename, append = TRUE)
for(i in 1:r){
  row.val <- 'INSERT INTO census_data'
  row.val <- paste(row.val, 'VALUES(', sep = '\n\t')
  attribute <- paste0('"', data[i, 1], '"')
  name <- paste0('"', data[i, 2], '"')
  row.val <- paste(row.val, attribute, ',', name, ');')
  cat(row.val, sep = '\n', append = TRUE)
}
sink()