#### Getting the data ####
results <- read.csv('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/PrimaryResults.csv')
unique(results$state[which(nchar(as.character(results$fips)) > 5)])

results.data <- results[which(results$state != 'Alaska'), ]
results.data <- results.data[which(results.data$state != 'Connecticut'), ]
results.data <- results.data[which(results.data$state != 'Illinois'), ]
results.data <- results.data[which(results.data$state != 'Kansas'), ]
results.data <- results.data[which(results.data$state != 'Maine'), ]
results.data <- results.data[which(results.data$state != 'Massachusetts'), ]
results.data <- results.data[which(results.data$state != 'North Dakota'), ]
results.data <- results.data[which(results.data$state != 'Rhode Island'), ]
results.data <- results.data[which(results.data$state != 'Vermont'), ]
results.data <- results.data[which(results.data$state != 'Wyoming'), ]

results.data$fips <- str_pad(as.character(results.data$fips), 5, side = "left", pad = "0")

results.data <- results.data[ , 4:8]

x <- which(grepl("'", results.data$candidate) == TRUE)

results.data$candidate <- as.character(results.data$candidate)
results.data$candidate[x] <- 'Martin OMalley'
results.data$candidate <- as.factor(results.data$candidate)

results.al <- results.data[1:469, ]
results.al <- results.al[which(results.al$fips == '01001' |
                                 results.al$fips == '01003' |
                                 results.al$fips == '01007'), ]
colnames(results.al)[1] <- 'fips_code'

#### table creation ####
out_file <- '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/results.txt'
columns <- c('VARCHAR(5)', 'VARCHAR(15)', 'VARCHAR(30)', 'NUMBER(10)', 'NUMBER(4, 3)')
constraints <- 'CONSTRAINT results_pk PRIMARY KEY (fips_code, party, candidate), \n
CONSTRAINT results_frac_check CHECK (fraction_votes BETWEEN 0 AND 1), \n
CONSTRAINT results_fk FOREIGN KEY (fips_code) REFERENCES fips (fips_code)'

table.creation(out_file, results.data, columns, constraints, 'results')

table.creation('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/results_AL.txt', results.al, 
                 columns, constraints, 'results')



#### OLD ####
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
cat(');\n', append = TRUE)
sink()

sink(output_filename, append = TRUE)
for(i in 1:r){
  row.val <- 'INSERT INTO census_data'
  row.val <- paste(row.val, 'VALUES(', sep = '\n\t')
  fips <- paste0('"', data[i, 1], "'")
  party <- paste0("'", data[i, 2], "'")
  candidate <- paste0("'", data[i, 3], "'")
  row.val <- paste(row.val, fips, ',', party, ',', candidate, ',', data[i, 4], ',', data[i, 5], ');')
  cat(row.val, sep = '\n', append = TRUE)
}
sink()