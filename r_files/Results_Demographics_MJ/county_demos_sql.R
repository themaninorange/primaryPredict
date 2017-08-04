#### Data ####
demographics <- read.csv('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/county_demos_fips.csv')
demographics <- demographics[ , -1]
colnames(demographics)[1] <- 'fips_code'
y <- c(2, 3)
demographics <- demographics[, -y]

demographics$year <- 2010

demographics <- demographics[ ,c(1, 53, 2:52)]
demographics$fips_code <- str_pad(as.character(demographics$fips_code), 5, side = "left", pad = "0")
demographics.al <- demographics[1:67, ]
 

#### table creation ####
#create the datatypes for each column #
columns = c('VARCHAR(5)', 'VARCHAR(4)', 'NUMBER(10, 0)', 'NUMBER(10, 0)', 'NUMBER(4, 1)', 'NUMBER(10, 0)',
            'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)',
            'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)',
            'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(10, 0)', 'NUMBER(4, 1)', 'NUMBER(10, 0)', 'NUMBER(4, 1)',
            'NUMBER(4, 1)', 'NUMBER(10, 0)', 'NUMBER(10, 0)', 'NUMBER(4, 1)', 'NUMBER(7, 0)', 'NUMBER(7, 0)', 'NUMBER(4, 1)',
            'NUMBER(10, 0)', 'NUMBER(10, 0)', 'NUMBER(4, 1)', 'NUMBER(10, 0)', 'NUMBER(10, 0)', 'NUMBER(4, 1)', 'NUMBER(4, 1)',
            'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(4, 1)', 'NUMBER(10, 0)', 'NUMBER(10, 0)', 'NUMBER(10, 0)',
            'NUMBER(7, 0)', 'NUMBER(10, 0)', 'NUMBER(10, 0)', 'NUMBER(10, 0)', 'NUMBER(4, 1)')

out_file <- '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/census_data.txt'
constraints <- 'CONSTRAINT results_pk PRIMARY KEY (fips_code, year)'

table.creation(out_file, demographics, columns, constraints, 'census_data')

insert.statement('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/census_data_AL.txt', 
                 columns, demographics.al, 'census_data')






#### OLD ####
sink('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/census_data.txt')
cat('CREATE TABLE census_data ( \n')
sink()

sink('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/census_data.txt', append = TRUE)
for(i in 1:ncol(demographics)){
  col <- paste(colnames(demographics)[i], columns[i], sep = '   ')
  if(i <= ncol(demographics)-1){
    col <- paste0(col, ',')
  }
  cat(col,sep = '\n', append = TRUE)
}
cat(');', append = TRUE)
sink()


sink('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/census_data.txt', append = TRUE)
for(i in 1:nrow(demographics)){
  row.val <- 'INSERT INTO census_data'
  row.val <- paste(row.val, 'VALUES(', sep = '\n')
  for(j in 1:2){
    row.val <- paste(row.val, '"', demographics[i, j], '"', ',', sep ='')
  }
  for(j in 3:(ncol(demographics) - 1)){
    row.val <- paste(row.val, demographics[i, j], ',', sep ='')
  }
  row.val <- paste(row.val, demographics[i, 53], ');', sep = ' ')
  cat(row.val, sep = '\n', append = TRUE)
}
sink()
