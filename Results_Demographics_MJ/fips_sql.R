
#### Getting the Data ####
national_county <- read.csv("~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/national_county.txt", header=FALSE)
colnames(national_county) <- c('state', 'state_fips', 'county_fips', 'county_name', 'something')

national_county <- national_county[ , -5]

library(stringr)
# Need to fix the fips codes for the states codes up until 10 - they are missing a character.
national_county$state_fips <- str_pad(as.character(national_county$state_fips), 2, side = "left", pad = "0")
national_county$county_fips <- str_pad(as.character(national_county$county_fips), 3, side = "left", pad = "0")

national_county$fips <- paste0(national_county$state_fips, national_county$county_fips)
national_county <- national_county[, c(5, 4, 1)]
colnames(national_county) <- c('fips_code', 'county', 'state')

fips_al <- national_county[which(national_county$state == 'AL'), ]

#### Creating the .txt file ####
columns <- c('VARCHAR(5)', 'VARCHAR(33)', 'VARCHAR(2)')
tablename <- 'fips'
out_file <- '~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/fips.txt'
data <- national_county
constraints <- 'CONSTRAINT fips_pk PRIMARY KEY (fips_code)'

table.creation(out_file, national_county, columns, constraints, 'fips')

insert.statement('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/DataBaseCreation/fips_AL.txt', 
                 columns, fips_al, 'fips')




#### OLD #### 
#create the datatypes for each column #
# columns = c('VARCHAR(5)', 'VARCHAR(33)', 'VARCHAR(2)')
# tablename = 'fips'
# 
# 
# sink('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/fips.txt')
# cat('CREATE TABLE fips ( \n')
# sink()
# 
# sink('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/fips.txt', append = TRUE)
# for(i in 1:ncol(national_county)){
#   col <- paste(colnames(national_county)[i], columns[i], sep = '   ')
#   if(i <= ncol(national_county)-1){
#     col <- paste0(col, ',')
#   }
#   cat(col,sep = '\n', append = TRUE)
# }
# cat(');', append = TRUE)
# sink()
# 
# 
# sink('~/Dropbox/DataMining/Scraping/Visualization/Results_Demographics_MJ/fips.txt', append = TRUE)
# for(i in 1:nrow(national_county)){
#   row.val <- 'INSERT INTO fips'
#   row.val <- paste(row.val, 'VALUES(', sep = '\t')
#   row.val <- paste0(row.val, "'", national_county[i, 1], "'", ',')
#   row.val <- paste0(row.val, "'", national_county[i, 2], "'", ',')
#   row.val <- paste0(row.val, "'", national_county[i, 3], "'", ');')
#   cat(row.val, sep = '\n', append = TRUE)
# }
# sink()
