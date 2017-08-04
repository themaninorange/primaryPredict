library(gdata)


# importing data
counties = read.xls("~/Downloads/US_FIPS_Codes.xls", sheet = 1, method = "csv")

ia.nh.sc.nv = read.csv("~/Downloads/2016_presidential_election/primary_results.csv", header = TRUE)

# getting rid of heading 
counties = counties[-1, ]
colnames(counties) = c("state", "county", "fips_state", "fips_county", "dem", "rep")
rownames(counties) = 1:3142

# adding complete FIPS coding at end 
# Don't run right now, look at this later
# counties$fips = paste(as.character(counties$fips_state), as.character(counties$fips_county), collapse = '')

# Creating Column for Democratic Nominee
democrat = character(length = dim(counties)[1])

# Create Column for Republican Nominee
republican = character(length = dim(counties)[1])

counties$dem = democrat
counties$rep = republican

# Excluding Maine Republican Column (add NA's)
counties$rep[counties$state == "Maine"] = NA

# Excluding Colorado Republican Column (add NA's)
counties$rep[counties$state == "Colorado"] = NA

# Excluding Wyoming Democratic Column (add NA's)
counties$dem[counties$state == "Wyoming"] = NA

# Excluding Idaho Democratic Column (add NA's)
counties$dem[counties$state == "Idaho"] = NA



#### MAINE ####
me = which(counties$state == "Maine")

# Maine Democrat vector 
maine.dem = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

# Maine Republican Vector
maine.rep = c(NA, rep = length(maine.dem))



#### NEW HAMPSHIRE ####
nh = which(counties$state == "New Hampshire")
# New Hampshire Democrat Vector
nh.dem = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

  
# New Hampshire Republican Vector
nh.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)



#### VERMONT ####
vt = which(counties$state == "Vermont")
# Vermont Democrat Vector
vt.dem = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

#Vermont Republican Vector
vt.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1)



#### MASSACHUSETTS ####
ma = which(counties$state == "Massachusetts")
# Massachusetts Democrat Vector
ma.dem = c(1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1)

#Masachusetts Republican Vector
ma.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)



#### VIRGINIA ####
va = which(counties$state == "Virginia")
# Virginia Democrat Vector
va.dem = c(2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 
           2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 
           2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1,
           NA, 2, 2, 1, 2, NA, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 
           2, 2, NA, 1, 2, 2, 2, 1, 2)

# Virginia Republican Vector
va.rep = c(1, NA, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 2, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1,
           1, 1, NA, 1, 1, 1, 1, 1, NA, 1, 1, 1, NA, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, NA, 1, 1, NA, 1, NA,
           1, 1, 1, 1, NA, NA, 1, NA, 1, 1, NA, 1, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, 1, 1, NA, NA, 1, 1, 1, NA, 1)



#### North Carolina ####
nc = which(counties$state == "North Carolina")
# North Carolina Democrat Vector
nc.dem = c(1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 
           2, 2, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1,
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1)


# North Carolina Republican Vector
nc.rep = c(3, 3, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 3, 1, 3, 3, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           3, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 1, 3, 3, 1, 1, 3, 1, 1)



#### South Carolina ####
sc = which(counties$state == "South Carolina")

# South Carolina Democrat Vector
sc.dem = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
# South Carolina Republican Vector
sc.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)



#### GEORGIA ####
ga = which(counties$state == "Georgia")

# Georgia Democrat Vector
ga.dem = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

# Georgia Republican Vector
ga.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, NA, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)



#### FLORIDA ####
fl = which(counties$state == "Florida")
# Florida Democrat Vector
fl.dem = c(2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 
           2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
           1, 2, 1, 2, 2, 2, 2)

# Florida Republican Vector
fl.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1)



#### ALABAMA ####
al = which(counties$state == "Alabama")
# Alabama Democrat Vector
al.dem = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
           2, 2, 2, 2, 2, 2, 2)

# Alabama Republican Vector
al.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           1, 1, 1, 1, 1, 1, 1)



#### MISSISSIPPI ####
ms = which(counties$state == "Mississippi")



#### TENNESSEE ####
tn = which(counties$state == "Tennessee")



#### KENTUCKY ####
ky = which(counties$state == "Kentucky")



#### OHIO ####
oh = which(counties$state == "Ohio")
# Ohio Democrat Vector
oh.dem = c(2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 1, 2, 
           2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
           2, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2)

# Ohio Republican Vector
oh.rep = c(1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 1, 
           2, 2, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 1, 2, 1, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1, 2, 2,
           1, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 2, 1, 1, 2, 2, 1, 2, 1, 2, 2, 2, 2)



#### MICHIGAN ####
mi = which(counties$state == "Michigan")




#### ILLINOIS ####
il = which(counties$state == "Illinois")

il.dem = c(2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2,
           2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 2, 1, 
           1, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1)

il.rep = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 1, 3, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1,
           1, 1, 1, 3, 3, 3, 1, 1, 1, 3, 3, 3, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3, 3,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3)



#### MISSOURI ####
mo = which(counties$state == "Missouri")

# Missouri Democrat Vector
mo.dem = c(1, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 
           2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 1, 1, 
           2, 2, 2, 1, 2, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 2, 
           2, 2, 2, 1, 2, 2, 2, 1, 2, 1, 2, 2, 2, 1, 2, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2)


# Missouri Republican Vector
mo.rep = c(1, 1, 1, 3, 3, 3, 1, 1, 1, 3, 1, 1, 1, 3, 1, 3, 1, 1, 3, 3, 1, 3, 1, 3, 1, 3, 3, 1, 3, 1, 
           1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 3, 1, 1, 3, 3, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 
           1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1)


#### ARKANSAS ####
ar = which(counties$state == "Arkansas")



#### LOUISIANA ####
la = which(counties$state == "Louisiana")



#### TEXAS ####
tx = which(counties$state == "Texas")



#### OKLAHOMA ####
ok = which(counties$state == "Oklahoma")



#### KANSAS ####
kn = which(counties$state == "Kansas")



#### NEBRASKA ####
nb = which(counties$state == "Nebraska")


#### IOWA ####
ia = which(counties$state == "Iowa")



#### MINNESOTA ####
mn = which(counties$state == "Minnesota")



#### COLORADO ####
co = which(counties$state == "Colorado")



#### WYOMING ####
wy = which(counties$state == "Wyoming")



#### IDAHO ####
id = which(counties$state == "Idaho")
id.dem = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
id.rep = c(3, 1, 3, 3, 3, 3, 1, 3, 3, 3, 3, 3, 1, 3, 3,
           3, 3, 1, 1, 1, 3, 3, 3, 3, 1, 3, 3, 3, 3, 1,
           1, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 1, 3)




#### ARIZONA ####
az = which(counties$state == "Arizona")
az.dem = c(2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
az.rep = c(1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)




#### UTAH ####
ut = which(counties$state == "Utah")
ut.dem = c(NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
           NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
ut.rep = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
           3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)



#### NEVADA ####
nv = which(counties$state == "Nevada")




#### ALASKA ####
ak = which(counties$state == "Alaska")



#### HAWAII ####
hi = which(counties$state == "Hawaii")

all.dem = c()
all.dem = factor(all.dem)
levels(all.dem) = c("bernie", "hillary")

all.rep = c()
all.rep = factor(all.rep)
levels(all.rep) = c("trump", "kasich", "cruz")

counties$dem = all.dem
counties$rep = all.rep


states = c("Arizona", "Florida", "Illinois", "Missouri", "North Carolina", "Ohio", "Idaho", "Utah")
