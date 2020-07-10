fileURL<- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"

directory<- getwd()
download.file(fileURL, destfile=paste0(directory,"/restaurants.csv"),method ="curl")
restaurant_data<- read.csv(paste(directory,"/restaurants.csv",sep=""))

library(dplyr)
data<- select(restaurant_data,-(7:9))

head(data,n=3)
#     name zipCode neighborhood councilDistrict policeDistrict                      Location.1
# 1   410   21206    Frankford               2   NORTHEASTERN 4509 BELAIR ROAD\nBaltimore, MD
# 2  1919   21231  Fells Point               1   SOUTHEASTERN    1919 FLEET ST\nBaltimore, MD
# 3 SAUTE   21224       Canton               1   SOUTHEASTERN   2844 HUDSON ST\nBaltimore, MD

tail(data,n=3)
#                  name zipCode  neighborhood councilDistrict policeDistrict                       Location.1
# 1325 ZINK'S CAF\u0090   21213 Belair-Edison              13   NORTHEASTERN 3300 LAWNVIEW AVE\nBaltimore, MD
# 1326     ZISSIMOS BAR   21211       Hampden               7       NORTHERN      1023 36TH ST\nBaltimore, MD
# 1327           ZORBAS   21224     Greektown               2   SOUTHEASTERN  4710 EASTERN Ave\nBaltimore, MD

## or we can just ask for summary

summary(data)
#     name              zipCode       neighborhood       councilDistrict  policeDistrict      Location.1       
#Length:1327        Min.   :-21226   Length:1327        Min.   : 1.000   Length:1327        Length:1327       
#Class :character   1st Qu.: 21202   Class :character   1st Qu.: 2.000   Class :character   Class :character  
#Mode  :character   Median : 21218   Mode  :character   Median : 9.000   Mode  :character   Mode  :character  
#Mean   : 21185                      Mean   : 7.191                                        
#3rd Qu.: 21226                      3rd Qu.:11.000                                        
#Max.   : 21287                      Max.   :14.000  

quantile(data$councilDistrict,na.rm=TRUE)
# 0%  25%  50%  75% 100% 
# 1    2    9   11   14 
# smallest value is 1, max is 14 and median is 9

###making a table from this dataframe

table(data$zipCode, useNA = "ifany")
# -21226  21201  21202  21205  21206  21207  21208  21209  21210  21211  21212  21213  21214  21215  21216  21217  21218  21220 
#   1    136    201     27     30      4      1      8     23     41     28     31     17     54     10     32     69      1 
#21222  21223  21224  21225  21226  21227  21229  21230  21231  21234  21237  21239  21251  21287 
#   7     56    199     19     18      4     13    156    127      7      1      3      2      1 

# shows the distribution of zipcodes
# had there been any NAs, they wouldve had their own section at the end

table(data$councilDistrict,data$zipCode)
# this would return a table between the two variables in the argument


### checking for NA in the entire data

colSums(is.na(data))
#  name         zipCode    neighborhood councilDistrict  policeDistrict      Location.1 
#   0               0               0               0               0               0 


# see how many zipcodes are equal to 21212

table(data$zipCode %in% c("21212"))
# FALSE  TRUE 
# 1299    28 
# thus 28 restaurants have 21212 zipcodes

# seeing info for these 28 restaurants

data[ (data$zipCode %in% c("21212")),]
#                                       name zipCode              neighborhood councilDistrict policeDistrict
# 29                      BAY ATLANTIC CLUB   21212                  Downtown              11        CENTRAL
# 92                              ATWATER'S   21212 Chinquapin Park-Belvedere               4       NORTHERN
# 187                              CAFE ZEN   21212                  Rosebank               4       NORTHERN
# 220                   CERIELLO FINE FOODS   21212 Chinquapin Park-Belvedere               4       NORTHERN
# 373                         DUNKIN DONUTS   21212                  Homeland               4       NORTHERN
# 417                      FIELDS OLD TRAIL   21212                Mid-Govans               4       NORTHERN


#### crosstabs : to find relationships


summary(data)

data_cross_tab <- xtabs(zipCode~ neighborhood + councilDistrict, data )
#                                                                       councilDistrict
# neighborhood                           1       2       3       4       5       6       7       8       9      10      11    12      13     14
# Abell                                  0       0       0       0       0       0       0       0       0       0       0      0     0      84872
# Arlington                              0       0       0       0   63645       0       0       0       0       0       0      0     0      0
# Armistead Gardens                      0   21205       0       0       0       0       0       0       0       0       0      0    21205   0
# Baltimore Highlands               127344  212240       0       0       0       0       0       0       0       0       0      0     0      0

# zipcodes was distributed between neighbourhood and councildistrict


########## creating new variables


data$nearME <- data$neighborhood %in% c("Roland Park","Homeland")
table(data$nearME)
# FALSE  TRUE 
# 1314    13 

data$wrongzipcode <- ifelse(data$zipCode<0,TRUE,FALSE)
summary(data$wrongzipcode)
#   Mode     FALSE    TRUE 
# logical    1326       1 

#hence, one zipcode is wrong


########## cutting data
# now assume we want to break the ziocdes based on their quantiles
# quantiles in r represent 0,25,50,75, and 100th percentile

data$zipGroups<- cut(data$zipCode, breaks=quantile(data$zipCode))
table(data$zipGroups)
#(-2.123e+04,2.12e+04]  (2.12e+04,2.122e+04] (2.122e+04,2.123e+04] (2.123e+04,2.129e+04] 
#      337                   375                   282                   332 
#   0 - 25                 25-50                  50-75                75-100


# OR we can use this you want specific number of data cuts

library(Hmisc)
data$zipGroups2 <- cut2(data$zipCode, g=4)
table(data$zipGroups2)
# [-21226,21205) [ 21205,21220) [ 21220,21227) [ 21227,21287] 
#     338            375            300            314 


######### Renaming Columns
data2<- data
data2<- rename(data2, Zip_Codes=zipCode, Restaurant_names = name)

######### Grouping

data3<- data2
data3<- mutate(data3,closefar = factor(1*(councilDistrict <5),labels=c("close","far")))
far_or_not <- group_by(data3,closefar)