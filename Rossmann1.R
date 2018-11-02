setwd("~/Desktop/Kaggle")
#Step 1 : Load Datasets
store <- read.csv(file = "RossStore.csv", stringsAsFactors = FALSE, header = TRUE)
train <- read.csv(file = "Rosstrain.csv", stringsAsFactors = FALSE, header = TRUE)
test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#Step 2: Look at Datasets (2Exploratory Analysis)
str(store)
str(train)
str(test)
summary(store)
summary(train)
summary(test)
head(store)
head(train)
head(test)

#Step 3 : add columns to test dataset to combine later with train and store so duplicate work does not need to be done later.
test$StoreType <- NA
test$Assortment <- NA
test$CompetitionDistance <- NA
test$CompetitionOpenSinceMonth <- NA
test$CompetitionOpenSinceYear <- NA
test$Promo2SinceWeek <- NA
test$Promo2SinceYear <- NA
test$PromoInterval <- NA
test$Sales <- NA
test$Customers <- NA
test$Promo2 <- NA

#Step 4: combine store and train on Store
combo <- merge(store, train, by = "Store")

#Step 5: Feature engineering
#Add IsTrainSet variable decipher between datasets
combo$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE

#add Id column to combo
combo$Id <-  c(1:1017209)

#rearrange Id column
combo <- subset(combo, select = c(20, 1:19))
#rearrange Test columns to match combo
test <- subset(test, select = c(1:2, 9, 10, 11, 12, 13, 19, 14, 15, 16, 3, 4, 17, 18, 5, 6, 7, 8, 20))

#Step 6: Combine data sets
combotest.combined <- rbind(combo, test)
#Data types
str(combotest.combined)
combotest.combined$StoreType <- as.factor(combotest.combined$StoreType)
combotest.combined$Assortment <- as.factor(combotest.combined$Assortment)
combotest.combined$StateHoliday <- as.factor(combotest.combined$StateHoliday)
combotest.combined$PromoInterval <- as.factor(combotest.combined$PromoInterval)
#combotest.combined$Store <- as.factor(combotest.combined$Store) 
combotest.combined$Store <- as.numeric(combotest.combined$Store)
combotest.combined$CompetitionOpenSinceMonth <- as.factor(combotest.combined$CompetitionOpenSinceMonth) 
combotest.combined$CompetitionOpenSinceYear <- as.factor(combotest.combined$CompetitionOpenSinceYear) 
combotest.combined$Promo2SinceWeek <- as.factor(combotest.combined$Promo2SinceWeek) 
combotest.combined$Promo2SinceYear <- as.factor(combotest.combined$Promo2SinceYear)
combotest.combined$PromoInterval <- as.factor(combotest.combined$PromoInterval)
combotest.combined$Promo2 <- as.factor(combotest.combined$Promo2)
combotest.combined$Promo <- as.factor(combotest.combined$Promo)
combotest.combined$Open <- as.factor(combotest.combined$Open)
#Calculate number of levels for each categorical variable
combotest.combined.levels <- cbind.data.frame(Variable = names(combotest.combined), Total_Levels = sapply(combotest.combined, function(x){as.numeric(length(levels(x)))}))
print(combotest.combined.levels)
#check names of different levels
table(combotest.combined$Store)
table(combotest.combined$StoreType)
table(combotest.combined$Assortment)
levels(combotest.combined$Assortment) <- c("Basic", "Extra", "Extended")
table(combotest.combined$PromoInterval)
#PromoInterval has a missing level
levels(combotest.combined$PromoInterval) <- c("NoPromo", "Feb, May, Aug, Nov", "Jan, Apr, Jul, Oct", "Mar, Jun, Sept, Dec") 
table(combotest.combined$Promo2SinceYear)
table(combotest.combined$Promo2SinceWeek)
table(combotest.combined$Promo2)
levels(combotest.combined$Promo2) <- c("NoParticipate", "Participating")
table(combotest.combined$Promo)
levels(combotest.combined$Promo) <- c("NoPromo", "PromoRunning")
table(combotest.combined$StateHoliday)
levels(combotest.combined$StateHoliday) <- c("NoHoliday", "PublicHoliday", "Easter", "Christmas")
table(combotest.combined$Open)
levels(combotest.combined$Open) <- c("Closed", "Open")
# Load ggplot2 package to use for visualizations
library(ggplot2)

#Step 7: Missing values
sum(is.na(train))
sum(is.na(store))
#look at records that are missing in store dataset
NACompDist <- as.character(store[which(is.na(as.character(store$CompetitionDistance))), "CompetitionDistance" ])
store[which(store$CompetitionDistance %in% NACompDist),]
NACOSMonth <- as.character(store[which(is.na(as.character(store$CompetitionOpenSinceMonth))), "CompetitionOpenSinceMonth" ])
store[which(store$CompetitionOpenSinceMonth %in% NACOSMonth),]
NACOSYear <- as.character(store[which(is.na(as.character(store$CompetitionOpenSinceYear))), "CompetitionOpenSinceYear" ])
store[which(store$CompetitionOpenSinceYear %in% NACOSYear),]
NAPrSWeek <- as.character(store[which(is.na(as.character(store$Promo2SinceWeek))), "Promo2SinceWeek" ])
store[which(store$Promo2SinceWeek %in% NAPrSWeek),]
NAPrSYear <- as.character(store[which(is.na(as.character(store$Promo2SinceYear))), "Promo2SinceYear" ])
store[which(store$Promo2SinceYear %in% NAPrSYear),]

#Check State Holiday
table(combo$StateHoliday)
#Check School Holiday
table(combo$SchoolHoliday)
#Check Store Type
table(combo$StoreType)
#Check Assortment
table(combo$Assortment)
#Check Promo Interval
table(combo$PromoInterval)
#need to add "none" to PromoInterval***    

#competition open since month as factor
table(as.factor(store$CompetitionOpenSinceMonth))

#competition open since year as factor
table(as.factor(store$CompetitionOpenSinceYear))
#Promo2SinceWeek as factor
table(as.factor(store$Promo2SinceWeek))
#Promo2SinceYear as factor
table(as.factor(store$Promo2SinceYear))
#PromoInterval as factor
table(as.factor(store$PromoInterval))

#Check for factors in train dataset
table(as.factor(train$DayOfWeek))
table(as.factor(train$Open))
table(as.factor(train$Promo))
table(as.factor(train$StateHoliday))
table(as.factor(train$SchoolHoliday))

#Should I think about NA's in CompetitionOpenSince..?
summary(store$CompetitionDistance)
summary(store$CompetitionOpenSinceMonth)
summary(store$CompetitionOpenSinceYear)
summary(store$Promo2SinceWeek)
hist(store$Promo2SinceWeek)

#Step 8: Divide Date between Year, Month, Day, and Week number of Year.
combotest.combined$DateYear <- as.numeric(strftime(combotest.combined$Date, format = "%Y"))
combotest.combined$DateMonth <- as.numeric(strftime(combotest.combined$Date, format = "%m"))
combotest.combined$DateDay <- as.numeric(strftime(combotest.combined$Date, format = "%d"))
combotest.combined$DateWeek <- as.numeric(strftime(combotest.combined$Date, format = "%W"))
#drop Date column from combotest.combined and move DayOfWeek column
combotest.combined <- combotest.combined[c(1:11, 14:24, 12)]

#Step 9: check distributions on train data
hist(train$Store)
hist(train$DayOfWeek)
hist(train$Sales)
hist(train$Customers)
hist(train$Open)
hist(train$Promo)
hist(train$SchoolHoliday)

#check distributions on store data
hist(store$Store)
hist(store$CompetitionDistance)
hist(store$CompetitionOpenSinceMonth)
hist(store$CompetitionOpenSinceYear)
hist(store$Promo2)
hist(store$Promo2SinceWeek)
hist(store$Promo2SinceYear)
hist(store$PromoInterval)

#check distributions on combo as.numeric
hist(as.numeric(combo$StoreType))
hist(as.numeric(combo$Assortment))
hist(as.numeric(combo$CompetitionDistance))
hist(as.numeric(combo$CompetitionOpenSinceMonth))
hist(as.numeric(combo$CompetitionOpenSinceYear))
hist(as.numeric(combo$Promo2))
hist(as.numeric(combo$Promo2SinceWeek))
hist(as.numeric(combo$Promo2SinceYear))
hist(as.numeric(combo$PromoInterval))
hist(as.numeric(combo$Sales))
hist(as.numeric(combo$Customers))
hist(as.numeric(combo$Open))
hist(as.numeric(combo$Promo))
hist(as.numeric(combo$StateHoliday))
hist(as.numeric(combo$SchoolHoliday))
hist(as.numeric(combo$IsTrainSet))
hist(as.numeric(combo$DateYear))
hist(as.numeric(combo$DateMonth))
hist(as.numeric(combo$DateDay))
hist(as.numeric(combo$DateWeek))
hist(as.numeric(combo$DayOfWeek))
#More Feature engineering on combo
summary(combo)
summary(combo$StoreType)
summary(combo$Assortment)
summary(combo$CompetitionDistance)
summary(combo$CompetitionOpenSinceMonth)
summary(combo$CompetitionOpenSinceYear)
summary(combo$Promo2)
summary(combo$Promo2SinceWeek)
summary(combo$Promo2SinceYear)
summary(combo$PromoInterval)
summary(combo$Sales)
summary(combo$Customers)
summary(combo$Open)
summary(combo$Promo)
summary(combo$StateHoliday)
summary(combo$SchoolHoliday)
summary(combo$IsTrainSet)
summary(combo$DateYear)
summary(combo$DateMonth)
summary(combo$DateDay)
summary(combo$DateWeek)
summary(combo$DayOfWeek)

sum(is.na(combo$Sales))
sum(is.na(combo$Open))
sum(combo$Open == 0)

sum(combo$Open == 1)

#Bivariate statistics
#T test
t.test(combotest.combined$Sales[combotest.combined$Promo == "NoPromo"], combotest.combined$Sales[combotest.combined$Promo == "PromoRunning"])
#NoPromo and Promo Running is statisically significant

#Promo2
t.test(combotest.combined$Sales[combotest.combined$Promo2 == "NoParticipate"], combotest.combined$Sales[combotest.combined$Promo2 == "Participating"])
#sPROMO2ignificant
t.test(combotest.combined$Sales[combotest.combined$Customers])
t.test(combotest.combined$Sales[combotest.combined$StoreType])
t.test(train$Sales[as.factor(train$StoreType)])
t.test(combotest.combined$Sales[combotest.combined$CompetitionDistance])
#Comp Distance significant
t.test(combotest.combined$Sales[combotest.combined$StateHoliday])
t.test(train$Sales[train$StateHoliday])
#StateHoliday are significant
t.test(combotest.combined$Sales[combotest.combined$SchoolHoliday])
t.test(train$Sales[as.factor(train$SchoolHoliday)])
#SchoolHoliday are signigicant
#One way ANOVA
#StoreType
aov(combotest.combined$Sales ~ combotest.combined$StoreType, data = combotest.combined)
#not statisically significant
#Assortment
aov(combotest.combined$Sales ~ combotest.combined$Assortment, data = combotest.combined)
#not statisically significant
aov(combotest.combined$Sales ~ combotest.combined$Promo2SinceWeek, data = combotest.combined)
aov(combotest.combined$Sales ~ combotest.combined$Promo2SinceYear, data = combotest.combined)
aov(combotest.combined$Sales ~ combotest.combined$PromoInterval, data = combotest.combined)
aov(combotest.combined$Sales ~ combotest.combined$Open, data = combotest.combined)
aov(combotest.combined$Sales ~ combotest.combined$Promo, data = combotest.combined)
aov(combotest.combined$Sales ~ combotest.combined$StateHoliday, data = combotest.combined)

#Correlation
cor(combotest.combined$Sales, combotest.combined$CompetitionDistance, use = "complete.obs")
cor(combotest.combined$Sales, combotest.combined$Customers, use = "complete.obs")
#highly correlated but Customers not in Test dataset, Customers would be Target variable
cor(combotest.combined$Sales, combotest.combined$DateYear, use = "complete.obs")
cor(combotest.combined$Sales, combotest.combined$DateMonth, use = "complete.obs")


cor(combotest.combined$Sales, combotest.combined$DateDay, use = "complete.obs")
cor(combotest.combined$Sales, combotest.combined$DateWeek, use = "complete.obs")
cor(combotest.combined$Sales, combotest.combined$DayOfWeek, use = "complete.obs")

#Bar chart Sales compared to categorical variables
barplot(combotest.combined$Sales, names = combotest.combined$StoreType)

#What to do with Sales = 0?
sum(combotest.combined$Sales == 0, na.rm = TRUE)
#What to do with Customers = 0?
sum(combotest.combined$Customers == 0, na.rm = TRUE)
#2 cases were Sales were 0 but there were customers
which(combotest.combined$Sales == 0 & combotest.combined$Customers > 0)
#look at rows with 0 Sales and more than 0 Customers
combotest.combined[863563,]
combotest.combined[1002959,]
#should I add Sales to these data instances?
#convert CompetitionOpenSinceMonth and CompetitionOpenSinceYear into days?

#2Exploratory Analysis
#Plot numerical distributions
hist(combotest.combined$Store)
hist(combotest.combined$CompetitionDistance, na.rm =TRUE)
hist(combotest.combined$Sales, na.rm = TRUE)
hist(combotest.combined$Customers, na.rm = TRUE)
hist(combotest.combined$DateYear, na.rm = TRUE)
hist(combotest.combined$DateMonth, na.rm = TRUE)
hist(combotest.combined$DateDay, na.rm = TRUE)
hist(combotest.combined$DateWeek, na.rm = TRUE)
hist(combotest.combined$DayOfWeek, na.rm = TRUE)

