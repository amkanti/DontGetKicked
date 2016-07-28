#Final Project - Spring 2016
#Course       : CS 513 A
#Name         : Amulya Manchikanti, Sarayu Rakshith
#Student ID   : 10409673, 10405238


#Remove all the objects
rm(list=ls())

#Load the following CSV file to your R environment
mydata <- read.csv("E:/2nd_Sem/Data Mining_CS513/Final Project/Dontgetkicked/SUBMISSION/DontGetKicked_Amulya_Sarayu/Data Sets/training_data_C50.csv")
attach(mydata)

#Start Decision Tree
library('C50')

#considering relevant fields
partData <- as.data.frame(cbind(VehicleAge, IsBadBuy, Make, Transmission, VehOdo, TopThreeAmericanName, Nationality, MMRAcquisitionAuctionAveragePrice, MMRAcquisitionAuctionCleanPrice,
                                MMRAcquisitionRetailAveragePrice, MMRAcquisitonRetailCleanPrice, MMRCurrentAuctionAveragePrice, MMRCurrentAuctionCleanPrice, MMRCurrentRetailAveragePrice, MMRCurrentRetailCleanPrice, WarrantyCost))


#converting outcome variable as factor outcome
partData$IsBadBuy <- as.factor(partData$IsBadBuy)

# fixing empty character level names 
levels(partData$Transmission)[1] = "missing"

# % of the sample size
smp_size <- floor(0.75 * nrow(mydata))

#set the seed to make your partition reproductible
set.seed(123)
training_data <- sample(seq_len(nrow(partData)), size = smp_size)

#dividing into training and test data sets
trainData <- partData[training_data, ]
View(trainData)
testData <- partData[-training_data, ] 
View(testData)

#applying rule model
ruleModel <- C5.0(IsBadBuy~., data = partData , rules = TRUE)          
summary.C5.0(ruleModel)

#Create a C50 decision Tree
treeModel <- C5.0(x = partData[,-2], y = partData$IsBadBuy,
                  control = C5.0Control(winnow = TRUE))
summary(treeModel)

#plot the tree
plot(treeModel)

#retrieve actual values
actual_value <-testData$IsBadBuy

#Apply model to test data
predicted_val_c5<- predict(ruleModel,testData[,-2])

#Observe expected vs actual values
table(actual_value,predicted_val_c5)

#Calculate error rate
mean(predicted_val_c5!=actual_value) * 100
