#Final Project - Spring 2016
#Course       : CS 513 A
#Name         : Amulya Manchikanti, Sarayu Rakshith
#Student ID   : 10409673, 10405238

#Remove all the objects
rm(list=ls())

#Load the following CSV file to your R environment
myData <- read.csv("E:/2nd_Sem/Data Mining_CS513/Final Project/Dontgetkicked/SUBMISSION/DontGetKicked_Amulya_Sarayu/Data Sets/training_data_ANN.csv")
attach(myData)
head(myData)

#considering relevant fields
importedData <- as.data.frame(cbind(RefId, IsBadBuy, VehicleAge, Transmission, WheelTypeID, VehOdo, TopThreeAmericanName, MMRAcquisitionAuctionAveragePrice, MMRAcquisitionAuctionCleanPrice,
                                MMRAcquisitionRetailAveragePrice, MMRAcquisitonRetailCleanPrice, MMRCurrentAuctionAveragePrice, MMRCurrentAuctionCleanPrice, MMRCurrentRetailAveragePrice, MMRCurrentRetailCleanPrice, IsOnlineSale, WarrantyCost))

#cleaning data
#Converting categorical value Transmission to Indicator Variables
transmission <- ifelse(Transmission == "AUTO", 0, 1)

#Converting categorical value Top 3 american brands to Indicator Variables
isFord <- ifelse(TopThreeAmericanName == "FORD", 1, 0)
isChrysler <- ifelse(TopThreeAmericanName == "CHRYSLER", 1, 0)
isGM <- ifelse(TopThreeAmericanName == "GM", 1, 0)


#min-max normalization function

#x, minx, maxx are the parameters passed to the function.
mmnorm <- function (x,minx,maxx){
  z<-((x-minx)/(maxx-minx));
  return (z)
}

#Normalization of the data set
normdata <- cbind.data.frame( refId = importedData$RefId,
                              isBadBuy = importedData$IsBadBuy,
                              vehAge = mmnorm(importedData[,3], min(importedData[,3], na.rm=TRUE), max(importedData[,3], na.rm=TRUE)),
                              trans = mmnorm(transmission, min(transmission), max(transmission)),
                              wheelTypeId = mmnorm(importedData[,5], min(importedData[,5], na.rm=TRUE), max(importedData[,5], na.rm=TRUE)),
                              vehOdometer = mmnorm(importedData[,6], min(importedData[,6], na.rm=TRUE), max(importedData[,6], na.rm=TRUE)),
                              isBrandFord = mmnorm(isFord, min(isFord), max(isFord)),
                              isBrandChrysler = mmnorm(isChrysler, min(isChrysler), max(isChrysler)),
                              isBrandGM = mmnorm(isGM, min(isGM), max(isGM)),
                              acqAucAvgPrice = mmnorm(importedData[,8], min(importedData[,8], na.rm=TRUE), max(importedData[,8], na.rm=TRUE)),
                              acqAucCleanPrice = mmnorm(importedData[,9], min(importedData[,9], na.rm=TRUE), max(importedData[,9], na.rm=TRUE)),
                              acqRetAvgPrice = mmnorm(importedData[,10], min(importedData[,10], na.rm=TRUE), max(importedData[,10], na.rm=TRUE)),
                              acqRetCleanPrice = mmnorm(importedData[,11], min(importedData[,11], na.rm=TRUE), max(importedData[,11], na.rm=TRUE)),
                              curAucAvgPrice = mmnorm(importedData[,12], min(importedData[,12], na.rm=TRUE), max(importedData[,12], na.rm=TRUE)),
                              curAucCleanPrice = mmnorm(importedData[,13], min(importedData[,13], na.rm=TRUE), max(importedData[,13], na.rm=TRUE)),
                              curRetAvgPrice = mmnorm(importedData[,14], min(importedData[,14], na.rm=TRUE), max(importedData[,14], na.rm=TRUE)),
                              curRetCleanPrice = mmnorm(importedData[,15], min(importedData[,15], na.rm=TRUE), max(importedData[,15], na.rm=TRUE)),
                              isOnlineSale = importedData$IsOnlineSale,
                              warrantyCost = mmnorm(importedData[,17], min(importedData[,17], na.rm=TRUE), max(importedData[,17], na.rm=TRUE)))


#Normalized data
View(normdata)


# 75% of the sample size
smp_size <- floor(0.75 * nrow(normdata))

#set the seed to make your partition reproductible
set.seed(123)
training_data <- sample(seq_len(nrow(normdata)), size = smp_size)

#dividing into training and test data sets
trainData <- normdata[training_data, ]
View(trainData)
testData <- normdata[-training_data, ] 
View(testData)

#install.packages("neuralnet")
#Library for Neural Nets.
library("neuralnet")

net <- neuralnet(IsBadBuy~ normdata$vehAge+normdata$trans+normdata$wheelTypeId+normdata$vehOdometer+normdata$isBrandFord+normdata$isBrandChrysler+normdata$isBrandGM+normdata$acqAucAvgPrice+normdata$acqAucCleanPrice+
                   normdata$acqRetAvgPrice+ normdata$acqRetCleanPrice+normdata$curAucAvgPrice+normdata$curAucCleanPrice +normdata$curRetAvgPrice+ normdata$curRetCleanPrice+normdata$isOnlineSale+normdata$warrantyCost,data=trainData, hidden=15, threshold=0.01, linear.output=FALSE, likelihood=TRUE)

#print the resutls
print(net)

#plot the graph
plot(net)


#compute the predicted value
predicted_value_NN <- compute(net,testData[,3:19])

#retrieve actual value
actual_value_NN <- testData$isBadBuy

print(predicted_value_NN$net.result)

#comparing actual vs predicted values
result <- data.frame(actual_value_NN, pred_value_NN = predicted_value_NN$net.result)

#frequency table
table(actual_value_NN, round(result$pred_value_NN))

#calculate error rate
mean(actual_value_NN != round(result$pred_value_NN)) * 100


