dataset <- read.csv('Malin-Dataset.csv')
colnames(dataset) <- c("Date", "Indicator", "Precipitation Amount", "Air Temperature", "Wet Bulb Temperature ",
                              "Dew Point Temperature", " Vapour Pressure", " Relative Humidity", "Mean Sea Level Pressure",
                              "Mean Wind Speed","Predominant Wind Direction", "Synop code for Present Weather", 
                              " Synop code for Past Weather", "Sunshine duration", "Visibility", "Cloud height", 
                              "Cloud amount")
Data <- dataset$`Mean Sea Level Pressure`
hist(Data,breaks= "sturges",main="Histogram: Sea Level Pressure",
     xlab="Pressure(Sea Level)")
library(ggplot2)
#install.packages('plyr',dependencies = TRUE)
#Histogram qplot
qplot(Data, geom = 'histogram', binwidth = 5, fill = I("darkblue"),col=I("red"),
      main ="Histogram: Sea Level Pressure" ,
      xlab = "Pressure(Sea Level)", ylab = "Frequency")
#Histogram ggplot
ggplot(data =dataset, aes(dataset$`Air Temperature`)) + 
  geom_histogram(col = "red", fill = "darkblue")+
  labs(title="Histogram: Sea Level Pressure") +
  labs(x= "Pressure(Sea Level)", y="Frequency" )



#scatter plot basic R function
WetBulbTemparature <- dataset$`Wet Bulb Temperature `
AirTemperature <- dataset$`Air Temperature`
plot(AirTemperature, WetBulbTemparature, main = "Scatter Plot", 
     xlab = "Air Temperature", ylab = "Wet Bulb Temparature", pch = 20)


#Scatter Plot
ggplot(dataset, 
       aes( x=AirTemperature, y=WetBulbTemparature)) +
  geom_point(size = 1.5,col = "blue") + labs(title = "Scatter Plot: Air temperature Vs Wet Bulb Temperature")
+ labs(x="AirTemperature", y= "WetBulbTemperature")

#Time series Plot-------------------------------------------------------
install.packages('xts')
install.packages('tidyverse')
library(tidyverse)
library(rlang)
library(utils)
library(xts)
library(ggplot2)
library(scales)
dataAppartment <- read.csv('Second hand Appartment Prices.csv',na.strings = "")
str(dataAppartment)
#as.Date(dataAppartment[,1])
colnames(dataAppartment) <- c("Year", "National", "Dublin", "Cork", "Galway", "Limerick", "Waterford", "Other Areas")
dataAppartment <- dataAppartment[-1,]

#dataAppartment <- xts(dataAppartment[,-1], order.by = as.Date(paste0(dataAppartment[,1],"-01-01")))
#temp <- data.frame(index(dataAppartment), stack(as.data.frame(as.numeric(gsub(",","",dataAppartment))))) 

tempAppartment <- gather(data = dataAppartment,"Areas", "Price", -Year)
tempAppartment$Year <- as.Date(paste0(dataAppartment[,1],"-01-01"))
tempAppartment$Price <- as.numeric(gsub(",","",tempAppartment$Price))

plot.ts(tempAppartment$Year,tempAppartment$Price, 
        plot.type = "multiple", xlab= "Years",
        ylab= "Prices", xy.labels = FALSE, xy.lines = TRUE, 
        main = "Basic Time series plot: Yearly price changes for various locations" )
          
#Basic time series plot
timeseries <- ts(dataAppartment[,2:8], start = c(1997,1), end = c(2015,4),  frequency = 1)
plot(timeseries, main = "Time Series Plot")

#Time series plot using ggplot
ggplot(data = tempAppartment, aes(x= Year, y= Price, color = Areas))+
   geom_line() + geom_point() +
  xlab("Year") + ylab("Price Variations") + 
  ggtitle("Enhanced Time series plot: Yearly price changes for various locations")

#Visual of the towns in Cork

datasetTowns <- read.csv('Cork-Towns-Datasets.csv')
str(datasetTowns)
datasetTowns[,2:3] <- scale(datasetTowns[,2:3])
datasetTowns$name <- as.character(datasetTowns$name)
range(datasetTowns$easting)
range(datasetTowns$northing)

plot(c(-3,3),c(-3,3),type="n",main = "Cork-Towns Plot" ,xlab="",ylab="")
for(i in 1:length(datasetTowns$name)){
  text(datasetTowns$easting[i],datasetTowns$northing[i],datasetTowns$name[i],cex=0.6)}

datasetTowns <- read.csv('Clare-Towns-Dataset.csv')
str(datasetTowns)
datasetTowns[,2:3] <- scale(datasetTowns[,2:3])
datasetTowns$name <- as.character(datasetTowns$name)
range(datasetTowns$easting)
range(datasetTowns$northing)

plot(c(-2,3),c(-2,3),type="n",main = "Clare-Towns(North/East coordinates Plot)" ,xlab="",ylab="")
for(i in 1:length(datasetTowns$name)){
  text(datasetTowns$easting[i],datasetTowns$northing[i],datasetTowns$name[i],cex=0.6)}
