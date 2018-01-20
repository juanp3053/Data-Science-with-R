#Lab 7 Juan P. Garces
cars <- read.csv("04cars.csv")
head(cars)


#Simple model: Predict Horsepower by Engine Size
plot(HP ~ EngineSize, data = cars, pch=19, col="blue")
hpModel <- lm(HP ~ EngineSize, data = cars)
summary(hpModel)
abline(hpModel, lwd=2)

#Intervals


#Interval for individual value
xVar <- data.frame(EngineSize=4)
singleConfInt <- predict(hpModel,xVar,interval = "confidence")
singleConfInt

singlePredInt <- predict(hpModel,xVar,interval = "prediction")
singlePredInt

#Plotting interval for data set
explanatory <- data.frame(EngineSize = cars$EngineSize)
hpConfInt <- predict(hpModel,explanatory,interval = "confidence")
str(hpConfInt)
hpConfInt[1,]
lines(cars$EngineSize,hpConfInt[,2],lty=3,lwd=2,col="darkgrey") #lower bound of confidence interval
lines(cars$EngineSize,hpConfInt[,3],lty=3,lwd=2,col="darkgrey") #upper bound of confidence interval

hpPredInt <- predict(hpModel,explanatory,interval = "prediction")
hpPredInt[1,]
lines(cars$EngineSize,hpPredInt[,2],col="lightblue") #lower bound of confidence interval
lines(cars$EngineSize,hpPredInt[,3],col="lightblue") #upper bound of confidence interval


#Examine for sport and non-sport
plot(HP ~ EngineSize, data = cars, pch=19)
points(cars$EngineSize,cars$HP,pch=19,col=((cars$sport==1)*1 + 1))

sportFilter <- cars$sport==1
hpModelSport <- lm(cars$HP[sportFilter] ~ cars$EngineSize[sportFilter])
hpModelNotSport <- lm(cars$HP[!sportFilter] ~ cars$EngineSize[!sportFilter])
abline(hpModelSport,lwd=2,col="red")
abline(hpModelNotSport ,lwd=2)
hpModelSport$coefficients
hpModelNotSport$coefficients

hpModelBoth <- lm(HP ~ EngineSize + sport, data = cars)
summary(hpModelBoth)
#********************   QUESTION 1 AND 2
#Sport 3.5 Liter Engine
hpModelBoth$coefficients[1] + (hpModelBoth$coefficients[2]*3.5) + (hpModelBoth$coefficients[3]*1)
#Non-Sport 4.0 Liter Engine
hpModelBoth$coefficients[1] + (hpModelBoth$coefficients[2]*4.0) + (hpModelBoth$coefficients[3]*0)
#*********************

#Simple model: Predict MPG by Engine Size
plot(City.MPG ~ EngineSize, data = cars, pch=19, col="blue")
mpgModel <- lm(City.MPG ~ EngineSize, data = cars)
summary(mpgModel)
abline(mpgModel, lwd=2)

#Examine for RWD and non-RWD
plot(City.MPG ~ EngineSize, data = cars, pch=19)
points(cars$EngineSize,cars$City.MPG,pch=19,col=((cars$RWD==1)*1 + 1))

rwdFilter <- cars$RWD==1
mpgModelRWD <- lm(cars$City.MPG[rwdFilter] ~ cars$EngineSize[rwdFilter])
mpgModelNotRWD <- lm(cars$City.MPG[!rwdFilter] ~ cars$EngineSize[!rwdFilter])
abline(mpgModelRWD,lwd=2,col="red")
abline(mpgModelNotRWD ,lwd=2)
mpgModelRWD$coefficients
mpgModelNotRWD$coefficients

mpgModelBoth <- lm(City.MPG ~ EngineSize + RWD + EngineSize*RWD, data = cars)
summary(mpgModelBoth)
#********************* Questions c(3:5)
#MPG RWD 2.0 Liter
mpgModelBoth$coefficients[1] + (mpgModelBoth$coefficients[2]*2.0) + (mpgModelBoth$coefficients[3]*1) + (mpgModelBoth$coefficients[4]*2.0*1)
#MPG NON-RWD 2.5 Liter
mpgModelBoth$coefficients[1] + (mpgModelBoth$coefficients[2]*2.5) + (mpgModelBoth$coefficients[3]*0) + (mpgModelBoth$coefficients[4]*2.5*0)
#MPG NON-RWD 4.0 Liter
mpgModelBoth$coefficients[1] + (mpgModelBoth$coefficients[2]*4.0) + (mpgModelBoth$coefficients[3]*0) + (mpgModelBoth$coefficients[4]*4.0*0)
