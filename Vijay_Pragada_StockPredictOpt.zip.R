
rm(list=ls(all=TRUE))

##set working directory.
setwd("C:/Users/bh/Desktop/")

## load required libraries.
library(quantmod)
library(xlsx)
library(tseries)
library(forecast)
library(lubridate)
library(dummies)
library(xts)

#Download Data from MARCH 2007.
from.dat <- as.Date("03/01/2007", format="%m/%d/%y") 
getSymbols("BHARTIARTL.NS", src="yahoo", from = from.dat, frequency = 5)

#Convert it into data frame
airtel <- as.data.frame(BHARTIARTL.NS)

#Create the xts for Ajusted price
stock_data <- xts(airtel$BHARTIARTL.NS.Adjusted, frequency = 5, order.by = index(BHARTIARTL.NS))
plot.xts(stock_data)

#Set the Frequency to 5 as it has only weekday data
attr(stock_data,'frequency') <- 5
frequency(stock_data)

#Split the data into train and test
train <- window(stock_data, end = '2017-02-17')
test <- window(stock_data, start = '2017-02-17')

#Decompose to see seasonal, trend and random component
stock_data_ts = as.ts(stock_data)
frequency(stock_data_ts)

stock_data_timeseriescomponents <-
  decompose(stock_data_ts, type = c("additive","multiplicative"))

plot(stock_data_timeseriescomponents)
stock_data_timeseriescomponents$seasonal
stock_data_timeseriescomponents$trend
stock_data_timeseriescomponents$figure

#Observe the ACF and PACF 
par(mfrow=c(1,3))
plot.xts(stock_data)
acf(stock_data, lag.max=20)
pacf(stock_data, lag.max=20)

#Identify the number of differences
ndiffs(stock_data_ts)
nsdiffs(stock_data_ts)

#Observe the ACF and PACF after differencing
stock_data_timeseriesdiff1 <- diff(stock_data, differences=1)
stock_data_timeseriesdiff1
sum(is.na(stock_data_timeseriesdiff1))
stock_data_timeseriesdiff1[is.na(stock_data_timeseriesdiff1)] <- 0
acf(stock_data_timeseriesdiff1, lag.max=20)
pacf(stock_data_timeseriesdiff1, lag.max=20)


########### HOLT WINTERS ####################

#Holt-Winters
#Additive, trend and seasonality models
stock_data_HW1 <- 
  HoltWinters(train)
stock_data_HW1
stock_data_HW1$fitted

par(mfrow=c(1,1))
plot(stock_data_HW1)
stock_data_HW1$SSE
stock_data_residualsHW1 <- residuals(stock_data_HW1)
stock_data_residualsHW1
plot(stock_data_residualsHW1)
par(mfrow = c(1,2))
acf(stock_data_residualsHW1)
pacf(stock_data_residualsHW1)

#it predicts seasonal peaks well
stock_data_forecastHW1 <- 
  forecast.HoltWinters(stock_data_HW1, 
                       h=5)

stock_data_forecastHW1$mean
par(mfrow = c(1, 1))
plot.forecast(stock_data_forecastHW1,
              shadecols="oldstyle")

accuracy(stock_data_forecastHW1$mean,test)

#################### ARIMA ######################

#ARIMA
#Step-by-step ARIMA model building
# Model 1
# Step 1: Plot timeseries (in terms of ARIMA, it is an ARIMA(0,0,0))
par(mfrow = c(1, 1))
plot(train)

# Step 2: Plot ACF and PACF to get preliminary understanding of the process
par(mfrow = c(1, 2))
acf(train)
pacf(train)

# Step 3: Differencing the as Arima(0,1,0) as error term was having a pattern
par(mfrow = c(1, 1))
stock_data_timeseriesdiff1 <- diff(train, differences = 1)
stock_data_timeseriesdiff1
plot(stock_data_timeseriesdiff1)

# Step 5: Check ACF and PACF to explore remaining dependencies
par(mfrow = c(1, 2))
stock_data_timeseriesdiff1[is.na(stock_data_timeseriesdiff1)] <- 0
acf(stock_data_timeseriesdiff1)
pacf(stock_data_timeseriesdiff1)


# Step 6: ACF show significant lag, requiring an AR(1) 
stock_data_Arima1 <- Arima(train, order = c(0,1,1),
                           include.drift = FALSE)
stock_data_Arima1
accuracy(stock_data_Arima1)

# Step 7: Check residuals to ensure they are white noise
par(mfrow = c(1, 2))
acf(stock_data_Arima1$residuals, lag.max = 24)
pacf(stock_data_Arima1$residuals, lag.max = 24)
Box.test(stock_data_Arima1$residuals, lag=24, type="Ljung-Box")

# Step 8: Start forecasting
par(mfrow = c(1, 1))
stock_data_timeseriesforecastsArima1 <- forecast.Arima(stock_data_Arima1,
                                                       h=5)
plot.forecast(stock_data_timeseriesforecastsArima1)

stock_data_timeseriesforecastsArima1

# Step 9: Check the accuracy of the model
accuracy(stock_data_timeseriesforecastsArima1,test)

########### AUTO ARIMA ############

# Automated functions are available
stock_data_AutoArima <- auto.arima(train,ic='aic')
stock_data_AutoArima
stock_data_timeseriesforecastsAutoArima <- forecast.Arima(stock_data_AutoArima, h=5)
plot.forecast(stock_data_timeseriesforecastsAutoArima)
stock_data_timeseriesforecastsAutoArima
accuracy(stock_data_timeseriesforecastsAutoArima,test)


############################################################################################################
              ##########################      OPTIMIZATION          ##########################

                      #################### GENETIC ALGORITHM METHOD #################


rm(list=ls(all=TRUE))

library(lubridate)
library(xts)
CAG<-function(x){
  #Path of Stocks input files
  dirpath = "C:/Users/bh/Desktop/VJ/"
  #get list of files from input directory
  files<-list.files(dirpath,full.names=T)
  stocksreturn<-list(files)
  #go through each input stock file and predict stock price for 3 weeks
  for (file in files){
    #file<-("C:/Users/bh/Desktop/VJ/SBIN.NS.csv")
    Data<-read.csv(file)
    stockdf<-subset(Data,select = c(Date,Adj.Close))
    stockdf$Date<-as.Date(stockdf$Date,"%Y-%m-%d")
    stocksts<-xts(stockdf$Adj.Close,order.by = stockdf$Date)
    CAGPeriod<-stocksts["2012-03-01/2017-02-3"]
    #CAG =  [(end value/start value)^(1/no of years)] - 1
    endValve<-as.numeric(CAGPeriod["2017-02-3"])
    startValue<-as.numeric(CAGPeriod["2012-03-01"])
    yrs<-5  #5 years hist data used
    cag = ((endValve/startValue)^(1/yrs))-1
    StockName = gsub("(.*)\\/(.*)\\.(.*)","\\2",file) #to get file name of stocks
    stocksreturn[StockName]<-cag
    #Compound Annual Growth rate (CAG) =  [(end value/start value)^(1/no of years)] - 1
    
  }
  return(stocksreturn)
}

stocksreturn<-CAG()
top5Stocks <- unlist(stocksreturn[order(unlist(stocksreturn), decreasing=TRUE)][1:5])
top5Stocks
dataSet <- data.frame(stocks = names(top5Stocks),returns = round(top5Stocks,digits = 4))
dataSet

amount = 150000
stoppingCriteria = 8000
weightLimit = 1

# stopping criteria could be min returns or 
# if there is no much change for last n iteerations you may stop

Chromosome = c(0.16, 0.16, 0.17, 0.17, 0.17, 0.17, 0.17)

# Initital population generation
fnGenerateInitPop = function(dataSet){
  
  InitPopSize = 100
  
  initPop = as.data.frame(setNames(replicate(nrow(dataSet),
                                             numeric(0), 
                                             simplify = F), 
                                   dataSet$stock))
  
  set.seed(1234)
  
  for (i in 1:InitPopSize){
    
    chromosome = diff(c(0, sort(runif(5)), 1))
    initPop[i,]= chromosome
  }
  
  return(initPop)
}

#We define the Objective function as follows.

fnEvaluate = function(x){
  
  individualInvestments = (x * rep(amount, length(x)))
  
  totalReturns = sum(dataSet$returns * individualInvestments)
  
  if (sum(x) > weightLimit) 
    return(0) 
  else 
    return(totalReturns)
  
}

# mutation : pick one position and change the value to 0 or 1 as the case may be

fnMutate = function(individual){
  #change one value to the other value 1 to 0 or 0 to 1
  a = sample(1:length(individual),2)
  i = a[1]
  j = a[2]
  delta = min(individual)/2
  individual[i]=individual[i]-delta
  individual[j]=individual[i]+delta
  
  return(individual)
}

# Crossover : randomly select a point and swap the tails

fnCrossOver = function(p1, p2){
  a = sample(2:(length(p1)-2), 1)
  p11 = c(p1[1:a], p2[(a+1):length(p2)])
  p12 = c(p2[1:a], p1[(a+1):length(p1)])
  
  return(list(p11,p12))
}

# Execute the genetic algorithm
fnRunGeneticAlgo= function(initPop, mutstartProb,
                           elitepercent, maxiterations,
                           MaxPossiblesurvivalpoints){
  
  counter=0   # is used for stopping criteria
  
  cat("max iterations =", maxiterations, "\n")
  # How many winners from each generation?
  
  origPopSize = nrow(initPop)
  topElite = round(elitepercent*origPopSize, 0)
  fitN = apply(initPop, 1, fnEvaluate)
  
  initPop = data.frame(initPop, fitN)
  initPop = initPop[order(-initPop$fitN),]
  
  # Main loop
  NewPop = initPop
  bestretind = NewPop
  bestretind<-bestretind[-c(1:nrow(bestretind)),]
  
  for (i in 1:maxiterations) {
    cat("i value is :", i, "\n")
    NewPop = NewPop[order(-NewPop$fitN),]
    ElitePop = NewPop[1:topElite,]
    
    NewPop = NewPop[, -c(length(colnames(NewPop)))]
    NewPop = NewPop[-(1:nrow(NewPop)), 1:nrow(dataSet)]
    mut = mutstartProb/i
    
    # Add mutated and bred forms of the winners
    while (nrow(NewPop) < origPopSize) {
      
      # Mutation
      if (runif(1,0,1) < mut) {
        c = sample(1:topElite, 1)
        NewPop[nrow(NewPop)+1,] = fnMutate(ElitePop[c, 1:nrow(dataSet)])
        if (nrow(NewPop)==origPopSize){break()}
      }
      
      # Crossover
      else {
        c1 = sample(1:topElite,1)
        c2 = sample(1:topElite,1)
        ls = (fnCrossOver(ElitePop[c1,1:nrow(dataSet)], 
                          ElitePop[c2,1:nrow(dataSet)]))
        NewPop[nrow(NewPop)+1,]=ls[[1]]
        NewPop[nrow(NewPop)+1,]=ls[[2]]
        if (nrow(NewPop)==origPopSize){break()}
      }
      
    }
    NewPop$fitN = apply(NewPop, 1, fnEvaluate)
    NewPop = NewPop[order(-NewPop$fitN),]
    
    # stopping criteria 
    if(NewPop[1, (nrow(dataSet)+1)] >= ElitePop[1, (nrow(dataSet)+1)]){
      if(is.na(bestretind$fitN)){
        bestretind <- NewPop[1,]
      }else if(bestretind$fitN < NewPop[1, (nrow(dataSet)+1)]){
        bestretind <- NewPop[1,]    
      }
      #counter = counter+1
      #if(counter==5){break()}
    }else{
      bestretind <- ElitePop[1,]
    }
    #if (NewPop[1,(nrow(dataSet)+1)] == MaxPossiblesurvivalpoints) {break()}
    cat("best returns in iteration", i, 
        " = ", NewPop[1,(nrow(dataSet)+1)], "\n")
    
  }
  # Print current best score
  
  cat("Max returns in all iteration", " = ", 
      bestretind[1,(nrow(dataSet)+1)], "\n")
  
  return(NewPop[1,])
}


fnExecuteMain = function(dataSet, mutstartProb,
                         elitepercent, maxiterations, 
                         MaxPossiblesurvivalpoints){
  
  initPop = fnGenerateInitPop(dataSet)
  
  solution = fnRunGeneticAlgo(initPop, mutstartProb, 
                              elitepercent, maxiterations,
                              MaxPossiblesurvivalpoints)
  
  Finalsolution = as.numeric(solution[1, 1:nrow(dataSet)])
  
  Finalsolutiondf<-dataSet
  Finalsolutiondf<-cbind(dataSet,Finalsolution)
  
  #cat("optimal investment in Stocks in percentage = ",(Finalsolution*100))
  #cat("Total retuns = ",solution[,"fitN"])
  #print(Finalsolutiondf)
  
  return(Finalsolutiondf)
}

mutstartProb=0.5
elitepercent=0.2
maxiterations=10

Result = fnExecuteMain(dataSet, mutstartProb=0.5, 
                       elitepercent=0.2, maxiterations=10)
Result

#############################################################################################################

    #####################              SIMULATED ANNEALING METHOD       ############################


stoppingCriteria = 8000
weightLimit = 1

1/5 #initial solution assumed for 5 stocks

fnInitialSolu <- function(){
  
  # Let us initiate solution with almost equal weights to make it total 1
  initialSolu = c(0.2,0.2,0.2,0.2,0.2) 
  
  return(initialSolu)
}

# Evaluation function.
fnEvaluate = function(dataSet, weightLimit, solution, amount) {
  
  individualInvestments = (solution * rep(amount, length(solution)))
  
  totalReturns = dataSet$returns %*% individualInvestments               
  
  if (round(sum(solution), 1) != weightLimit) 
    return(0) 
  else 
    return(totalReturns)
}


# Purterbation function: randomly select a point and do operation
fnPurterb = function(solution){
  
  idx = sample(1:length(solution), 2)
  
  purtSolution = solution
  
  x = solution[idx[1]]
  y = solution[idx[2]]
  
  # Taking the diff of 1st stock weight and 0.03 and take half of it 
  # Subtract that value and the same value to the secnd stock weight 
  # (As total should be 1)
  
  diff = ((solution[idx[1]] - 0.03)/2)
  purtSolution[idx[1]] = solution[idx[1]] - diff
  purtSolution[idx[2]] = solution[idx[2]] + diff
  
  return(purtSolution)
}



fnRunSimulatedAnnealingAlgo = function(maxIterations, amount){
  
  cat("Max iterations =", maxIterations, "\n")
  
  # Generate a random solution
  initialSolu = fnInitialSolu()
  
  initialVal = fnEvaluate(dataSet, weightLimit, initialSolu, amount)
  
  baseSolu = initialSolu
  baseVal = initialVal
  counter = 0
  
  # solution vs available
  cat(paste("Baseval initially is : ", baseVal, "\n"))
  
  for (i in 1:maxIterations) {
    
    # Purterbation
    nextSolu = fnPurterb(baseSolu)
    nextVal = fnEvaluate(dataSet, weightLimit, nextSolu, amount)
    
    if(any(nextSolu > 0.03) == FALSE){
      return(0)
    }
    else{
      counter = counter + 1
      
      if(nextVal > baseVal){
        baseSolu = nextSolu
        baseVal = nextVal
      }
      else{
        # Accept with acceptence probability
        acceptanceProb = runif(1, 0, 1)
        if(acceptanceProb > 0.5){
          baseSolu = nextSolu
          baseVal = nextVal
        }
      }
    }
    if (baseVal >= stoppingCriteria){break()}     
    i = counter
    
    # solution 
    cat("Returns in ", i, "iteration is : ", baseVal,"\n")
  }
  return(list(baseSolu, baseVal)) 
}



fnExecuteMain = function(dataSet, maxIterations, amount){
  
  set.seed(1234)
  
  solutionList = fnRunSimulatedAnnealingAlgo(maxIterations, amount)
  
  finalSolution = as.numeric(solutionList[[1]])
  finalSolutionValue = solutionList[[2]]
  
  dataSet$finalSolution = finalSolution
  
  cat("Total returns = ", finalSolutionValue,"\n")
  return(dataSet)
}

result = fnExecuteMain(dataSet, maxIterations = 4000, amount = 150000)
result


########################################         END       ####################################################


