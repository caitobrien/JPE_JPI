


  #Reg <- read.table("C:/Documents and Settings/Owner/Desktop/Tilapia Farming/RegressionModel.txt",header=T,sep="\t")
 SEValues <- lm(Efficiency~PercQ, data = Reg) # Regression equation values and std Error
#summary(SEValues)
MSValue<-anova(lm(Efficiency~PercQ,data=Reg))    # Mean-Square value
MSValue
 #par(mfrow = c(2, 2))
#plot(SEValues)
#plot(Efficiency~PercQ,data=Reg)
# abline(col="red",lty=3,lm(Efficiency~PercQ,data=Reg))
 #Extract the Std. Error values
#i<- predict(SEValues)
 #lines(i)
# fitted(SEValues)
Beta <- coef(summary(SEValues))
Beta
StdE=Beta[,"Std. Error"]
StdE
# Intercept or Upper Std. Error value
StdE[1]
varA <-StdE[1]^2
varA
#  Variable(PercQ) or lower Std. Error value
StdE[2]
varB <- StdE[2]^2
varB
# Extract the Mean sq after estimating ANOVA
msquare<- MSValue$"Mean Sq"[2]
msquare
# Estimate the Mean Standard Error of Percent Q
 meanQ <- mean(Reg$PercQ)
 meanQ
 n<- nrow(Reg)
 n
 variance<- var(Reg$PercQ)
 variance
 covAB<-msquare*(-meanQ)/(var(Reg$PercQ)*n-1)
 covAB
# Import by week and bind the varPD column with the variance of the daily passage estimates
 # ByMonth<- read.csv("C:/Documents and Settings/fcarrillo/Desktop/CI all Runs Passage Data/Monthly.csv")
 #ByMonth <- read.table("C:/Documents and Settings/Owner/Desktop/CI All Runs Passage Data/Monthly.txt",header=T,sep="\t")
  #ByMonth
  #ByMonth <- subset(ByMonth,month==1)
  #ByMonth
  
  #Count the length of the month(28,29,30 or 31 days)
  NumDays<- nrow(ByMonth)
  NumDays
  # Omit the NA's to count the days sampled only
 #daysSamp<- na.omit(ByWeek)
 #daysSamp<- nrow(daysSamp)
 #daysSamp
 
 Year<- unique(ByMonth$year)      #get the year number
  Year
   Month <- unique(ByMonth$month)
   Month
  # Make a dataframe with the sampled days only
sampled <- subset(ByMonth,xd>=0,select=c(xd,pd,td))
sampled
# Count the days sampled only (I could have used length to count the sampled days)
daysSamp<-nrow(sampled)
daysSamp
# Missed days
NonSampDays <- NumDays-daysSamp
NonSampDays
#closeAllConnections()
 # Estimate and add the varPD column to the dataframe
ByMonth$varPD <- with(ByMonth, (pd * (1 - td))/td + msquare * ((pd*(1-td)+ pd^2*td))/td^3)
# Convert the NA's to "0" to calculate the weekly covariance.
ByMonth <- data.frame(lapply(ByMonth,function(x) replace(x,is.na(x),0)))
 ByMonth
#Subset the ByWeek dataset to extract xd,td,pd and transpose it to estimate daily covariance
#X<- subset(ByWeek,select=c(xd,pd,td))
# use the 'signif' function to apply scientific decimals to the values(or 'round' to limit the decimals)
xd<- ByMonth$xd
pd<- ByMonth$pd
td<- ByMonth$td
mydf<- data.frame(xd,pd,td)
mydf
transData <- t(mydf)
transData
 #if (ncol(transData) == 1){
    #CovSummary <- cbind(xd=xd, pd=pd, td=td)
#}else {
 Csequence <- lapply(seq(ncol(transData) - 1), function(x) x:(ncol(transData) - 1))
 CovSummary <- lapply(Csequence, function(.col){
# compute the 3 columns of data
+ cbind(xd=varA + transData[1, .col[1]] * covAB + transData[1, .col + 1] *
covAB + transData[1, .col[1]] * transData[1, .col + 1] * varB,
pd=transData[2, .col[1]] * transData[2, .col + 1],td=transData[3, .col[1]] * transData[3, .col + 1])
 })
 #}
  CovSummary
 # rbind for the output
 MCovariance <- do.call(rbind, CovSummary)
 MCovariance
 # add the covariance
 makeDF <- cbind(MCovariance, MonthCov=MCovariance[, 'xd'] * MCovariance[, 'pd'] / MCovariance[, 'td'])
 makeDF
SummedCovPiPj <- sum(makeDF[,'MonthCov'],na.rm=T)
 SummedCovPiPj
 pdmean<- sum(ByMonth$pd)/daysSamp   # Mean weekly passage
 #pdmean<- mean(ByWeek$pd)       # Mean weekly passage
 pdmean
 Monthvariance<- var(ByMonth$pd)  #Weekly pd variance
 Monthvariance
 SummedVarPD<-sum(ByMonth$varPD)  #Summed variance of pd
 SummedVarPD

  TotalPD<- pdmean*NumDays    # Mean based on number of days of the week
  TotalPD
  varTotalPD<-((1-daysSamp/NumDays)*NumDays^2/daysSamp*Monthvariance) + ((NumDays/daysSamp)*(SummedVarPD + 2*SummedCovPiPj))
  varTotalPD
  T<-qt((1-0.10/2),NumDays)  # Inverse t-distribution for 90% confidence intervals
  T
  PlusMin<-T*varTotalPD^.5
  PlusMin
  Lower<-TotalPD-PlusMin   # Lower Confidence
  Lower
  Upper<- TotalPD+PlusMin  # Upper Confidence
  Upper
Final<- data.frame(Year,Month,daysSamp,pdmean,MonthVar=Monthvariance,SummedVarPD,SummedCovPiPj,NumDays,TotalPD,varTotalPD,T,PlusMin,Lower,Upper)
 # Replace NA's with 0
Final <- data.frame(lapply(Final,function(x) replace(x,is.na(x),0)))
# Another way for replacing NA's
 #ByWeek <- data.frame(lapply(ByWeek_NA,function(x){x[is.na(x)] <- 0 ; x}))
Final
FinalData<- data.frame(Year,Month,daysSamp,NonSampDays,Lower,TotalPD,Upper,varTotalPD)
FinalData <- data.frame(lapply(FinalData,function(x) replace(x,is.na(x),0)))
FinalData
 # Export to Excel
#write.table(FinalData, file = "C:/Documents and Settings/fcarrillo/Desktop/WeeklyData.csv", sep = ",", col.names = NA,qmethod = "double")
#write.table(FinalData, file = "C:/Documents and Settings/fcarrillo/Desktop/WCovariance.xls",sep="\t")


