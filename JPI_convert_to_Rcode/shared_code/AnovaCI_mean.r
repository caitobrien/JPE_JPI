
ByWeek
#arrange(ByWeek,-wk)
currentwk<-c(ByWeek$wk[nrow(ByWeek)])
ByWeek<-ByWeek[ByWeek$wk==currentwk, ]
ByWeek

#ByWeek <- data.frame(lapply(ByWeek,function(x) replace(x,is.na(x),0)))

SEValues <- lm(Efficiency~PercQ, data = Reg) # Regression equation values and std Error

MSValue<-anova(lm(Efficiency~PercQ,data=Reg))    # Mean-Square value
MSValue

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


   thismonth <- unique(ByWeek$month);thismonth  #OLD CODE--Doesn't work when week has >1 value
###thismonth <-ByWeek[4,6];thismonth
  ByMonth <- subset(ByMonth2,month==thismonth)
  ByMonth
  
  #Count the length of the month(28,29,30 or 31 days)
NumDays<- nrow(ByMonth)
  NumDays
  
  Year<- unique(ByMonth$year)      #get the year number
  Year
   Month <- unique(ByMonth$month)
   Month
   Week <- unique(ByWeek$wk)
  Week
   
  # Make a dataframe with the sampled days only
sampled <- subset(ByWeek,xd>=0,select=c(xd,pd,td)) 
sampled <- subset(ByMonth,xd>=0,select=c(xd,pd,td))
sampled

# Count the days sampled only (I could have used length to count the sampled days)
daysSamp <- nrow(sampled) 
daysSamp
# Missed days
NonSampDays <- NumDays-daysSamp
NonSampDays

 # Estimate and add the varPD column to the dataframe
ByMonth$varPD <- with(ByMonth, (pd * (1 - td))/td + msquare * ((pd*(1-td)+ pd^2*td))/td^3)
# Convert the NA's to "0" to calculate the weekly covariance.
ByMonth <- data.frame(lapply(ByMonth,function(x) replace(x,is.na(x),0)))
 ByMonth
#Subset the ByWeek dataset to extract xd,td,pd and transpose it to estimate daily covariance
# use the 'signif' function to apply scientific decimals to the values(or 'round' to limit the decimals)
xd<- ByMonth$xd
pd<- ByMonth$pd
td<- ByMonth$td
mydf<- data.frame(xd,pd,td)
mydf
transData <- t(mydf)
transData
 
 Csequence <- lapply(seq(ncol(transData) - 1), function(x) x:(ncol(transData) - 1))
 CovSummary <- lapply(Csequence, function(.col){
# compute the 3 columns of data
+ cbind(xd=varA + transData[1, .col[1]] * covAB + transData[1, .col + 1] *
covAB + transData[1, .col[1]] * transData[1, .col + 1] * varB,
pd=transData[2, .col[1]] * transData[2, .col + 1],td=transData[3, .col[1]] * transData[3, .col + 1])
 })
 
  CovSummary
 # rbind for the output
 MCovariance <- do.call(rbind, CovSummary)
 MCovariance
 # add the covariance
 makeDF <- cbind(MCovariance, MonthCov=MCovariance[, 'xd'] * MCovariance[, 'pd'] / MCovariance[, 'td'])
 makeDF
SummedCovPiPj <- sum(makeDF[,'MonthCov'],na.rm=T)
 SummedCovPiPj
 pdmean<- sum(ByMonth$pd)/daysSamp
 pdmean<-pdmean*7
 # Mean weekly passage
 #pdmean<- mean(ByWeek$pd)       # Mean weekly passage
 pdmean
 Monthvariance<- var(ByMonth$pd)  #Weekly pd variance
 Monthvariance
 SummedVarPD<-sum(ByMonth$varPD)  #Summed variance of pd
 SummedVarPD

   varpdmean<-((1-daysSamp/NumDays)*NumDays^2/daysSamp*Monthvariance) + ((NumDays/daysSamp)*(SummedVarPD + 2*SummedCovPiPj))
  varpdmean
  
  T<-qt((1-0.10/2),NumDays)  # Inverse t-distribution for 90% confidence intervals
  T
     PlusMin<-T*varpdmean^.5
  PlusMin

  Lower<-pdmean-PlusMin   # Lower Confidence
  Lower
  Upper<- pdmean+PlusMin  # Upper Confidence
  Upper

 Final<- data.frame(Year,Month,daysSamp,pdmean,MonthVar=Monthvariance,SummedVarPD,SummedCovPiPj,NumDays,varpdmean,T,PlusMin,Lower,Upper)
 # Replace NA's with 0
Final <- data.frame(lapply(Final,function(x) replace(x,is.na(x),0)))
Final

FinalData<- data.frame(Year,Week,daysSamp,NonSampDays,Lower,pdmean,Upper,varpdmean)
FinalData <- data.frame(lapply(FinalData,function(x) replace(x,is.na(x),0)))
FinalData

 
#*****************************************************************************************************************************
  
### EXTRACT THE COEFFICIENTS (slope and intercept) ALONG WITH THE R-SQUARED AND P-VALUE
Trials <- nrow(Reg);Trials  # Number of trials
  # to extract these from a list of lm models, better to write a function
# (you can modify this accordingly):
mstMODEL <-function(lm){
out <- c(cbind(Year),lm$coefficients[2],
	lm$coefficients[1],
	#length(lm$model$y),
	cbind(Trials),
	#summary(lm)$coefficients[2,2],
	pf(summary(lm)$fstatistic[1], summary(lm)$fstatistic[2],
summary(lm)$fstatistic[3], lower.tail = FALSE),
	summary(lm)$r.squared)
#names(out) <- c("intercept","slope","Trials","slope.SE","p.value","r.squared")
 names(out) <- c("Year","slope","intercept","Trials","p.value","r.squared")
return(out)
}

# now let's see ...transpose the output so it can be horizontal for excel
model_values <- format(t(mstMODEL(SEValues)),scientific=FALSE,digits=6) 
model_values
#coefplot(SEValues)
#*********************************************************************************************************************************  
