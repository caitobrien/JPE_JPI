ByWeek
#arrange(ByWeek,-wk)
currentwk<-c(ByWeek$wk[nrow(ByWeek)])
ByWeek<-ByWeek[ByWeek$wk==currentwk, ]
ByWeek

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

  NumDays<- nrow(ByWeek)
  NumDays
  
 Week<- unique(ByWeek$wk)         #get the week number
  Week
  Month <- unique(ByWeek$month)
  Month
  Year<- unique(ByWeek$year)      #get the year number
  Year
  # Make a dataframe with the sampled days only
  sampled <- subset(ByWeek,pd !='',select=c(xd,pd,td))

sampled
# Count the days sampled only (I could have used length to count the sampled days)
daysSamp<-nrow(sampled)
daysSamp
NonSampDays <- NumDays-daysSamp
NonSampDays

 # Estimate and add the varPD column to the dataframe 
ByWeek$varPD <- with(ByWeek, (pd * (1 - td))/td + msquare * ((pd*(1-td)+ pd^2*td))/td^3)
# Convert the NA's to "0" to calculate the weekly covariance.
ByWeek <- data.frame(lapply(ByWeek,function(x) replace(x,is.na(x),0))) 
 ByWeek

xd<- ByWeek$xd
pd<- ByWeek$pd
td<- ByWeek$td
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
 WCovariance <- do.call(rbind, CovSummary)
 # add the covariance
 WCovariance <- cbind(WCovariance, WeekCov=WCovariance[, 'xd'] * WCovariance[, 'pd'] / WCovariance[, 'td'])
 WCovariance
SummedCovPiPj<-sum(WCovariance[, 'WeekCov'],na.rm=T)
 SummedCovPiPj
 pdmean<- sum(ByWeek$pd)/daysSamp   # Mean weekly passage
 #pdmean<- mean(ByWeek$pd)       # Mean weekly passage
 pdmean
 Weekvariance<- var(ByWeek$pd)  #Weekly pd variance
 Weekvariance
 SummedVarPD<-sum(ByWeek$varPD)  #Summed variance of pd
 SummedVarPD

  TotalPD<- pdmean*NumDays    # Mean based on number of days of the week
  TotalPD
  varTotalPD<-((1-daysSamp/NumDays)*NumDays^2/daysSamp*Weekvariance) + ((NumDays/daysSamp)*(SummedVarPD + 2*SummedCovPiPj))
  varTotalPD
  T<-qt((1-0.10/2),NumDays)  # Inverse t-distribution for 90% confidence intervals
  T
  PlusMin<-T*varTotalPD^.5
  PlusMin
  Lower<-TotalPD-PlusMin   # Lower Confidence
  Lower
  Upper<- TotalPD+PlusMin  # Upper Confidence
  Upper
Final<- data.frame(Year,Week,NumDays,daysSamp,NonSampDays,pdmean,Weekvariance,SummedVarPD,SummedCovPiPj,TotalPD,varTotalPD,T,PlusMin,Lower,Upper)
 # Replace NA's with 0
Final <- data.frame(lapply(Final,function(x) replace(x,is.na(x),0)))

Final
FinalData<- data.frame(Year,Week,daysSamp,NonSampDays,Lower,TotalPD,Upper,varTotalPD)
FinalData <- data.frame(lapply(FinalData,function(x) replace(x,is.na(x),0)))
FinalData

### EXTRACT THE COEFFICIENTS (slope and intercept) ALONG WITH THE R-SQUARED AND P-VALUE
Trials <- nrow(Reg) ; Trials  # Number of trials
 
mstMODEL <-
function(lm)
{
out <- c(cbind(Year),lm$coefficients[2],
	lm$coefficients[1],
	cbind(Trials),
	pf(summary(lm)$fstatistic[1], summary(lm)$fstatistic[2],
summary(lm)$fstatistic[3], lower.tail = FALSE),
	summary(lm)$r.squared)
#names(out) <- c("intercept","slope","Trials","slope.SE","p.value","r.squared")
 names(out) <- c("Year","slope","intercept","Trials","p.value","r.squared")
 return(out)
}#} 

model_values <- format(t(mstMODEL(SEValues)),scientific=FALSE,digits=6) 
model_values


