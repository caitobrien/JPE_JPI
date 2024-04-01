

# Reg <- read.csv("C:/Documents and Settings/fcarrillo/Desktop/Software/R Scripts and Datasets/RegressionModel.csv",header=TRUE)
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
  #ByYear<- read.csv("C:/Documents and Settings/fcarrillo/Desktop/CI all Runs passage Data/Monthly.csv")
# ByYear<- read.table("C:/Documents and Settings/fcarrillo/Desktop/CI all Runs Passage Data/Monthly.txt",header=T,sep="\t")
  ByYear
   ByYear <- data.frame(wk=ByYear$wk,year=ByYear$year,xd=ByYear$xd,td=ByYear$td,pd=ByYear$pd) 
  ByYear <- data.frame(wk,year,xd,td,pd)
  #ByYear
  #closeAllConnections()
   # Sort the data by Week
  ByYear <- ByYear[order(ByYear$wk),]
  #ByYear
  
  # Estimate the Daily passage variation and attach it to dataframe
ByYear$varPD <- with(ByYear, (pd * (1 - td))/td + msquare * ((pd*(1-td)+ pd^2*td))/td^3)
 #ByYear

 #Count Total sampled days during current year
  ByYear2 <- na.omit(ByYear)
  #ByYear2
  SampledDays <- nrow(ByYear2)
  SampledDays
  
     # Get the year
year <- unique(ByYear2$year)
year
  # Sum the every day VarPD for the sampled days
  SummedVarPD <- sum(ByYear2$varPD)
 SummedVarPD
 
  # Get the yearly variance of pd
  YearlyVar <- var(ByYear2$pd)
  YearlyVar

 # Yearly passage data mean
  Yearlymean <- mean(ByYear2$pd)
  Yearlymean
  
  # Count days of the year
  YearDays <- year
 countDays <- as.Date(as.yearmon(YearDays)+1) - as.Date(as.yearmon(YearDays))
YearDays <- as.double(countDays, units = "days")
YearDays

#YearDays <- as.integer(as.Date("2007-06-30") - as.Date("2006-07-01"))     
   TotalPD <- Yearlymean*YearDays
    #names(TotalPD)<- c('TotalPD')
  TotalPD
  
 # Get the  non-sampled days
   nonSampledDays <- YearDays - SampledDays
   nonSampledDays
   
    # Replace NA's with "0" to avoid errors while computing
ByYear <- data.frame(lapply(ByYear,function(x) replace(x,is.na(x),0))) 
# ByYear
 
 #ByYear2 <- ByYear
#ByYear2
# split by week for processing
#x.s <- split(ByYear, ByYear$wk)
#x.s

#Select only xd,td and pd for computing the covariance
mydf<-ByYear[3:5]
#mydf<-ByYear[1:250,3:5]  # process only 250 days of the year
#mydf

trans <- t(mydf)
#transs <- trans[,1:250]
#transs
  #write.table(transs,file="C:/Documents and Settings/fcarrillo/Desktop/Transposed2 CovSum.xls",sep="\t",row.names=F) 
#trans

 # Apply the function to each day of the year to estimate the covariance
     i.seq <- lapply(seq(ncol(trans) - 1),  function(x) x:(ncol(trans) - 1))
     CovSum <- lapply(i.seq, function(.col){
     # compute the 3 columns of data
  cbind(xd=varA + trans["xd", .col[1]] * covAB + trans["xd", .col + 1] *
  covAB + trans["xd", .col[1]] * trans["xd", .col + 1] *  varB,
  pd=trans["pd", .col[1]] * trans["pd", .col +  1],
  td=trans["td", .col[1]] * trans["td", .col +  1])
  })
  #CovSum
 #i <- CovSum[6:6]   # get the numbers of records from CovSum
 #i
# write.table(i,file="C:/Documents and Settings/fcarrillo/Desktop/CovSum250.xls",sep="\t",row.names=F) 
# rbind for the output
 Covariance <- do.call(rbind, CovSum)
 #Covariance[1:4]
 # add the covariance to the CovSum dataset
  makedf <- data.frame(Covariance, Cov=Covariance[, 'xd'] * Covariance[, 'pd'] / Covariance[, 'td'])
  #makedf <- makedf[1:65535,]
     #write.table(makedf,file="C:/Documents and Settings/fcarrillo/Desktop/results.xls",sep="\t",row.names=F) 
  #makedf <- makedf[65536:66415,]
   #write.table(makedf,file="C:/Documents and Settings/fcarrillo/Desktop/results2.xls",sep="\t",row.names=F) 
  #makedf[1:2500,]
##### Remove NaN's from dataset
#rmNaN <- apply(SummedCov, 2, is.nan)
#SummedCov[rmNaN] <- 0
#SummedCov$Cov[1:2000]
SummedCovPiPj<-sum(makedf[, 'Cov'],na.rm=T)
SummedCovPiPj
     # Estimate the variability of the total passage 
 varTotPD <- ((1-SampledDays/YearDays)*YearDays^2/SampledDays*YearlyVar) + ((YearDays/SampledDays*(SummedVarPD + 2*SummedCovPiPj)))
  #names(varTotPD) <- 'varTotPD'
 varTotPD
  # Estimate the quantiles
  T <- qt((1-0.10/2),YearDays) # Inverse t-distribution for 90% confidence intervals
 T<-data.frame(T)
  T
  # PlusMin
PlusMin <- T*varTotPD^.5
PlusMin
Lower <- TotalPD - PlusMin
names(Lower) <- 'Lower' 
Lower
Upper <- TotalPD + PlusMin
names(Upper) <- 'Upper'
 Upper
MyYear <- data.frame(Year=year,SampledDays,Yearlymean,MissedDays=nonSampledDays,Lower,TotalPD,Upper,varTotPD)
MyYear

# Export to Excel (csv or xls)
#write.table(MyFinal, file = "C:/Documents and Settings/fcarrillo/Desktop/Year.csv", sep = ",", col.names = NA,qmethod = "double")
#write.table(MyFinal, file = "C:/Documents and Settings/fcarrillo/Desktop/Year.xls",sep="\t",row.names=F)