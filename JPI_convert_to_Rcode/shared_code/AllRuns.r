
 chinook4 <- read.csv("Chinook_2004-2008.csv")
  head(chinook4);tail(chinook4)
  str(chinook4)
 nrow(chinook4)
  #winter$week1 <- seq(as.Date("2008-07-08"), length.out=52, by="1 week")
   # Separate the different runs and add a date column to each run
   winter <- subset(chinook4,Chinook_Run=='Winter');winter
 spring <- subset(chinook4,Chinook_Run=='Spring');spring
 #fall <- subset(chinook4,Chinook_Run=='Fall');fall
 #latefall <- subset(chinook4,Chinook_Run=='Late-Fall');latefall

  winter$Date <- seq(as.Date("2008-07-01"),as.Date("2009-06-30"),by="week")
 winter
 spring$Date <- seq(as.Date("2008-10-16"),as.Date("2009-10-15"),by="week")
 spring
#fall$Date <- seq(as.Date("2008-12-01"),as.Date("2009-11-30"),by="week")
# fall
# latefall$Date <- seq(as.Date("2008-4-01"),as.Date("2009-3-31"),by="week")
  #latefall
  
  #allruns <- rbind(winter,spring,fall,latefall);head(allruns)
  allruns <- rbind(winter,spring);head(allruns)
  m_allruns <- melt(allruns,id=c("week","Chinook_Run","Date"))
  nrow(m_allruns);head(m_allruns)
 

ggplot(m_allruns,aes(Date,value/1000)) + geom_line(aes(colour=Chinook_Run),size=1) +  theme_bw() +
scale_x_date(major="month", minor="2 weeks", format="%b",expand=c(0,0))  +
#scale_y_continuous(breaks=c(100,250,500,750,1000,1500,2000)) +
opts(legend.position="none",axis.text.x = theme_text(angle=45,hjust=1)) +
facet_grid(variable~Chinook_Run,scales="free") + labs(y="Number of fish X 1,000",x="")




