rm(list=ls())
library(MASS)
#Read in the already created .RDS files with the variables (die, etc)
#that I want to summarize

AirHeFull=readRDS("DataOutput/dt.AirDecay.He.Full.RDS")
AirHeMaster=readRDS("DataOutput/dt.AirDecay.He.Master.RDS")
AirHeNoMaster=readRDS("DataOutput/dt.AirDecay.He.NoMaster.RDS")
AirMCFull=readRDS("DataOutput/dt.AirDecay.MC.Full.RDS")
AirMCMaster=readRDS("DataOutput/dt.AirDecay.MC.Master.RDS")
AirMCNoMaster=readRDS("DataOutput/dt.AirDecay.MC.NoMaster.RDS")
AirWPFull=readRDS("DataOutput/dt.AirDecay.WP.Full.RDS")
AirWPMaster=readRDS("DataOutput/dt.AirDecay.WP.Master.RDS")
AirWPNoMaster=readRDS("DataOutput/dt.AirDecay.WP.NoMaster.RDS")
TempHum=readRDS("DataOutput/dt.TempHumidity.RDS")

usedata=AirWPNoMaster
usedata$logdecaywp=log(usedata$air_decay_wp)

boxplot(log(usedata$air_decay_wp)~usedata$CastMC)
length(which(log(usedata$air_decay_wp)>-2))

#there is a weird discontinuity in this data showing up at the log scale
#try to draw a histogram zeroing in on this
hist(log(usedata$air_decay_wp),xlim=c(-2.5,4),breaks=50,col=3)
hist(usedata$logdecaywp,xlim=c(-2.5,4),breaks=50,col=3)

#let's try a box cox transformation of my variable, 
#create a linear model with data in the call
positive=usedata[usedata$air_decay_wp>0,]
yfit=lm(air_decay_wp~CastMC,data=positive)
value=boxcox(yfit)
transform=value$x[36]
hist(usedata$logdecaywp,include.lowest=FALSE)

#let's try again on this transform thing

usedata$positivewp=usedata$air_decay_wp+3.001
usedata$transform_air=1/sqrt(usedata$positivewp)
hist(usedata$transform_air,xlim=c(0,9))
remove=which(usedata$transform_air>1|usedata$transform_air<0.2)

usedata=usedata[-remove,]
hist(usedata$transform_air,breaks=50)
boxplot(usedata$transform_air~usedata$CastMC)
boxplot(usedata$transform_air~usedata$CastDie)
boxplot(jitter(usedata$transform_air)~usedata$CastMC_Die)
