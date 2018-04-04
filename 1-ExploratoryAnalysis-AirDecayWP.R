rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych")

Install_And_Load(Required_Packages)

File.LeakTestStation <- c("DataSource/QUK2SH_WJ_Leak_Rate.tsv")
File.Inspection <- c("DataSource/Barcode_Reporting.tsv")


######################################################################################################################################################################
## Load data processed in 0-ExtractCleanData-LeakTestStation.R
dt.AirDecay.WP.NoMaster <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.RDS")
dt.AirDecay.WP.Master <- readRDS("DataOutput/dt.AirDecay.WP.Master.RDS")
dt.TempHumidity <- readRDS("DataOutput/dt.TempHumidity.RDS")
# dt.Inspection.Full <- readRDS("DataOutput/dt.Inspection.Full.RDS")

### All duplicates were not removed up to this stage ###


## Check data summary
summarizeColumns(dt.AirDecay.WP.NoMaster)
## Found that there were missing values on casting date due to wrong barcode format. We need to remove those part with incorrect ID.
dt.AirDecay.WP.NoMaster <- dt.AirDecay.WP.NoMaster[complete.cases(dt.AirDecay.WP.NoMaster),]
## Check data summary
summarizeColumns(dt.AirDecay.WP.NoMaster)


## Remove duplicates, calculate leak test result based on supplied spec.
## Run statics on data on daily basis.
dt.Daily.Statics.AirDecay.WP.NoMaster <- Daily.Statics.AirDecay.WP(dt.AirDecay.WP.NoMaster, -3, 2.1 )
# dt.Daily.Statics.AirDecay.MC <- Daily.Statics.AirDecay.MC(dt.AirDecay.MC.Full, -6, 6 )
# dt.Daily.Statics.AirDecay.He <- Daily.Statics.AirDecay.He(dt.AirDecay.He.Full, 0, 3.0E-6 )



## find the descriptive statistics of daily summary
describe(dt.Daily.Statics.AirDecay.WP.NoMaster)


## Establish control limits from good dates
dt.Daily.Statics.AirDecay.WP.2018 <- dt.Daily.Statics.AirDecay.WP.NoMaster[dt.Daily.Statics.AirDecay.WP.NoMaster$Date >= as.Date("2018-01-01"),]

g.Mean <- Plot.LineChart.Date(dt.Daily.Statics.AirDecay.WP.2018, "Date", "Avg.LeakRate", 0, -3, 2.1,
                         mean(dt.Daily.Statics.AirDecay.WP.2018$Avg.LeakRate),
                         sd(dt.Daily.Statics.AirDecay.WP.2018$Avg.LeakRate),
                         "WP Leak Rate Statics - Daily Mean Chart")

g.SD <- Plot.LineChart.Date(dt.Daily.Statics.AirDecay.WP.2018, "Date", "Stdev.LeakRate", 0, 0, 0,
                       mean(dt.Daily.Statics.AirDecay.WP.2018$Stdev.LeakRate),
                       sd(dt.Daily.Statics.AirDecay.WP.2018$Stdev.LeakRate),
                       "WP Leak Rate Statics - Daily Sigma Chart")

g.NGRate <- Plot.LineChart.Date(dt.Daily.Statics.AirDecay.WP.2018, "Date", "RejectPrecent", 0, 0, 0,
                           mean(dt.Daily.Statics.AirDecay.WP.2018$RejectPrecent),
                           sd(dt.Daily.Statics.AirDecay.WP.2018$RejectPrecent),
                           "WP Leak Rate Statics - Daily Reject Precentage Chart")

g.qty <- Plot.LineChart.Date(dt.Daily.Statics.AirDecay.WP.2018, "Date", "Qty", 0, 0, 0,
                        mean(dt.Daily.Statics.AirDecay.WP.2018$Qty),
                        sd(dt.Daily.Statics.AirDecay.WP.2018$Qty),
                        "WP Leak Rate Statics - Daily Reject Qty. Chart")

multiplot(g.Mean, g.SD, g.NGRate, g.qty, cols=1)


## Select below good dates to establish contorl limit: 10/JAN/2018 to 16/JAN/2018, 1~4/FEB, 20/FEB, 22/FEB
date.good <- data.frame(Date = c(as.Date("2018-01-10"), as.Date("2018-01-11"), as.Date("2018-01-12"), as.Date("2018-01-13"), as.Date("2018-01-14"),
                                 as.Date("2018-01-15"), as.Date("2018-01-16"), as.Date("2018-02-01"), as.Date("2018-02-02"), as.Date("2018-02-03"),
                                 as.Date("2018-02-04"), as.Date("2018-02-20"), as.Date("2018-02-22")))
dt.good <- dt.AirDecay.WP.NoMaster[date(dt.AirDecay.WP.NoMaster$LeakTestDateTime) %in% date.good[,1], ]
dt.good <- dt.good[order(dt.good$LeakTestDateTime, decreasing = FALSE),]
#Remove duplicates
dt.good <- dt.good[ , .SD[.N] ,  by = c("part_id") ]
Mean.Control.WP <- mean(dt.good$air_decay_wp)
Sigma.Control.WP <- sd(dt.good$air_decay_wp)



## Plot Contorl Chart from Jan2017 till MAR2018
g.Mean <- Plot.LineChart.Year(dt.Daily.Statics.AirDecay.WP.NoMaster, "Date", "Avg.LeakRate", 0, -3, 2.1,
                                mean(dt.Daily.Statics.AirDecay.WP.NoMaster$Avg.LeakRate),
                                sd(dt.Daily.Statics.AirDecay.WP.NoMaster$Avg.LeakRate),
                                "WP Leak Rate Statics - Daily Mean Chart")

g.SD <- Plot.LineChart.Year(dt.Daily.Statics.AirDecay.WP.NoMaster, "Date", "Stdev.LeakRate", 0, 0, 0,
                                mean(dt.Daily.Statics.AirDecay.WP.NoMaster$Stdev.LeakRate),
                                sd(dt.Daily.Statics.AirDecay.WP.NoMaster$Stdev.LeakRate),
                                "WP Leak Rate Statics - Daily Sigma Chart")

g.NGRate <- Plot.LineChart.Year(dt.Daily.Statics.AirDecay.WP.NoMaster, "Date", "RejectPrecent", 0, 0, 0,
                              mean(dt.Daily.Statics.AirDecay.WP.NoMaster$RejectPrecent),
                              sd(dt.Daily.Statics.AirDecay.WP.NoMaster$RejectPrecent),
                              "WP Leak Rate Statics - Daily Reject Precentage Chart")

g.qty <- Plot.LineChart.Year(dt.Daily.Statics.AirDecay.WP.NoMaster, "Date", "Qty", 0, 0, 0,
                           mean(dt.Daily.Statics.AirDecay.WP.NoMaster$Qty),
                           sd(dt.Daily.Statics.AirDecay.WP.NoMaster$Qty),
                           "WP Leak Rate Statics - Daily Reject Qty. Chart")

multiplot(g.Mean, g.SD, g.NGRate, g.qty, cols=1)




## Observation #1: 1/JAN~ 29/JAN 2017: High reject rate but low leak rate variation
## Subset dataset from specific period
## Found out there was variation on leak rate, it didn't show up previously due to scale issue.
        dt.P1 <- dt.AirDecay.WP.NoMaster[date(dt.AirDecay.WP.NoMaster$LeakTestDateTime) >= as.Date("2017-01-01") 
                                      & date(dt.AirDecay.WP.NoMaster$LeakTestDateTime) <= as.Date("2017-01-29") ,]
        
        Sum.Stat.AirDecay.WP(dt.P1)
        
        dt.Hourly.Statics.P1 <- Hourly.Statics.AirDecay.WP(dt.P1, -3, 2.1 )
        
        g.Mean.P1 <- Plot.LineChart.Hour(dt.Hourly.Statics.P1, "HourDate", "Avg.LeakRate", 0, -3, 2.1,
                                 mean(dt.Hourly.Statics.P1$Avg.LeakRate),
                                 sd(dt.Hourly.Statics.P1$Avg.LeakRate),
                                 "WP Leak Rate Statics - Hourly Sigma Chart")
        
        g.Mean.P1 <- g.Mean.P1 + 
                      annotate("rect", xmin=as.POSIXct(as.Date("2017-01-03")), xmax=as.POSIXct(as.Date("2017-01-07")), 
                                ymin=-1, ymax=5, alpha=.1,fill="blue") + 
                      annotate("text", label="3~6/JAN 2017: \nHigh Leak Rate\n Variation & \nReject Rate", x=as.POSIXct(as.Date("2017-01-05")), y=4 ) +
                      annotate("rect", xmin=as.POSIXct(as.Date("2017-01-11")), xmax=as.POSIXct(as.Date("2017-01-14")), 
                               ymin=-1, ymax=5, alpha=.1,fill="blue") +
                      annotate("text", label="11~13/JAN 2017: \nHigh Leak Rate\n Variation & \nReject Rate", x=as.POSIXct(as.Date("2017-01-12")), y=4 )
          
        g.SD.P1 <- Plot.LineChart.Hour(dt.Hourly.Statics.P1, "HourDate", "Stdev.LeakRate", 0, 0, 0,
                                             mean(dt.Hourly.Statics.P1$Stdev.LeakRate),
                                             sd(dt.Hourly.Statics.P1$Stdev.LeakRate),
                                             "WP Leak Rate Statics - Hourly Mean Chart")

        g.SD.P1 <- g.SD.P1 + 
                      annotate("rect", xmin=as.POSIXct(as.Date("2017-01-03")), xmax=as.POSIXct(as.Date("2017-01-07")), 
                               ymin=-1, ymax=10, alpha=.1,fill="blue") + 
                      annotate("rect", xmin=as.POSIXct(as.Date("2017-01-11")), xmax=as.POSIXct(as.Date("2017-01-14")), 
                               ymin=-1, ymax=10, alpha=.1,fill="blue") 

        g.NGRate.P1 <- Plot.LineChart.Hour(dt.Hourly.Statics.P1, "HourDate", "RejectPrecent", 0, 0, 0,
                                             mean(dt.Hourly.Statics.P1$RejectPrecent),
                                             sd(dt.Hourly.Statics.P1$RejectPrecent),
                                             "WP Leak Rate Statics - Hourly Reject Precentage Chart")

        g.NGRate.P1 <- g.NGRate.P1 + 
                          annotate("rect", xmin=as.POSIXct(as.Date("2017-01-03")), xmax=as.POSIXct(as.Date("2017-01-07")), 
                                   ymin=-1, ymax=50, alpha=.1,fill="blue") + 
                          annotate("rect", xmin=as.POSIXct(as.Date("2017-01-11")), xmax=as.POSIXct(as.Date("2017-01-14")), 
                                   ymin=-1, ymax=50, alpha=.1,fill="blue") 
                
        g.qty.P1 <- Plot.LineChart.Hour(dt.Hourly.Statics.P1, "HourDate", "Qty", 0, 0, 0,
                                             mean(dt.Hourly.Statics.P1$Qty),
                                             sd(dt.Hourly.Statics.P1$Qty),
                                             "WP Leak Rate Statics - Hourly Reject Qty. Chart")

        g.qty.P1 <- g.qty.P1 + 
                        annotate("rect", xmin=as.POSIXct(as.Date("2017-01-03")), xmax=as.POSIXct(as.Date("2017-01-07")), 
                                 ymin=-1, ymax=30, alpha=.1,fill="blue") + 
                        annotate("rect", xmin=as.POSIXct(as.Date("2017-01-11")), xmax=as.POSIXct(as.Date("2017-01-14")), 
                                 ymin=-1, ymax=30, alpha=.1,fill="blue") 
        
        multiplot(g.Mean.P1, g.SD.P1, g.NGRate.P1, g.qty.P1, cols=1)

        
                
## Observation #2: 03/JAN ~ 06/JAN 2017: High variation on leak rate and reject rate

        # Tidy Leak Rate Data
        # Duplicates was included
        Plot.SinglePoint.WP.Type3(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                  " WP - 3~6/JAN/2017", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                  as.Date("2017-01-03"), as.Date("2017-01-06") )

        # Tidy humidity and Temp data
        # 
        
        # # Tidy Cast Date Data
        # usl = 2.1
        # lsl = -3
        # dt.LeakRate.WP.P2[dt.LeakRate.WP.P2$air_decay_wp > usl | dt.LeakRate.WP.P2$air_decay_wp < lsl, Result := as.factor("FAIL")]
        # dt.LeakRate.WP.P2[dt.LeakRate.WP.P2$air_decay_wp <= usl & dt.LeakRate.WP.P2$air_decay_wp >= lsl, Result := as.factor("PASS")]
        # 
        # dt.30min.Statics.WP.P2 <- HalfHourly.Statics.AirDecay.WP(dt.LeakRate.WP.P2, -3, 2.1)
        # 
        # g.Hst.CastDate <- ggplot(dt.LeakRate.WP.P2, aes(x=CastDate, fill = Result, color=Result)) +
        #                   geom_histogram(binwidth = 1, position="identity", alpha=0.5) + 
        #                   ggtitle(paste("QUK2 SH WJ WP Air Decay Pass/Fail Histogram - Cast Date" ))




## Observation #3: 15/JAN ~ 19/JAN 2017: leak rate cyclic variation
        
        # Tidy Leak Rate Data
        # Duplicates was included
        Plot.SinglePoint.WP.Type3(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                  " WP - 15~19/JAN/2017", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                  as.Date("2017-01-15"), as.Date("2017-01-19") )
        
    
        
        
## Observation #4: 27/JAN ~ 29/JAN 2017: High Variation & Reject Rate
        
        # Tidy Leak Rate Data
        # Duplicates was included
        Plot.SinglePoint.WP.Type3(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                  " WP - 27~29/JAN/2017", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                  as.Date("2017-01-27"), as.Date("2017-01-29") )
        
        Plot.SinglePoint.WP.SmallScale.Type1(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                  " WP - 27~29/JAN/2017", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                  as.Date("2017-01-27"), as.Date("2017-01-29") )
 

## Observation #5: 1/APR ~ 12/APR 2017: High Variation & Reject Rate
        # Tidy Leak Rate Data
        # Duplicates was included
        Plot.SinglePoint.WP.Type3(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                  " WP - 01~12/APR/2017", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                  as.Date("2017-04-01"), as.Date("2017-04-12") )

## Observation #6: 15/JAN ~ 20/JAN 2018: High Variation & Reject Rate
        # Tidy Leak Rate Data
        # Duplicates was included
        Plot.SinglePoint.WP.Type3(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                  " WP - 15~20/JAN/2018", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                  as.Date("2018-01-15"), as.Date("2018-01-20") )
        
        
        Plot.SinglePoint.WP.SmallScale.Type1(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                  " WP - 15~20/JAN/2018", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                  as.Date("2018-01-15"), as.Date("2018-01-20") )
        
        
        Plot.SinglePoint.WP.SmallScale.Type1(dt.AirDecay.WP.NoMaster, 0, -3, 2.1, 
                                             " WP - 15~20/JAN/2018", dt.AirDecay.WP.Master, "XBA1601290101A23", 
                                             as.Date("2018-01-19"), as.Date("2018-01-20") )
        
