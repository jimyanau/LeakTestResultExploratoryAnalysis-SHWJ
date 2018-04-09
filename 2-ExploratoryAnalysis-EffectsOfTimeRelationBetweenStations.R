rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")

## Load data from Pinning Station processed in 0-ExtractCleanData-LeakTestStation.R
## Data included duplicates
dt.Pinning.Full <- readRDS("DataOutput/dt.Pinning.RDS")

## Removing duplicates by keeping only the oldest record of the same part ID
dt.Pinning.1stRecord <- KeepOldestRecord.PinningStation(dt.Pinning.Full)
rm(dt.Pinning.Full)



## Load data from FIPg Station processed in 0-ExtractCleanData-LeakTestStation.R
## Data included duplicates
dt.FIPG.Full <- readRDS("DataOutput/dt.FIPG.RDS")

## Removing duplicates by keeping only the oldest record of the same part ID
dt.FIPG.1stRecord <- KeepOldestRecord.FIPGStation(dt.FIPG.Full)
rm(dt.FIPG.Full)

## Load data of Air Decay Stations
## Data included duplicates
dt.AirDecay.WP.NoMaster <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.RDS")
dt.AirDecay.MC.NoMaster <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.RDS")
dt.AirDecay.He.NoMaster <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.RDS")

## Remove duplicates by keeping only the oldest record of the same part id
dt.AirDecay.WP.NoMaster.1stRecord <- KeepOldestRecord.AirDecay(dt.AirDecay.WP.NoMaster)
dt.AirDecay.MC.NoMaster.1stRecord <- KeepOldestRecord.AirDecay(dt.AirDecay.MC.NoMaster)
dt.AirDecay.He.NoMaster.1stRecord <- KeepOldestRecord.AirDecay(dt.AirDecay.He.NoMaster)
rm(dt.AirDecay.WP.NoMaster)
rm(dt.AirDecay.MC.NoMaster)
rm(dt.AirDecay.He.NoMaster)

## Load Inspection Data
dt.Inspection.Full <- readRDS("DataOutput/dt.Inspection.Full.RDS")
dt.Inspection.Gate1 <- dt.Inspection.Full[dt.Inspection.Full$gate=="100%",]
dt.Inspection.Gate2 <- dt.Inspection.Full[dt.Inspection.Full$gate=="200%",]
dt.Inspection.Gate3 <- dt.Inspection.Full[dt.Inspection.Full$gate=="300%",]
rm(dt.Inspection.Full)

## Keep only the oldest data of the same part ID at each gate
dt.Inspection.Gate1.1stRecord <- KeepOldestRecord.Inspction(dt.Inspection.Gate1)
dt.Inspection.Gate2.1stRecord <- KeepOldestRecord.Inspction(dt.Inspection.Gate2)
dt.Inspection.Gate3.1stRecord <- KeepOldestRecord.Inspction(dt.Inspection.Gate3)


## Assemble Date/Time of different Station as per individual part id

dt.CompleteProcessTiming.1stRecord <- NULL
dt.CompleteProcessTiming.1stRecord <- dt.Inspection.Gate1.1stRecord[, c("part_id", "datetime", "inspection_result", "defect_code","defect_location")]

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "datetime", "100%_Insp_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "inspection_result", "100%InspectionResult")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_code", "100%DefectCode")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_location", "100%DefectLocation")

# Combine 100% Inspection data with Pinning Station Data
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.Pinning.1stRecord[, c( "part_id" , "PinningDateTime")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Add FIPG Station data into dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.FIPG.1stRecord[, c( "part_id" , "FIPGDateTime")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Add Air Decay WP Data into Dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.AirDecay.WP.NoMaster.1stRecord[, c( "part_id" , 
                                 "CastMC", "CastDie","CastMC_Die","CastDate","CastDateTime","LeakTestDateTime", "air_decay_wp" ,"Result")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "LeakTestDateTime", "1st_AirDecay_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "air_decay_wp", "1st_LeakRate_WP")
setnames(dt.CompleteProcessTiming.1stRecord, "Result", "1st_LeakTestResult_WP")

# Add Air Decay MC Data into Dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.AirDecay.MC.NoMaster.1stRecord[, c( "part_id" , "LeakTestDateTime", "air_decay_mc" ,"Result")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "LeakTestDateTime", "1st_AirDecay_DateTime_MC")
setnames(dt.CompleteProcessTiming.1stRecord, "air_decay_mc", "1st_LeakRate_MC")
setnames(dt.CompleteProcessTiming.1stRecord, "Result", "1st_LeakTestResult_MC")

### Remove row with missing data on Pinning Station Time & FIPG Station Time & Air Decay Test  ###
dt.CompleteProcessTiming.1stRecord <- dt.CompleteProcessTiming.1stRecord[ complete.cases(dt.CompleteProcessTiming.1stRecord), ]


# Add Helium Leak Test Data into Dataset
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.AirDecay.He.NoMaster.1stRecord[, c( "part_id" , "LeakTestDateTime", "helium_test" ,"Result")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "LeakTestDateTime", "1st_Helium_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "helium_test", "1st_LeakRate_He")
setnames(dt.CompleteProcessTiming.1stRecord, "Result", "1st_HeTestResult")

# Combine 200% Inspection data with Pinning Station Data
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.Inspection.Gate2.1stRecord[, c("part_id", "datetime", "inspection_result", "defect_code", "defect_location")], 
                                  by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "datetime", "200%_Insp_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "inspection_result", "200%InspectionResult")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_code", "200%DefectCode")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_location", "200%DefectLocation")


# Combine 300% Inspection data with Pinning Station Data
dt.CompleteProcessTiming.1stRecord <- merge(dt.CompleteProcessTiming.1stRecord, dt.Inspection.Gate3.1stRecord[, c("part_id", "datetime", "inspection_result", "defect_code", "defect_location")], 
                                            by.x = "part_id", by.y = "part_id", all.x = TRUE)

# Rename columns
setnames(dt.CompleteProcessTiming.1stRecord, "datetime", "300%_Insp_DateTime")
setnames(dt.CompleteProcessTiming.1stRecord, "inspection_result", "300%InspectionResult")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_code", "300%DefectCode")
setnames(dt.CompleteProcessTiming.1stRecord, "defect_location", "300%DefectLocation")


### Noticed that there was time difference between stations. Working with NCAP to get the difference between machine time and actual time.
### Inspection Stations had daylight stations
### Pinning Station, FIPG Station & Leak Test Station had no daylight saving
### Additional offest need to be adjusted on data as per the observation on machine on 5/APR/2018:
### -7 mins on Pinning Station Time
### +6 mins on FIPG Station

# Adjust time difference
dt.CompleteProcessTiming.1stRecord$PinningDateTime <- dt.CompleteProcessTiming.1stRecord$PinningDateTime - dminutes(7)
dt.CompleteProcessTiming.1stRecord$FIPGDateTime <- dt.CompleteProcessTiming.1stRecord$FIPGDateTime + dminutes(6)


### Pls notice that the data with missing value in 100% Inspection, Pinning Station, FIPG Station & Air Decay Leak 
### Test Station had been removed for investigation purpose. All records were the 1st. record of the same part ID.

# Calculate leak time between stations
dt.CompleteProcessTiming.1stRecord$Mins_IncomingInsp_FIPG <- as.numeric(difftime(dt.CompleteProcessTiming.1stRecord$FIPGDateTime, dt.CompleteProcessTiming.1stRecord$"100%_Insp_DateTime"), units="mins")
dt.CompleteProcessTiming.1stRecord$Mins_FIPG_AirDecay <- as.numeric(difftime(dt.CompleteProcessTiming.1stRecord$`1st_AirDecay_DateTime`, dt.CompleteProcessTiming.1stRecord$FIPGDateTime), units="mins")
dt.CompleteProcessTiming.1stRecord$Mins_IncomingInsp_AirDecay <- as.numeric(difftime(dt.CompleteProcessTiming.1stRecord$`1st_AirDecay_DateTime`, dt.CompleteProcessTiming.1stRecord$"100%_Insp_DateTime"), units="mins")

## Save dataset into "DataOutput/dt.CompleteProcessTiming.1stRecord.RDS"
saveRDS(dt.CompleteProcessTiming.1stRecord, "DataOutput/dt.CompleteProcessTiming.1stRecord.RDS")
# write.table(dt.CompleteProcessTiming.1stRecord, file = "DataOutput/dt.CompleteProcessTiming.1stRecord.csv",row.names=FALSE, na="",col.names=TRUE, sep=",")






############ Start Analysis  #####################

dt.CompleteProcessTiming.1stRecord <- readRDS("DataOutput/dt.CompleteProcessTiming.1stRecord.RDS")

# Subset Data of 2018 for investigation
dt.CompleteProcessTiming.1stRecord <- dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$"1st_AirDecay_DateTime" >= as.Date("2017-06-01"),]

### Air Decay WP

## Plot Disctribution of lead time between FIPG station & Leak Test Station
g.Mins_FIPG_AirDecay_WP.FAIL <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_WP`=="FAIL", ], 
                                       aes(x=Mins_FIPG_AirDecay,fill=`1st_LeakTestResult_WP`)) +
                                      geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
                                      scale_x_continuous(limits = c(50, 150)) +
                                      xlab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                      ylab("Counts") +
                                      ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay WP Failures (Jun17~Mar18)")) +
                                      theme(text = element_text(size=10),legend.position="bottom") 

g.Mins_FIPG_AirDecay_WP.PASS <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_WP`=="PASS", ], 
                                           aes(x=Mins_FIPG_AirDecay,fill=`1st_LeakTestResult_WP`)) +
                                            geom_histogram(binwidth=1, alpha=.5, position="identity", colour='blue', fill = 'blue') +
                                            scale_x_continuous(limits = c(50, 150)) +
                                            xlab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                            ylab("Counts") +
                                            ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay WP Passers (Jun17~Mar18)")) +
                                            theme(text = element_text(size=10),legend.position="bottom") 

g.boxplot.Mins_FIPG_AirDecay_WP <- ggplot(dt.CompleteProcessTiming.1stRecord, aes(x=`1st_LeakTestResult_WP`, 
                                            y=Mins_FIPG_AirDecay, fill=`1st_LeakTestResult_WP`)) + 
                                            geom_boxplot() +
                                            scale_y_continuous(limits = c(50, 150)) +
                                            ylab("Minutes btw FIPG Station & Air Decay Leka Test") +
                                            xlab("Leak Test Result") +
                                            ggtitle(paste("QUK2 SH WJ Lead Time Comparison btw Incoming Inspection & Air Decay Leak Test - WP (Jun17~Mar18)")) +
                                            theme(text = element_text(size=10),legend.position="bottom") 


multiplot(g.Mins_FIPG_AirDecay_WP.PASS, g.Mins_FIPG_AirDecay_WP.FAIL, cols=1)


## Plot Disctribution of lead time between Incoming Inspection & Leak Test Station
g.Mins_IncomingInsp_AirDecay_WP.FAIL <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_WP`=="FAIL", ], 
                                           aes(x=Mins_IncomingInsp_AirDecay,fill=`1st_LeakTestResult_WP`)) +
                                          geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
                                          # geom_density() +
                                          scale_x_continuous(limits = c(50, 150)) +
                                          xlab("Minutes btw Incoming Inspection & Air Decay Leka Test") +
                                          ylab("Counts") +
                                          ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay WP Failures (Jun17~Mar18)")) +
                                          theme(text = element_text(size=10),legend.position="bottom") 

g.Mins_IncomingInsp_AirDecay_WP.PASS <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_WP`=="PASS", ], 
                                           aes(x=Mins_IncomingInsp_AirDecay,fill=`1st_LeakTestResult_WP`)) +
                                            geom_histogram(binwidth=1, alpha=.5, position="identity", colour='blue', fill = 'blue') +
                                            # geom_density() +
                                            scale_x_continuous(limits = c(50, 150)) +
                                            xlab("Minutes btw Incoming Inspection & Air Decay Leka Test") +
                                            ylab("Counts") +
                                            ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay WP Passers (Jun17~Mar18)")) +
                                            theme(text = element_text(size=10),legend.position="bottom") 

g.boxplot.Mins_IncomingInsp_AirDecay_WP <- ggplot(dt.CompleteProcessTiming.1stRecord, aes(x=`1st_LeakTestResult_WP`, 
                                          y=Mins_IncomingInsp_AirDecay, fill=`1st_LeakTestResult_WP`)) + 
                                          geom_boxplot() +
                                          scale_y_continuous(limits = c(50, 150)) +
                                          ylab("Minutes btw Incoming Inspection & Air Decay Leka Test") +
                                          xlab("Leak Test Result") +
                                          ggtitle(paste("QUK2 SH WJ Lead Time Comparison btw Incoming Inspection & Air Decay Leak Test - WP (Jun17~Mar18)")) +
                                          theme(text = element_text(size=10),legend.position="bottom") 

multiplot(g.Mins_IncomingInsp_AirDecay_WP.PASS, g.Mins_IncomingInsp_AirDecay_WP.FAIL, cols=1)


## Plot Disctribution of lead time between Incoming inspection & FIPG station
g.Mins_IncomingInsp_FIPG_WP.FAIL <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_WP`=="FAIL", ], 
                                       aes(x=Mins_IncomingInsp_FIPG,fill=`1st_LeakTestResult_WP`)) +
                                        geom_histogram(binwidth=5, alpha=.5, position="identity", colour='red', fill = 'red') +
                                        scale_x_continuous(limits = c(-50, 100)) +
                                        xlab("Minutes btw Incoming Inspection & FIPG Station") +
                                        ylab("Counts") +
                                        ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay WP Failures (Jun17~Mar18)")) +
                                        theme(text = element_text(size=10),legend.position="bottom") 

g.Mins_IncomingInsp_FIPG_WP.PASS <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_WP`=="PASS", ], 
                                       aes(x=Mins_IncomingInsp_FIPG,fill=`1st_LeakTestResult_WP`)) +
                                        geom_histogram(binwidth=5, alpha=.5, position="identity", colour='blue', fill = 'blue') +
                                        scale_x_continuous(limits = c(-50, 100)) +
                                        xlab("Minutes btw Incoming Inspection & FIPG Station") +
                                        ylab("Counts") +
                                        ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay WP Passers (Jun17~Mar18)")) +
                                        theme(text = element_text(size=10),legend.position="bottom")


### Air Decay MC

## Plot Disctribution of lead time between FIPG station & Leak Test Station
g.Mins_FIPG_AirDecay_MC.FAIL <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_MC`=="FAIL", ], 
                                       aes(x=Mins_FIPG_AirDecay,fill=`1st_LeakTestResult_MC`)) +
                                        geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
                                        scale_x_continuous(limits = c(50, 150)) +
                                        xlab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                        ylab("Counts") +
                                        ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay MC Failures (Jun17~Mar18)")) +
                                        theme(text = element_text(size=10),legend.position="bottom") 

g.Mins_FIPG_AirDecay_MC.PASS <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_MC`=="PASS", ], 
                                       aes(x=Mins_FIPG_AirDecay,fill=`1st_LeakTestResult_MC`)) +
                                      geom_histogram(binwidth=1, alpha=.5, position="identity", colour='blue', fill = 'blue') +
                                      scale_x_continuous(limits = c(50, 150)) +
                                      xlab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                      ylab("Counts") +
                                      ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay MC Passers (Jun17~Mar18)")) +
                                      theme(text = element_text(size=10),legend.position="bottom") 

g.boxplot.Mins_FIPG_AirDecay_MC <- ggplot(dt.CompleteProcessTiming.1stRecord, aes(x=`1st_LeakTestResult_MC`, 
                                                                                  y=Mins_FIPG_AirDecay, fill=`1st_LeakTestResult_MC`)) + 
                                                                                  geom_boxplot() +
                                                                                  scale_y_continuous(limits = c(50, 150)) +
                                                                                  ylab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                                                                  xlab("Leak Test Result") +
                                                                                  ggtitle(paste("QUK2 SH WJ Lead Time Comparison btw Incoming Inspection & Air Decay Leak Test - MC (Jun17~Mar18)")) +
                                                                                  theme(text = element_text(size=10),legend.position="bottom") 


multiplot(g.Mins_FIPG_AirDecay_MC.PASS, g.Mins_FIPG_AirDecay_MC.FAIL, cols=1)


## Plot Disctribution of lead time between Incoming Inspection & Leak Test Station
g.Mins_IncomingInsp_AirDecay_MC.FAIL <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_MC`=="FAIL", ], 
                                               aes(x=Mins_IncomingInsp_AirDecay,fill=`1st_LeakTestResult_MC`)) +
                                              geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
                                              # geom_density() +
                                              scale_x_continuous(limits = c(50, 150)) +
                                              xlab("Minutes btw Incoming Inspection & Air Decay Leak Test") +
                                              ylab("Counts") +
                                              ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay MC Failures (Jun17~Mar18)")) +
                                              theme(text = element_text(size=10),legend.position="bottom") 

g.Mins_IncomingInsp_AirDecay_MC.PASS <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_MC`=="PASS", ], 
                                               aes(x=Mins_IncomingInsp_AirDecay,fill=`1st_LeakTestResult_MC`)) +
                                                geom_histogram(binwidth=1, alpha=.5, position="identity", colour='blue', fill = 'blue') +
                                                # geom_density() +
                                                scale_x_continuous(limits = c(50, 150)) +
                                                xlab("Minutes btw Incoming Inspection & Air Decay Leak Test") +
                                                ylab("Counts") +
                                                ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Air Decay MC Passers (Jun17~Mar18)")) +
                                                theme(text = element_text(size=10),legend.position="bottom") 

g.boxplot.Mins_IncomingInsp_AirDecay_MC <- ggplot(dt.CompleteProcessTiming.1stRecord, aes(x=`1st_LeakTestResult_MC`, 
                                                    y=Mins_IncomingInsp_AirDecay, fill=`1st_LeakTestResult_MC`)) + 
                                                    geom_boxplot() +
                                                    scale_y_continuous(limits = c(50, 150)) +
                                                    ylab("Minutes btw Incoming Inspection & Air Decay Leak Test") +
                                                    xlab("Leak Test Result") +
                                                    ggtitle(paste("QUK2 SH WJ Lead Time Comparison btw Incoming Inspection & Air Decay Leak Test - MC (Jun17~Mar18)")) +
                                                    theme(text = element_text(size=10),legend.position="bottom") 

multiplot(g.Mins_IncomingInsp_AirDecay_MC.PASS, g.Mins_IncomingInsp_AirDecay_MC.FAIL, cols=1)


### Helium Test

## Plot Disctribution of lead time between FIPG station & Leak Test Station
g.Mins_FIPG_AirDecay_He.FAIL <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_HeTestResult`=="FAIL", ], 
                                       aes(x=Mins_FIPG_AirDecay,fill=`1st_HeTestResult`)) +
                                      geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
                                      scale_x_continuous(limits = c(50, 150)) +
                                      xlab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                      ylab("Counts") +
                                      ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Helium Failures (Jun17~Mar18)")) +
                                      theme(text = element_text(size=10),legend.position="bottom") 

g.Mins_FIPG_AirDecay_He.PASS <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_HeTestResult`=="PASS", ], 
                                       aes(x=Mins_FIPG_AirDecay,fill=`1st_HeTestResult`)) +
                                      geom_histogram(binwidth=1, alpha=.5, position="identity", colour='blue', fill = 'blue') +
                                      scale_x_continuous(limits = c(50, 150)) +
                                      xlab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                      ylab("Counts") +
                                      ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Helium Passers (Jun17~Mar18)")) +
                                      theme(text = element_text(size=10),legend.position="bottom") 

g.boxplot.Mins_FIPG_AirDecay_He <- ggplot(dt.CompleteProcessTiming.1stRecord, aes(x=`1st_HeTestResult`, 
                                                                                  y=Mins_FIPG_AirDecay, fill=`1st_HeTestResult`)) + 
                                                                                  geom_boxplot() +
                                                                                  scale_y_continuous(limits = c(50, 150)) +
                                                                                  ylab("Minutes btw FIPG Station & Air Decay Leak Test") +
                                                                                  xlab("Leak Test Result") +
                                                                                  ggtitle(paste("QUK2 SH WJ Lead Time Comparison btw Incoming Inspection & Air Decay Leak Test - Helium (Jun17~Mar18)")) +
                                                                                  theme(text = element_text(size=10),legend.position="bottom") 


multiplot(g.Mins_FIPG_AirDecay_He.PASS, g.Mins_FIPG_AirDecay_He.FAIL, cols=1)


## Plot Disctribution of lead time between Incoming Inspection & Leak Test Station
g.Mins_IncomingInsp_AirDecay_He.FAIL <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_HeTestResult`=="FAIL", ], 
                                               aes(x=Mins_IncomingInsp_AirDecay,fill=`1st_HeTestResult`)) +
                                              geom_histogram(binwidth=1, alpha=.5, position="identity", colour='red', fill = 'red') +
                                              # geom_density() +
                                              scale_x_continuous(limits = c(50, 150)) +
                                              xlab("Minutes btw Incoming Inspection & Air Decay Leak Test") +
                                              ylab("Counts") +
                                              ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Helium Failures (Jun17~Mar18)")) +
                                              theme(text = element_text(size=10),legend.position="bottom") 

g.Mins_IncomingInsp_AirDecay_He.PASS <- ggplot(dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_HeTestResult`=="PASS", ], 
                                               aes(x=Mins_IncomingInsp_AirDecay,fill=`1st_HeTestResult`)) +
                                              geom_histogram(binwidth=1, alpha=.5, position="identity", colour='blue', fill = 'blue') +
                                              # geom_density() +
                                              scale_x_continuous(limits = c(50, 150)) +
                                              xlab("Minutes btw Incoming Inspection & Air Decay Leak Test") +
                                              ylab("Counts") +
                                              ggtitle(paste("QUK2 SH WJ Lead Time Comparison - Helium Passers (Jun17~Mar18)")) +
                                              theme(text = element_text(size=10),legend.position="bottom") 

g.boxplot.Mins_IncomingInsp_AirDecay_He <- ggplot(dt.CompleteProcessTiming.1stRecord, aes(x=`1st_HeTestResult`, 
                                                                                          y=Mins_IncomingInsp_AirDecay, fill=`1st_HeTestResult`)) + 
                                                                                          geom_boxplot() +
                                                                                          scale_y_continuous(limits = c(50, 150)) +
                                                                                          ylab("Minutes btw Incoming Inspection & Air Decay Leak Test") +
                                                                                          xlab("Leak Test Result") +
                                                                                          ggtitle(paste("QUK2 SH WJ Lead Time Comparison btw Incoming Inspection & Air Decay Leak Test - Helium (Jun17~Mar18)")) +
                                                                                          theme(text = element_text(size=10),legend.position="bottom") 

multiplot(g.Mins_IncomingInsp_AirDecay_He.PASS, g.Mins_IncomingInsp_AirDecay_He.FAIL, cols=1)
