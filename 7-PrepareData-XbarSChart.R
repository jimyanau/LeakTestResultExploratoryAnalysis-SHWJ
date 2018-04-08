## Here is to prepare data for setup the x-bar S chart


rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS", "ggpubr")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


########## Preprocess Data                               ########################################
########## Only do this when your data source is changed ########################################
## Load data processed in 0-ExtractCleanData-LeakTestStation.R
## duplicaes will be included becasue we want to observe all process variation
dt.AirDecay.WP.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.RDS")
dt.AirDecay.MC.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.RDS")
dt.AirDecay.He.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.RDS")

## assemble time between failures on testing time and casting time
dt.AirDecay.WP.NoMaster.TBM <- Add.Suportive.Cols.to.LeakTestStation(dt.AirDecay.WP.NoMaster.TBM)
dt.AirDecay.MC.NoMaster.TBM <- Add.Suportive.Cols.to.LeakTestStation(dt.AirDecay.MC.NoMaster.TBM)
dt.AirDecay.He.NoMaster.TBM <- Add.Suportive.Cols.to.LeakTestStation(dt.AirDecay.He.NoMaster.TBM)

## load data of timing of each process. This data was processed by "2-ExploratoryAnalysis-EffectsOfTimeRelationBetweenStations.R"
dt.CompleteProcessTiming.1stRecord <- readRDS("DataOutput/dt.CompleteProcessTiming.1stRecord.RDS")

## merge complete process timming into dataset with time between failures for analysis
dt.AirDecay.WP.NoMaster.TBM <- merge(dt.AirDecay.WP.NoMaster.TBM, dt.CompleteProcessTiming.1stRecord[, c("part_id", "100%_Insp_DateTime", 
                                     "1st_AirDecay_DateTime", "1st_Helium_DateTime", "200%_Insp_DateTime", "300%_Insp_DateTime",
                                     "Mins_IncomingInsp_FIPG","Mins_FIPG_AirDecay","Mins_IncomingInsp_AirDecay")], 
                                     by.x = "part_id", by.y = "part_id", all.x = TRUE)

dt.AirDecay.MC.NoMaster.TBM <- merge(dt.AirDecay.MC.NoMaster.TBM, dt.CompleteProcessTiming.1stRecord[, c("part_id", "100%_Insp_DateTime", 
                                     "1st_AirDecay_DateTime", "1st_Helium_DateTime", "200%_Insp_DateTime", "300%_Insp_DateTime",
                                     "Mins_IncomingInsp_FIPG","Mins_FIPG_AirDecay","Mins_IncomingInsp_AirDecay")], 
                                     by.x = "part_id", by.y = "part_id", all.x = TRUE)


dt.AirDecay.He.NoMaster.TBM <- merge(dt.AirDecay.He.NoMaster.TBM, dt.CompleteProcessTiming.1stRecord[, c("part_id", "100%_Insp_DateTime", 
                                     "1st_AirDecay_DateTime", "1st_Helium_DateTime", "200%_Insp_DateTime", "300%_Insp_DateTime",
                                     "Mins_IncomingInsp_FIPG","Mins_FIPG_AirDecay","Mins_IncomingInsp_AirDecay")], 
                                     by.x = "part_id", by.y = "part_id", all.x = TRUE)

# sort dataset in order of test time
dt.AirDecay.WP.NoMaster.TBM <- dt.AirDecay.WP.NoMaster.TBM[order(dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime, decreasing = FALSE),]
dt.AirDecay.MC.NoMaster.TBM <- dt.AirDecay.MC.NoMaster.TBM[order(dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime, decreasing = FALSE),]
dt.AirDecay.He.NoMaster.TBM <- dt.AirDecay.He.NoMaster.TBM[order(dt.AirDecay.He.NoMaster.TBM$LeakTestDateTime, decreasing = FALSE),]

# Save dataset for further investigation
saveRDS(dt.AirDecay.WP.NoMaster.TBM, "DataOutput/dt.AirDecay.WP.NoMaster.TBM.RDS")
saveRDS(dt.AirDecay.MC.NoMaster.TBM, "DataOutput/dt.AirDecay.MC.NoMaster.TBM.RDS")
saveRDS(dt.AirDecay.He.NoMaster.TBM, "DataOutput/dt.AirDecay.He.NoMaster.TBM.RDS")


# This is just to see correlation between WP & MC on failures
dt.Failure <- dt.CompleteProcessTiming.1stRecord[dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_WP`== "FAIL" |  dt.CompleteProcessTiming.1stRecord$`1st_LeakTestResult_MC` == "FAIL", ]

dt.Failure$LeakRateWP <- dt.Failure$`1st_LeakRate_WP`
dt.Failure$LeakRateMC <- dt.Failure$`1st_LeakRate_MC`
dt.Failure$LeakRateHe <- dt.Failure$`1st_LeakRate_He`*1000000

dt.Failure <- dt.Failure[dt.Failure$LeakRateWP < 50, ]
dt.Failure <- dt.Failure[dt.Failure$LeakRateMC < max(dt.Failure$LeakRateMC), ]
dt.Failure <- dt.Failure[dt.Failure$LeakRateHe < 50, ]

ggscatter(dt.Failure, x = "LeakRateWP", y = "LeakRateMC", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Air Decay Leak Rate (WP)", ylab = "Air Decay Leak Rate (MC)")

ggscatter(dt.Failure, x = "LeakRateWP", y = "LeakRateHe", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Air Decay Leak Rate (WP)", ylab = "Helium Leak Rate")

ggscatter(dt.Failure, x = "LeakRateMC", y = "LeakRateHe", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Air Decay Leak Rate (MC)", ylab = "Helium Leak Rate")



