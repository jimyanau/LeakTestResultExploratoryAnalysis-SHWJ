## This code is to Simulate the reject improvement if we add offset of OK master to leak rate.

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS","qicharts", "ggpubr", "prophet")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


# Load dataset processed at "7-PrepareData-XbarSChart.R"
## duplicaes will be included becasue we want to observe all process variation
dt.AirDecay.WP.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.TBM.RDS")
dt.AirDecay.MC.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.TBM.RDS")
# dt.AirDecay.He.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.TBM.RDS")
dt.AirDecay.MC.Master <- readRDS("DataOutput/dt.AirDecay.MC.Master.RDS")
dt.AirDecay.WP.Master <- readRDS("DataOutput/dt.AirDecay.WP.Master.RDS")



# subset data from DEC-17 ~ FEB-18
dt.AirDecay.WP.NoMaster.sub <- dt.AirDecay.WP.NoMaster.TBM[dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime >= as.Date("2017-07-01") &
                                                             dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime < as.Date("2018-01-01"), ]

dt.AirDecay.MC.NoMaster.sub <- dt.AirDecay.MC.NoMaster.TBM[dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime >= as.Date("2017-07-01") &
                                                             dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime < as.Date("2018-01-01"), ]

dt.MC.OKMaster <- dt.AirDecay.MC.Master[dt.AirDecay.MC.Master$part_id == "XBA1601290101A23" ,  ]

dt.WP.OKMaster <- dt.AirDecay.WP.Master[dt.AirDecay.WP.Master$part_id == "XBA1601290101A23"   ,  ]

# clean memory
rm(dt.AirDecay.WP.NoMaster.TBM, dt.AirDecay.MC.NoMaster.TBM)


# Extract only the latest result of each part id
dt.AirDecay.WP.NoMaster.sub <- KeepLatestRecord.AirDecay(dt.AirDecay.WP.NoMaster.sub)

dt.AirDecay.MC.NoMaster.sub <- KeepLatestRecord.AirDecay(dt.AirDecay.MC.NoMaster.sub)

# sort data in test time order
dt.MC.OKMaster <- dt.MC.OKMaster[order(dt.MC.OKMaster$LeakTestDateTime, decreasing = FALSE),]

#add ID column
dt.MC.OKMaster$ID <- seq.int(nrow(dt.MC.OKMaster))

## Get the middle of the time period between 2 master check as start & finish time of test value offest

## Air Decay MC
# create temp dt to get the previous & next test time of one test as per the order of ID
dt.MC.temp.previous <- dt.MC.OKMaster[1:(nrow(dt.MC.OKMaster)-1)]
dt.MC.temp.previous$ID <- dt.MC.temp.previous$ID+1
dt.MC.temp.previous$PreviousTestTime <- dt.MC.temp.previous$LeakTestDateTime
dt.MC.temp.previous$PreviousMasterValue <- dt.MC.temp.previous$air_decay_mc

dt.MC.temp.next <- dt.MC.OKMaster[2:nrow(dt.MC.OKMaster)]
dt.MC.temp.next$ID <- dt.MC.temp.next$ID-1
dt.MC.temp.next$NextTestTime <- dt.MC.temp.next$LeakTestDateTime
dt.MC.temp.next$NextMasterValue <- dt.MC.temp.next$air_decay_mc

# merge temp dt into main dt to get the previous & next test time of OK Master
dt.MC.OKMaster <- merge(dt.MC.OKMaster, dt.MC.temp.previous[, c("ID", "PreviousTestTime","PreviousMasterValue")], by.x = "ID", by.y = "ID", all.x = TRUE)
dt.MC.OKMaster <- merge(dt.MC.OKMaster, dt.MC.temp.next[, c("ID", "NextTestTime","NextMasterValue")], by.x = "ID", by.y = "ID", all.x = TRUE)

# # shift half of the previous/next test time as affect period of the OK master offset
# dt.MC.OKMaster$StartEffectTime <- dt.MC.OKMaster$LeakTestDateTime + difftime(dt.MC.OKMaster$PreviousTestTime,dt.MC.OKMaster$LeakTestDateTime)/2
# dt.MC.OKMaster$EndEffectTime <- dt.MC.OKMaster$LeakTestDateTime + difftime(dt.MC.OKMaster$NextTestTime,dt.MC.OKMaster$LeakTestDateTime)/2
# 
# # set correct value for the first row and last row
# dt.MC.OKMaster$StartEffectTime[1] <- dt.MC.OKMaster$LeakTestDateTime[1]
# dt.MC.OKMaster$EndEffectTime[nrow(dt.MC.OKMaster)] <- dt.MC.OKMaster$LeakTestDateTime[nrow(dt.MC.OKMaster)]



# Add OK Master offset into Air Decay Data
for (i in 1:nrow(dt.AirDecay.MC.NoMaster.sub)){
  dt.AirDecay.MC.NoMaster.sub$MasterDataID[i] <- dt.MC.OKMaster[LeakTestDateTime >= dt.AirDecay.MC.NoMaster.sub$LeakTestDateTime[i]  
                                                                  & PreviousTestTime <= dt.AirDecay.MC.NoMaster.sub$LeakTestDateTime[i]
                                                                  ,ID]
}

dt.AirDecay.MC.NoMaster.sub$PreviousMasterTime <- dt.MC.OKMaster$PreviousTestTime[ID=dt.AirDecay.MC.NoMaster.sub$MasterDataID]
dt.AirDecay.MC.NoMaster.sub$PreviousMasterValue <- dt.MC.OKMaster$PreviousMasterValue[ID=dt.AirDecay.MC.NoMaster.sub$MasterDataID]
dt.AirDecay.MC.NoMaster.sub$CurrentMasterTime <- dt.MC.OKMaster$LeakTestDateTime[ID=dt.AirDecay.MC.NoMaster.sub$MasterDataID]
dt.AirDecay.MC.NoMaster.sub$CurrentMasterValue <- dt.MC.OKMaster$MasterValue[ID=dt.AirDecay.MC.NoMaster.sub$MasterDataID]
dt.AirDecay.MC.NoMaster.sub$MasterOffset <- as.numeric(difftime(dt.AirDecay.MC.NoMaster.sub$LeakTestDateTime,dt.AirDecay.MC.NoMaster.sub$PreviousMasterTime,units="days"))*(dt.AirDecay.MC.NoMaster.sub$CurrentMasterValue - dt.AirDecay.MC.NoMaster.sub$PreviousMasterValue)/as.numeric(difftime(dt.AirDecay.MC.NoMaster.sub$CurrentMasterTime,dt.AirDecay.MC.NoMaster.sub$PreviousMasterTime,units="days"))
dt.AirDecay.MC.NoMaster.sub$LeakRateOffseted <- dt.AirDecay.MC.NoMaster.sub$air_decay_mc + dt.AirDecay.MC.NoMaster.sub$MasterOffset

# Calculate result of new offset leak rate
dt.AirDecay.MC.NoMaster.sub[dt.AirDecay.MC.NoMaster.sub$LeakRateOffseted > 6 | dt.AirDecay.MC.NoMaster.sub$LeakRateOffseted < -6, OffsetResult := as.factor("FAIL")]
dt.AirDecay.MC.NoMaster.sub[dt.AirDecay.MC.NoMaster.sub$LeakRateOffseted <= 6 & dt.AirDecay.MC.NoMaster.sub$LeakRateOffseted >= -6, OffsetResult := as.factor("PASS")]

# Save File
saveRDS(dt.AirDecay.MC.NoMaster.sub, "DataOutput/dt.AirDecay.MC.OffsetSimulation.RDS")

dt.sum.MC.NoMaster.sub <- readRDS("DataOutput/dt.AirDecay.MC.OffsetSimulation.RDS")

# Compare reject rate
dt.sum.MC.NoMaster.sub <- dt.AirDecay.MC.NoMaster.sub %>%
                                  # group_by(Date = as.Date(LeakTestDateTime, tz = "Australia/Melbourne")) %>%
                                  summarize(Qty = n(),
                                            OriginalNGQty = sum(Result=="FAIL"),
                                            OriginalRejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                            OffsetNGQty = sum(OffsetResult=="FAIL"),
                                            OffsetRejectPercent = (sum(OffsetResult=="FAIL") / Qty)*100
                                  )



# # Create time series objects
# dt.Master.MC <- dt.AirDecay.MC.Master[dt.AirDecay.MC.Master$part_id == "XBA1601290101A23", c("LeakTestDateTime", "air_decay_mc")]
# # dt.ts$y <- log(dt.ts$air_decay_mc+6)
# # prophet.ts <- prophet(dt.ts)
# 
# qic(y = dt.ts$air_decay_mc, x= dt.ts$LeakTestDateTime,
#     chart = 'i',
#     main  = 'I-MR Chart - OK Master Leak Rate (Air MC)',
#     ylab  = 'Test Date/Time',
#     xlab  = 'Leak Rate')





