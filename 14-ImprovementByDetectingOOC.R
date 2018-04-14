# This is to calculate improvement value by capturing dates which OK Master was out of control

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



# subset data from 2017
dt.AirDecay.WP.NoMaster.sub <- dt.AirDecay.WP.NoMaster.TBM[dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime >= as.Date("2017-01-01") &
                                                             dt.AirDecay.WP.NoMaster.TBM$LeakTestDateTime < as.Date("2018-01-01"), ]

dt.AirDecay.MC.NoMaster.sub <- dt.AirDecay.MC.NoMaster.TBM[dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime >= as.Date("2017-01-01") &
                                                             dt.AirDecay.MC.NoMaster.TBM$LeakTestDateTime < as.Date("2018-01-01"), ]

dt.MC.OKMaster <- dt.AirDecay.MC.Master[dt.AirDecay.MC.Master$part_id == "XBA1601290101A23" ,  ]

dt.WP.OKMaster <- dt.AirDecay.WP.Master[dt.AirDecay.WP.Master$part_id == "XBA1601290101A23"   ,  ]

# clean memory
rm(dt.AirDecay.WP.NoMaster.TBM, dt.AirDecay.MC.NoMaster.TBM)


#######################################################################
# Check the datesOK Master out of control
# The control limit got from "4-EstablishControlLimit-AirDecay-OKMaster.R"
Mean.LeakRate.WP.Master <- 0.1216667
SD.LeakRate.WP.Master <- 0.2504854

Mean.LeakRate.MC.Master <- 0.5116667
SD.LeakRate.MC.Master <- 1.252941

#Calculate the date which OK Master was out of control
dt.DailyStat.MC.OKMaster <- dt.MC.OKMaster %>%
                            group_by(Period = as.Date(LeakTestDateTime, tz = "Australia/Melbourne")) %>%
                            summarize(Qty = n(),
                                      Avg.LeakRate = mean(air_decay_mc),
                                      Stdev.LeakRate = sd(air_decay_mc),
                                      Max.LeakRate = max(air_decay_mc),
                                      Min.LeakRate = min(air_decay_mc),
                                      Range.LeakRate = (Max.LeakRate - Min.LeakRate)) %>%
                            mutate(OOC_1SD = ifelse(Avg.LeakRate>Mean.LeakRate.MC.Master+SD.LeakRate.MC.Master | Avg.LeakRate<Mean.LeakRate.MC.Master-SD.LeakRate.MC.Master,TRUE, FALSE),
                                   OOC_2SD = ifelse(Avg.LeakRate>Mean.LeakRate.MC.Master+2*SD.LeakRate.MC.Master | Avg.LeakRate<Mean.LeakRate.MC.Master-2*SD.LeakRate.MC.Master,TRUE, FALSE),
                                   OOC_3SD = ifelse(Avg.LeakRate>Mean.LeakRate.MC.Master+3*SD.LeakRate.MC.Master | Avg.LeakRate<Mean.LeakRate.MC.Master-3*SD.LeakRate.MC.Master,TRUE, FALSE))

# get the daily summary of production data
dt.DailyStat.MC.sub <- Daily.Statics.AirDecay.MC(dt.AirDecay.MC.NoMaster.sub, -6,6)

# Merge OK Master result into daily production summary
dt.combined.MC <- merge(dt.DailyStat.MC.sub, dt.DailyStat.MC.OKMaster, by.x = "Date", by.y= "Period", all.x = TRUE)

# Rename col
colnames(dt.combined.MC) <- gsub("\\.y$",".OKMaster",colnames(dt.combined.MC))
# setnames(dt.combined.MC, "LeakTestDateTime", "1st_AirDecay_DateTime")

sum <- dt.combined.MC %>%
        group_by(OOC_1SD) %>%
        summarize( TestQty = sum(Qty.x), NGQty = sum(NGQty), RejectRate = sum(NGQty)/sum(Qty.x), Days = n() )

# #Copy to clipboard
# write.table(dt.combined.MC, "clipboard", sep="\t", row.names=FALSE)
# write.table(dt.DailyStat.MC.OKMaster, "clipboard", sep="\t", row.names=FALSE)













