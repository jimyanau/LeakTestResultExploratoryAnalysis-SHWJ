## Here is to summarize the reject rate of dataset


rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


########## Preprocess Data                               ########################################
########## Only do this when your data source is changed ########################################
## Load data processed in 0-ExtractCleanData-LeakTestStation.R
dt.AirDecay.WP.NoMaster <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.RDS")
dt.AirDecay.MC.NoMaster <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.RDS")
dt.AirDecay.He.NoMaster <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.RDS")


## keep only the latest record of each part id for reject calculation
dt.AirDecay.WP.NoMaster.Latest <- KeepLatestRecord.AirDecay(dt.AirDecay.WP.NoMaster)
dt.AirDecay.MC.NoMaster.Latest <- KeepLatestRecord.AirDecay(dt.AirDecay.MC.NoMaster)
dt.AirDecay.He.NoMaster.Latest <- KeepLatestRecord.He(dt.AirDecay.He.NoMaster)

## Clean temp datasets
rm(dt.AirDecay.WP.NoMaster, dt.AirDecay.MC.NoMaster, dt.AirDecay.He.NoMaster)

## Save file for furture use
saveRDS(dt.AirDecay.WP.NoMaster.Latest, "DataOutput/dt.AirDecay.WP.NoMaster.Latest.RDS")
saveRDS(dt.AirDecay.MC.NoMaster.Latest, "DataOutput/dt.AirDecay.MC.NoMaster.Latest.RDS")
saveRDS(dt.AirDecay.He.NoMaster.Latest, "DataOutput/dt.AirDecay.He.NoMaster.Latest.RDS")
#################################################################################################
#################################################################################################


## Load data processed previously
dt.AirDecay.WP.NoMaster.Latest <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.Latest.RDS")
dt.AirDecay.MC.NoMaster.Latest <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.Latest.RDS")
dt.AirDecay.He.NoMaster.Latest <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.Latest.RDS")

# Monthly descriptive statics of Air Decay Test - WP
dt.MonthlyStat.Air.WP <- dt.AirDecay.WP.NoMaster.Latest %>%
                          group_by(Date = floor_date(LeakTestDateTime, "1 month")) %>%
                          # group_by(Month = format(floor_date(LeakTestDateTime, "1 month"), "%b-%y")) %>%
                          summarize(Qty = n(),
                                    Month = format(min(Date), "%b-%y"),
                                    RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                    Avg.LeakRate = mean(air_decay_wp), 
                                    Stdev.LeakRate = sd(air_decay_wp),
                                    Max.LeakRate = max(air_decay_wp), 
                                    Min.LeakRate = min(air_decay_wp),
                                    Range.LeakRate = (Max.LeakRate - Min.LeakRate)  )
            

# Monthly descriptive statics of Air Decay Test - MC
dt.MonthlyStat.Air.MC <- dt.AirDecay.MC.NoMaster.Latest %>%
                              group_by(Date = floor_date(LeakTestDateTime, "1 month")) %>%
                              # group_by(Month = format(floor_date(LeakTestDateTime, "1 month"), "%b-%y")) %>%
                              summarize(Qty = n(),
                                        Month = format(min(Date), "%b-%y"),
                                        RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                        Avg.LeakRate = mean(air_decay_mc), 
                                        Stdev.LeakRate = sd(air_decay_mc),
                                        Max.LeakRate = max(air_decay_mc), 
                                        Min.LeakRate = min(air_decay_mc),
                                        Range.LeakRate = (Max.LeakRate - Min.LeakRate)  )


# Monthly descriptive statics of Air Decay Test - MC
dt.MonthlyStat.Air.He <- dt.AirDecay.He.NoMaster.Latest %>%
                          group_by(Date = floor_date(LeakTestDateTime, "1 month")) %>%
                          # group_by(Month = format(floor_date(LeakTestDateTime, "1 month"), "%b-%y")) %>%
                          summarize(Qty = n(),
                                    Month = format(min(Date), "%b-%y"),
                                    RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                    Avg.LeakRate = mean(helium_test), 
                                    Stdev.LeakRate = sd(helium_test),
                                    Max.LeakRate = max(helium_test), 
                                    Min.LeakRate = min(helium_test),
                                    Range.LeakRate = (Max.LeakRate - Min.LeakRate)  )


## Combine statics of 3 tests
dt.MonthlyStat.Combined <- NULL

# Combine Air WP & Air MC
dt.MonthlyStat.Combined <- merge(dt.MonthlyStat.Air.WP, dt.MonthlyStat.Air.MC, by.x = "Month", by.y = "Month", all.x = TRUE)

# Rename columns
colnames(dt.MonthlyStat.Combined) <- sub(".x$", ".WP", colnames(dt.MonthlyStat.Combined))
colnames(dt.MonthlyStat.Combined) <- sub(".y$", ".MC", colnames(dt.MonthlyStat.Combined))

## Becareful, have to run the code form beginning. Otherwise, there will be error on column names while merging datasets
colnames(dt.MonthlyStat.Air.He) <- sub("$", ".He", colnames(dt.MonthlyStat.Air.He))

# Combine He test data into dataset
dt.MonthlyStat.Combined <- merge(dt.MonthlyStat.Combined, dt.MonthlyStat.Air.He, by.x = "Month", by.y = "Month.He", all.x = TRUE)

dt.MonthlyStat.Combined <- dt.MonthlyStat.Combined[order(dt.MonthlyStat.Combined$Date.WP, decreasing = FALSE),]

## Plot line chart of average reject rate
g.Monthly.NGRate <- ggplot(dt.MonthlyStat.Combined, aes(x= Date.WP)) + 
                          geom_line(aes(y = RejectPercent.WP, linetype = "RejectPercent.WP"), colour='blue' ,size=1) + 
                          geom_line(aes(y = RejectPercent.MC,linetype = "RejectPercent.MC"), colour = 'orange', size=1) +
                          geom_line(aes(y = RejectPercent.He, linetype = "RejectPercent.He"), colour='red', size=1) +
                          scale_y_continuous(breaks=1:10,limits=c(0, 10), expand = c(0, 0))+
                          geom_text(aes(y = RejectPercent.WP,label=round(RejectPercent.WP,digits=1)), colour='blue' ,vjust=-.5) +
                          geom_text(aes(y = RejectPercent.MC,label=round(RejectPercent.MC,digits=1)), colour='orange' ,vjust=-.5) +
                          geom_text(aes(y = RejectPercent.He,label=round(RejectPercent.He,digits=1)), colour='red' ,vjust=-.5) +
                          xlab("Month") +
                          ylab("Leak Test Reject Rate (%)") +
                          ggtitle(paste("SH WJ Leak Test Monthly Reject Percentage" )) +
                          scale_x_datetime(date_breaks = "1 month", labels = date_format("%b-%y")) +
                          theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")


# ## Box Plot the leak rate by Die #

# add column for month
dt.AirDecay.WP.NoMaster.Latest$Month <- reorder(format(dt.AirDecay.WP.NoMaster.Latest$LeakTestDateTime,'%b-%y'),dt.AirDecay.WP.NoMaster.Latest$LeakTestDateTime)

ggplot(dt.AirDecay.WP.NoMaster.Latest[air_decay_wp<max(air_decay_wp)], aes(x=Month, y=air_decay_wp, fill=CastDie)) +
  scale_y_continuous(limits=c(-1, 1), expand = c(0, 0))+
  # scale_x_datetime(date_breaks = "1 month", labels = date_format("%b-%y")) +
  geom_boxplot() +
  # geom_jitter(width = .2) +
  xlab("Month") +
  ylab("Leak Rate") +
  scale_fill_brewer(palette="Dark1")+
  ggtitle(paste("SH WJ Air Decay Leak Test (WP) Monthly Trend - Die Comparison" )) +
  facet_wrap(~as.factor(CastMC), nrow=2) + 
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")



