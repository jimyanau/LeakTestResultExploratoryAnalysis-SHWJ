# Here is to summarize the reject rate of dataset by MC & Die


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




### Box Plot the leak rate of Air Decay WP by MC/Die #
# add column for month
dt.AirDecay.WP.NoMaster.Latest$Month <- reorder(format(dt.AirDecay.WP.NoMaster.Latest$LeakTestDateTime,'%b-%y'),dt.AirDecay.WP.NoMaster.Latest$LeakTestDateTime)
# Plot Boxplot
ggplot(dt.AirDecay.WP.NoMaster.Latest, aes(x=Month, y=air_decay_wp, fill=CastDie)) +
  scale_y_continuous(limits=c(-1, 1), expand = c(0, 0))+
  geom_boxplot() +
  # geom_jitter(width = .2) +
  xlab("Month") +
  ylab("Leak Rate") +
  scale_fill_brewer(palette="Accent")+
  ggtitle(paste("SH WJ Air Decay Leak Test (WP) Monthly Trend - Die/Cast MC Comparison" )) +
  facet_wrap(~as.factor(CastMC), nrow=2) + 
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")


### Box Plot the leak rate of Air Decay MC by MC/Die #
# add column for month
dt.AirDecay.MC.NoMaster.Latest$Month <- reorder(format(dt.AirDecay.MC.NoMaster.Latest$LeakTestDateTime,'%b-%y'),dt.AirDecay.MC.NoMaster.Latest$LeakTestDateTime)
# Plot Boxplot with log transformation
ggplot(dt.AirDecay.MC.NoMaster.Latest, aes(x=Month, y=air_decay_mc, fill=CastDie)) +
  scale_y_continuous(limits=c(-5, 5), expand = c(0, 0))+
  # geom_boxplot(varwidth=TRUE, alpha=0.3) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 6, colour="USL"),  size=1) +
  geom_hline(aes(yintercept = -6, colour="LSL"),  size=1) +
  # geom_jitter(width = .2) +
  xlab("Month") +
  ylab("Leak Rate") +
  scale_fill_brewer(palette="Accent")+
  ggtitle(paste("SH WJ Air Decay Leak Test (MC) Monthly Trend - Die/Cast MC Comparison" )) +
  facet_wrap(~as.factor(CastMC), nrow=2) + 
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")


### Box Plot the leak rate of Helium by MC/Die #
# add column for month
dt.AirDecay.He.NoMaster.Latest$Month <- reorder(format(dt.AirDecay.He.NoMaster.Latest$LeakTestDateTime,'%b-%y'),dt.AirDecay.He.NoMaster.Latest$LeakTestDateTime)
# Convert Helium reading to ppm
dt.AirDecay.He.NoMaster.Latest$He.ppm <- dt.AirDecay.He.NoMaster.Latest$helium_test*1000000

# Plot Boxplot
ggplot(dt.AirDecay.He.NoMaster.Latest, aes(x=Month, y=He.ppm, fill=CastDie)) +
  scale_y_continuous(limits=c(0.5, 0.52), expand = c(0, 0))+
  geom_boxplot() +
  # geom_jitter(width = .2) +
  xlab("Month") +
  ylab("Leak Rate") +
  scale_fill_brewer(palette="Accent")+
  ggtitle(paste("SH WJ Helium Leak Test Monthly Trend - Die/Cast MC Comparison" )) +
  facet_wrap(~as.factor(CastMC), nrow=2) + 
  theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")


###########################################################################
###########################################################################
###########################################################################
## Do Monthly statics by Mc & Die

# Monthly descriptive statics of Air Decay Test - WP by MC & Die
dt.MonthlyStat.Air.MC.WP <- dt.AirDecay.WP.NoMaster.Latest %>%
                            group_by(Month = Month, CastMC = CastMC, Die = CastDie) %>%
                            summarize(Qty = n(),
                                      RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                      Avg.LeakRate = mean(air_decay_wp), 
                                      Stdev.LeakRate = sd(air_decay_wp),
                                      Max.LeakRate = max(air_decay_wp), 
                                      Min.LeakRate = min(air_decay_wp),
                                      Range.LeakRate = (Max.LeakRate - Min.LeakRate)  )

# Remvoe subgroup with sample size < 30
dt.MonthlyStat.Air.MC.WP <- dt.MonthlyStat.Air.MC.WP[dt.MonthlyStat.Air.MC.WP$Qty>30,]


## Plot line chart of average reject rate
g.Monthly.NGRate.WP <- ggplot(dt.MonthlyStat.Air.MC.WP, aes(x= Month, y=RejectPercent, group = Die, colour=Die, shape=Die)) + 
                    # scale_y_continuous(breaks=1:10,limits=c(0, 10), expand = c(0, 0))+
                    geom_line() +
                    geom_point() +
                     geom_text(aes(label=round(RejectPercent,digits=1)),vjust=-.5) +
                    xlab("Month") +
                    ylab("Leak Test Reject Rate (%)") +
                    ggtitle(paste("SH WJ Leak Test (Air Decay WP) Monthly Reject Percentage Comparison" )) +
                    facet_wrap(~as.factor(CastMC), nrow=2) + 
                    theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")


# Monthly descriptive statics of Air Decay Test - MC by MC & Die
dt.MonthlyStat.Air.MC.MC <- dt.AirDecay.MC.NoMaster.Latest %>%
                            group_by(Month = Month, CastMC = CastMC, Die = CastDie) %>%
                            summarize(Qty = n(),
                                      RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                      Avg.LeakRate = mean(air_decay_mc), 
                                      Stdev.LeakRate = sd(air_decay_mc),
                                      Max.LeakRate = max(air_decay_mc), 
                                      Min.LeakRate = min(air_decay_mc),
                                      Range.LeakRate = (Max.LeakRate - Min.LeakRate)  )

# Remvoe subgroup with sample size < 30
dt.MonthlyStat.Air.MC.MC <- dt.MonthlyStat.Air.MC.MC[dt.MonthlyStat.Air.MC.MC$Qty>30,]


## Plot line chart of average reject rate
g.Monthly.NGRate.MC <- ggplot(dt.MonthlyStat.Air.MC.MC, aes(x= Month, y=RejectPercent, group = Die, colour=Die, shape=Die)) + 
                            # scale_y_continuous(breaks=1:10,limits=c(0, 10), expand = c(0, 0))+
                            geom_line() +
                            geom_point() +
                            geom_text(aes(label=round(RejectPercent,digits=1)),vjust=-.5) +
                            xlab("Month") +
                            ylab("Leak Test Reject Rate (%)") +
                            ggtitle(paste("SH WJ Leak Test (Air Decay MC) Monthly Reject Percentage Comparison" )) +
                            facet_wrap(~as.factor(CastMC), nrow=2) + 
                            theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")


# Monthly descriptive statics of Helium Test - MC by MC & Die
dt.MonthlyStat.Air.MC.He <- dt.AirDecay.He.NoMaster.Latest %>%
                              group_by(Month = Month, CastMC = CastMC, Die = CastDie) %>%
                              summarize(Qty = n(),
                                        RejectPercent = (sum(Result=="FAIL") / Qty)*100,
                                        Avg.LeakRate = mean(He.ppm), 
                                        Stdev.LeakRate = sd(He.ppm),
                                        Max.LeakRate = max(He.ppm), 
                                        Min.LeakRate = min(He.ppm),
                                        Range.LeakRate = (Max.LeakRate - Min.LeakRate)  )

# Remvoe subgroup with sample size < 30
dt.MonthlyStat.Air.MC.He <- dt.MonthlyStat.Air.MC.He[dt.MonthlyStat.Air.MC.He$Qty>30,]


## Plot line chart of average reject rate
g.Monthly.NGRate.He <- ggplot(dt.MonthlyStat.Air.MC.He, aes(x= Month, y=RejectPercent, group = Die, colour=Die, shape=Die)) + 
                                    # scale_y_continuous(breaks=1:10,limits=c(0, 10), expand = c(0, 0))+
                                    geom_line() +
                                    geom_point() +
                                    geom_text(aes(label=round(RejectPercent,digits=1)),vjust=-.5) +
                                    xlab("Month") +
                                    ylab("Leak Test Reject Rate (%)") +
                                    ggtitle(paste("SH WJ Leak Test (Helium) Monthly Reject Percentage Comparison" )) +
                                    facet_wrap(~as.factor(CastMC), nrow=2) + 
                                    theme(text = element_text(size=15), axis.text.x = element_text(angle = 90),legend.position="bottom")


