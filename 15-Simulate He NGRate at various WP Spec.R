# This is to simulate He Reject Rate by changing Air Decay WP Spec

rm(list=ls())

source("ExploratoryAnalysisMoudle.R")

# Install & load required packages
Required_Packages=c("openxlsx", "data.table", "splitstackshape", "dplyr","tidyr", "lubridate","ggplot2", "scales", "plotly","mlr", "psych", "MASS","qicharts", "ggpubr", "prophet")

Install_And_Load(Required_Packages)

# Set system time zone of R
Sys.setenv(TZ="Australia/Melbourne")


# Load dataset processed at "2-ExploratoryAnalysis-EffectsOfTimeRelationBetweenStations.R"
dt.AirDecay.WP.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.WP.NoMaster.TBM.RDS")
dt.AirDecay.MC.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.MC.NoMaster.TBM.RDS")
dt.AirDecay.He.NoMaster.TBM <- readRDS("DataOutput/dt.AirDecay.He.NoMaster.TBM.RDS")

# Take latest record of each barcode
dt.WP.Latest <- KeepLatestRecord.AirDecay(dt.AirDecay.WP.NoMaster.TBM)
dt.MC.Latest <- KeepLatestRecord.AirDecay(dt.AirDecay.MC.NoMaster.TBM)
dt.He.Latest <- KeepLatestRecord.He(dt.AirDecay.He.NoMaster.TBM)

rm(dt.AirDecay.WP.NoMaster.TBM, dt.AirDecay.MC.NoMaster.TBM, dt.AirDecay.He.NoMaster.TBM)

# mergae WP result into He result
dt.He.AirDecay.Latest <- merge(dt.He.Latest, dt.WP.Latest[,c("part_id","air_decay_wp","ad_wp_pass_fail")], by.x = "part_id", by.y="part_id", all.x = TRUE)

dt.He.AirDecay.Latest$WP_Roundup <- round(dt.He.AirDecay.Latest$air_decay_wp,1)

# Remove NAs
dt.He.AirDecay.Latest <- dt.He.AirDecay.Latest[ !is.na(air_decay_wp), ]

# Remove outliners
dt.He.AirDecay.Latest <- dt.He.AirDecay.Latest[ air_decay_wp < max(air_decay_wp), ]
# dt.He.AirDecay.Latest <- dt.He.AirDecay.Latest[ helium_test < max(helium_test), ]

dt.sum.HevsWP <- dt.He.AirDecay.Latest %>% group_by(WP_Roundup) %>%
                            summarize(qty = n(),He_NGRate = sum(Result=="FAIL")/n(), NGQty=sum(Result=="FAIL"))

# cor(dt.sum.HevsWP$WP_Roundup, dt.sum.HevsWP$He_NGRate)

ggscatter(dt.sum.HevsWP, x = "WP_Roundup", y = "He_NGRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave. Leak Rate Air Decay WP", ylab = "Helium Reject Rate")

# remove group which sample size less than 30pcs
dt.sum.HevsWP1 <- dt.sum.HevsWP[ dt.sum.HevsWP$qty>=30,]

# Correlation plot for large size group
ggscatter(dt.sum.HevsWP1, x = "WP_Roundup", y = "He_NGRate", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ave. Leak Rate Air Decay WP", ylab = "Helium Reject Rate")

# #Copy to clipboard
write.table(dt.sum.HevsWP, "clipboard", sep="\t", row.names=FALSE)

################################################################################

## Study the relationship between He Reject Rate & response time
dt.He.AirDecay.Latest$ResponseDay <- round(difftime(dt.He.AirDecay.Latest$LeakTestDateTime, dt.He.AirDecay.Latest$CastDateTime,   units = c("days")),1)

# subset data only from 2018, add daylight saving
dt.He.AirDecay.Latest.2018<- dt.He.AirDecay.Latest[year(LeakTestDateTime)==2018,]
dt.He.AirDecay.Latest.2018$ResponseDay <- round((dt.He.AirDecay.Latest.2018$ResponseDay + 1/24),1)

dt.sumDaily.HeNGRate <- dt.He.AirDecay.Latest.2018 %>% group_by(ResponseDay) %>% 
                          summarize(qty = n(),He_NGRate = sum(Result=="FAIL")/n(), NGQty=sum(Result=="FAIL"))

dt.sumDaily.HeNGRate <- dt.sumDaily.HeNGRate[dt.sumDaily.HeNGRate$ResponseDay>=-0.5, ]

plot(dt.sumDaily.HeNGRate$ResponseDay, dt.sumDaily.HeNGRate$He_NGRate )




