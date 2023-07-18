library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
install.packages("forecast")
library(forecast)
library(tidyr)
library(data.table)

sink_ws <- read.csv("C:/Users/Nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Data/Final Network Data/si_w.csv")
sink_bs <- read.csv("C:/Users/Nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Data/Final Network Data/si_b.csv")
source_ws <- read.csv("C:/Users/Nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Data/Final Network Data/so_w.csv")
source_bs <- read.csv("C:/Users/Nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Data/Final Network Data/so_b.csv")

sink_ws$uf <- substr(sink_ws$mun,1,2)
sink_bs$uf <- substr(sink_bs$mun,1,2)
source_ws$uf <- substr(source_ws$mun,1,2)
source_bs$uf <- substr(source_bs$mun,1,2)

si_w2s <- sink_ws %>%
  group_by(uf,mun,date) %>%
  summarize(count=sum(value))

so_w2s <- source_ws %>%
  group_by(uf,mun,date) %>%
  summarize(count=sum(value))

si_b2s <- sink_bs %>%
  group_by(uf,mun,date) %>%
  summarize(count=sum(value))

so_b2s <- source_bs %>%
  group_by(uf,mun,date) %>%
  summarize(count=sum(value))

sinks <- merge(si_b2s,si_w2s,by=c("mun","date","uf"),all=T)
sources <- merge(so_b2s,so_w2s,by=c("mun","date","uf"),all=T)

tsdata <- merge(sinks,sources,by=c("mun","date","uf"),all=T)

colnames(tsdata) <- c("mun","date","uf","sink_between",
                                  "sink_within",
                                  "source_between",
                                  "source_within")
tsdata[is.na(tsdata)] <- 0
tsdata$year <- year(tsdata$date)
tsdata$month <- month(tsdata$date)

tsdata <- tsdata[tsdata$year<2021,]

tsdata2 <- tsdata %>%
  group_by(year,month,mun) %>%
  summarise(sink_between=sum(sink_between),
            sink_within=sum(sink_within),
            source_between=sum(source_between),
            source_within=sum(source_within),
            sink=sum(sink_between+sink_within),
            source=sum(source_between+source_within))

tsdata2$date <- as.Date(as.yearmon(paste(tsdata2$year, tsdata2$month), "%Y %m"))

tsdata2 <- tsdata2[order(tsdata2$date),]

tsdata2$sib_ind <- 0
tsdata2$sib_ind[tsdata2$sink_between>0] <- 1

tsdata2$siw_ind <- 0
tsdata2$siw_ind[tsdata2$sink_within>0] <- 1

tsdata2$sob_ind <- 0
tsdata2$sob_ind[tsdata2$source_between>0] <- 1

tsdata2$sow_ind <- 0
tsdata2$sow_ind[tsdata2$source_within>0] <- 1

tsdata2$source_ind <- 0
tsdata2$source_ind[tsdata2$source>0] <- 1

tsdata2$sink_ind <- 0
tsdata2$sink_ind[tsdata2$sink>0] <- 1

tsdata3 <- tsdata2 %>%
  group_by(mun) %>%
  summarise(sib=sum(sib_ind)/216,
            siw=sum(siw_ind)/216,
            sob=sum(sob_ind)/216,
            sow=sum(sow_ind)/216,
            source=sum(source_ind)/216,
            sink=sum(sink_ind)/216
  )

write.csv(tsdata3,file="C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Figures/Figure 2 Maps/Panel A Data.csv")





