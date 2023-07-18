install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(foreign)
install.packages("zoo")
library(zoo)
install.packages("tidyr")
library(tidyr)
library(data.table)

setwd("/n/holyscratch01/mcastro_lab/Users/nicholasarisco/Nat Comms Mobility Paper -- Current Analyses/Main Text -- Figure 2/Data")

sib <- read.csv(file="si_b_strength_with_sinan.csv")
siw <- read.csv(file="si_w_strength_with_sinan.csv")
sob <- read.csv(file="so_b_strength_with_sinan.csv")
sow <- read.csv(file="so_w_strength_with_sinan.csv")

colnames(siw)[4] <- "sink_w"
colnames(sib)[4] <- "sink_b"
colnames(sow)[4] <- "source_w"
colnames(sob)[4] <- "source_b"

sink <- merge(siw,sib,by=c("mun","date"),all=T)
sink$imported <- sink$sink_b+sink$sink_w
sink <- subset(sink,select=c("mun","date","imported"))
source <- merge(sow,sob,by=c("mun","date"),all=T)
source$exported <- source$source_b+source$source_w
source <- subset(source,select=c("mun","date","exported"))

movement <- merge(sink,source,by=c("mun","date"),all=T)
movement[is.na(movement)] <- 0
movement$date <- (as.Date(movement$date))
movement$year <- year(movement$date)

movement_year <- movement %>%
  group_by(mun,year) %>%
  summarise(imported=sum(imported),
            exported=sum(exported))

movement_year$uf <- substr(movement_year$mun,1,2)
summary(movement_year[movement_year$uf%in%c(22:50),])

write.csv(movement_year,file="/n/holyscratch01/mcastro_lab/Users/nicholasarisco/Nat Comms Mobility Paper -- Current Analyses/Main Text -- Figure 2/Figure 2 Data.csv")
