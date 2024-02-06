library(foreign)
library(sf)
library(dplyr)
library(lubridate)

setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Supplement/Supplemental Tables 7 & 8")

GMRt_final <- read.csv(file="GMRt_final.csv")
GMRb_final <- read.csv(file="GMRb_final.csv")
GMRw_final <- read.csv(file="GMRw_final.csv")
sib_final <- read.csv(file="sib_final.csv")
siw_final <- read.csv(file="siw_final.csv")
sob_final <- read.csv(file="sob_final.csv")
sow_final <- read.csv(file="sow_final.csv")

### Cluster Characteristics

GMRt = GMRt_final %>%
  group_by(year) %>%
  summarise(radius_avg = mean(RADIUS),
            months_avg = mean(months))

GMRb = GMRb_final %>%
  group_by(year) %>%
  summarise(radius_avg = mean(RADIUS),
            months_avg = mean(months))

GMRw = GMRw_final %>%
  group_by(year) %>%
  summarise(radius_avg = mean(RADIUS),
            months_avg = mean(months))

sib = sib_final %>%
  group_by(year) %>%
  summarise(radius_avg = mean(RADIUS),
            months_avg = mean(months))

siw = siw_final %>%
  group_by(year) %>%
  summarise(radius_avg = mean(RADIUS),
            months_avg = mean(months))

sob = sob_final %>%
  group_by(year) %>%
  summarise(radius_avg = mean(RADIUS),
            months_avg = mean(months))

sow = sow_final %>%
  group_by(year) %>%
  summarise(radius_avg = mean(RADIUS),
            months_avg = mean(months))


### Number of clusters
GMRt_final$count <- 1
GMRw_final$count <- 1
GMRb_final$count <- 1
sow_final$count <- 1
sob_final$count <- 1
siw_final$count <- 1
sib_final$count <- 1

GMRt2 <- GMRt_final %>%
  group_by(year) %>%
  summarise(num_clusters=length(unique(CLUSTER)))

GMRb2 <- GMRb_final %>%
  group_by(year) %>%
  summarise(num_clusters=length(unique(CLUSTER)))

GMRw2 <- GMRw_final %>%
  group_by(year) %>%
  summarise(num_clusters=length(unique(CLUSTER)))

sib2 <- sib_final %>%
  group_by(year) %>%
  summarise(num_clusters=length(unique(CLUSTER)))

siw2 <- siw_final %>%
  group_by(year) %>%
  summarise(num_clusters=length(unique(CLUSTER)))

sob2 <- sob_final %>%
  group_by(year) %>%
  summarise(num_clusters=length(unique(CLUSTER)))

sow2 <- sow_final %>%
  group_by(year) %>%
  summarise(num_clusters=length(unique(CLUSTER)))

gmrt <- merge(GMRt,GMRt2,by="year")
gmrb <- merge(GMRb,GMRb2,by="year")
gmrw <- merge(GMRw,GMRw2,by="year")
sob <- merge(sob,sob2,by="year")
sow <- merge(sow,sow2,by="year")
sib <- merge(sib,sib2,by="year")
siw <- merge(siw,siw2,by="year")

setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Supplement/Supplemental Tables 7 & 8")
write.csv(gmrt,file="gmrt_tabledata.csv")
write.csv(gmrb,file="gmrb_tabledata.csv")
write.csv(gmrw,file="gmrw_tabledata.csv")
write.csv(sib,file="sib_tabledata.csv")
write.csv(siw,file="siw_tabledata.csv")
write.csv(sob,file="sob_tabledata.csv")
write.csv(sow,file="sow_tabledata.csv")





