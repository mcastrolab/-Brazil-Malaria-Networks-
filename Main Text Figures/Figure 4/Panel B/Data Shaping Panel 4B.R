library(reshape2)
library(dplyr)
library(lubridate)

#Degree
sink_wd <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/si_w_degree_with_sinan.csv")
sink_bd <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/si_b_degree_with_sinan.csv")
source_wd <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/so_w_degree_with_sinan.csv")
source_bd <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/so_b_degree_with_sinan.csv")

sink_wd$year <- year(sink_wd$date)
sink_bd$year <- year(sink_bd$date)
source_wd$year <- year(source_wd$date)
source_bd$year <- year(source_bd$date)

sink_wd <- sink_wd[sink_wd$year>2003,]
sink_bd <- sink_bd[sink_bd$year>2003,]
source_wd <- source_wd[source_wd$year>2003,]
source_bd <- source_bd[source_bd$year>2003,]

sink_wd$uf <- substr(sink_wd$mun,1,2)
sink_bd$uf <- substr(sink_bd$mun,1,2)
source_wd$uf <- substr(source_wd$mun,1,2)
source_bd$uf <- substr(source_bd$mun,1,2)

si_w2 <- sink_wd %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

so_w2 <- source_wd %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

si_b2 <- sink_bd %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

so_b2 <- source_bd %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

sinkd <- merge(si_b2,si_w2,by=c("mun","variable"),all=T)
sourced <- merge(so_b2,so_w2,by=c("mun","variable"),all=T)

sinkd[is.na(sinkd)] <- 0
sourced[is.na(sourced)] <- 0

sinkd$total <- sinkd$count.x+sinkd$count.y
sourced$total <- sourced$count.x+sourced$count.y

sink_mean <- sinkd %>% 
  group_by(mun) %>%
  summarize(mean_in = mean(total), 
            sd_in = sd(total), 
            cv_in = mean(total)/sd(total))

source_mean <- sourced %>% 
  group_by(mun) %>%
  summarize(mean_out = mean(total), 
            sd_out = sd(total), 
            cv_out = mean(total)/sd(total))

# # quantile(na.omit(source_mean$mean_in),.975)
# quantile(na.omit(sink_mean$mean_in),.975)
# quantile(na.omit(source_mean$mean_out),.975)
# # quantile(na.omit(sink_mean$mean_out),.975)

# so_in_top <- source_mean[source_mean$mean_in<4.65651,]
# so_out_top <- source_mean[source_mean$mean_out<2.425,]
# si_in_top <- sink_mean[sink_mean$mean_in<5.426042 ,]
# si_out_top <- sink_mean[sink_mean$mean_out<2.332292,]

#Strength
sink_ws <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/si_w_strength_with_sinan.csv")
sink_bs <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/si_b_strength_with_sinan.csv")
source_ws <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/so_w_strength_with_sinan.csv")
source_bs <- read.csv("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Network data to input into Node_Types.R/so_b_strength_with_sinan.csv")

sink_ws$year <- year(sink_ws$date)
sink_bs$year <- year(sink_bs$date)
source_ws$year <- year(source_ws$date)
source_bs$year <- year(source_bs$date)

sink_ws <- sink_ws[sink_ws$year>2003,]
sink_bs <- sink_bs[sink_bs$year>2003,]
source_ws <- source_ws[source_ws$year>2003,]
source_bs <- source_bs[source_bs$year>2003,]

sink_ws$uf <- substr(sink_ws$mun,1,2)
sink_bs$uf <- substr(sink_bs$mun,1,2)
source_ws$uf <- substr(source_ws$mun,1,2)
source_bs$uf <- substr(source_bs$mun,1,2)

si_w2s <- sink_ws %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

so_w2s <- source_ws %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

si_b2s <- sink_bs %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

so_b2s <- source_bs %>%
  group_by(uf,mun,variable) %>%
  summarize(count=sum(value))

sinks <- merge(si_b2s,si_w2s,by=c("mun","variable"),all=T)
sources <- merge(so_b2s,so_w2s,by=c("mun","variable"),all=T)

sinks[is.na(sinks)] <- 0
sources[is.na(sources)] <- 0

sinks$total <- sinks$count.x+sinks$count.y
sources$total <- sources$count.x+sources$count.y

sink_means <- sinks %>% 
  group_by(mun) %>%
  summarize(mean_in_S = mean(total), 
            sd_in_S = sd(total), 
            cv_in_S = mean(total)/sd(total))

source_means <- sources %>% 
  group_by(mun) %>%
  summarize(mean_out_S = mean(total), 
            sd_out_S = sd(total), 
            cv_out_S = mean(total)/sd(total))

### Designations ###
sinkf <- na.omit(merge(sink_means,sink_mean,by="mun",all=T))
sourcef <- na.omit(merge(source_means,source_mean,by="mun",all=T))



## Type A ##
sink_A <- sinkf[sinkf$mean_in_S>=quantile(na.omit(sinkf$mean_in_S),.975)&
                  sinkf$mean_in>=quantile(na.omit(sinkf$mean_in),.975),]
source_A <- sourcef[sourcef$mean_out_S>=quantile(na.omit(sourcef$mean_out_S),.975)&
                  sourcef$mean_out>=quantile(na.omit(sourcef$mean_out),.975),]

## Type B ##
sink_B <- sinkf[sinkf$mean_in_S<quantile(na.omit(sinkf$mean_in_S),.975)&
                  sinkf$mean_in>=quantile(na.omit(sinkf$mean_in),.975),]
source_B <- sourcef[sourcef$mean_out_S<quantile(na.omit(sourcef$mean_out_S),.975)&
                      sourcef$mean_out>=quantile(na.omit(sourcef$mean_out),.975),]

## Type C ##
sink_C <- sinkf[sinkf$mean_in_S>=quantile(na.omit(sinkf$mean_in_S),.975)&
                  sinkf$mean_in<quantile(na.omit(sinkf$mean_in),.975),]
source_C <- sourcef[sourcef$mean_out_S>=quantile(na.omit(sourcef$mean_out_S),.975)&
                      sourcef$mean_out<quantile(na.omit(sourcef$mean_out),.975),]

## Type D ##
sink_D <- sinkf[sinkf$mean_in_S<quantile(na.omit(sinkf$mean_in_S),.975)&
                  sinkf$mean_in<quantile(na.omit(sinkf$mean_in),.975),]
source_D <- sourcef[sourcef$mean_out_S<quantile(na.omit(sourcef$mean_out_S),.975)&
                      sourcef$mean_out<quantile(na.omit(sourcef$mean_out),.975),]

sink_A$type <- "A"
sink_B$type <- "B"
sink_C$type <- "C"
sink_D$type <- "D"

source_A$type <- "A"
source_B$type <- "B"
source_C$type <- "C"
source_D$type <- "D"

Fig2sink <- rbind(sink_A,sink_B,sink_C,sink_D)
Fig2source <- rbind(source_A,source_B,source_C,source_D)

write.csv(Fig2sink,file="C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Output from Node_Types.R/Node typology results -- sink.csv")
write.csv(Fig2source,file="C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Typology/Output from Node_Types.R/Node typology results -- source.csv")

