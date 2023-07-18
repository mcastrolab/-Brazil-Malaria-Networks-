library(foreign)
library(sf)
library(dplyr)
library(lubridate)

### SINK BETWEEN ###
setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/SatScan/CURRENT -- SatScan -- 04 to 2022 with SINAN/Output/Sink between")
sib04 <- read_sf("sinkbtw2004.gis.shp")
sib05 <- read_sf("sinkbtw2005.gis.shp")
sib06 <- read_sf("sinkbtw2006.gis.shp")
sib07 <- read_sf("sinkbtw2007.gis.shp")
sib08 <- read_sf("sinkbtw2008.gis.shp")
sib09 <- read_sf("sinkbtw2009.gis.shp")
sib10 <- read_sf("sinkbtw2010.gis.shp")
sib11 <- read_sf("sinkbtw2011.gis.shp")
sib12 <- read_sf("sinkbtw2012.gis.shp")
sib13 <- read_sf("sinkbtw2013.gis.shp")
sib14 <- read_sf("sinkbtw2014.gis.shp")
sib15 <- read_sf("sinkbtw2015.gis.shp")
sib16 <- read_sf("sinkbtw2016.gis.shp")
sib17 <- read_sf("sinkbtw2017.gis.shp")
sib18 <- read_sf("sinkbtw2018.gis.shp")
sib19 <- read_sf("sinkbtw2019.gis.shp")
sib20 <- read_sf("sinkbtw2020.gis.shp")
sib21 <- read_sf("sinkbtw2021.gis.shp")
sib22 <- read_sf("sinkbtw2022.gis.shp")

sib04$year <- 2004
sib05$year <- 2005
sib06$year <- 2006
sib07$year <- 2007
sib08$year <- 2008
sib09$year <- 2009
sib10$year <- 2010
sib11$year <- 2011
sib12$year <- 2012
sib13$year <- 2013
sib14$year <- 2014
sib15$year <- 2015
sib16$year <- 2016
sib17$year <- 2017
sib18$year <- 2018
sib19$year <- 2019
sib20$year <- 2020
sib21$year <- 2021
sib22$year <- 2022

sib <- rbind(sib04,
             sib05,
             sib06,
             sib07,
             sib08,
             sib09,
             sib10,
             sib11,
             sib12,
             sib13,
             sib14,
             sib15,
             sib16,
             sib17,
             sib18,
             sib19,
             sib20,
             sib21,
             sib22)

sib <- sib[sib$P_VALUE<0.05,]

sib_m_04 <- read_sf("sinkbtw2004.col.shp")
sib_m_05 <- read_sf("sinkbtw2005.col.shp")
sib_m_06 <- read_sf("sinkbtw2006.col.shp")
sib_m_07 <- read_sf("sinkbtw2007.col.shp")
sib_m_08 <- read_sf("sinkbtw2008.col.shp")
sib_m_09 <- read_sf("sinkbtw2009.col.shp")
sib_m_10 <- read_sf("sinkbtw2010.col.shp")
sib_m_11 <- read_sf("sinkbtw2011.col.shp")
sib_m_12 <- read_sf("sinkbtw2012.col.shp")
sib_m_13 <- read_sf("sinkbtw2013.col.shp")
sib_m_14 <- read_sf("sinkbtw2014.col.shp")
sib_m_15 <- read_sf("sinkbtw2015.col.shp")
sib_m_16 <- read_sf("sinkbtw2016.col.shp")
sib_m_17 <- read_sf("sinkbtw2017.col.shp")
sib_m_18 <- read_sf("sinkbtw2018.col.shp")
sib_m_19 <- read_sf("sinkbtw2019.col.shp")
sib_m_20 <- read_sf("sinkbtw2020.col.shp")
sib_m_21 <- read_sf("sinkbtw2021.col.shp")
sib_m_22 <- read_sf("sinkbtw2022.col.shp")

sib_m_04$year <- 2004
sib_m_05$year <- 2005
sib_m_06$year <- 2006
sib_m_07$year <- 2007
sib_m_08$year <- 2008
sib_m_09$year <- 2009
sib_m_10$year <- 2010
sib_m_11$year <- 2011
sib_m_12$year <- 2012
sib_m_13$year <- 2013
sib_m_14$year <- 2014
sib_m_15$year <- 2015
sib_m_16$year <- 2016
sib_m_17$year <- 2017
sib_m_18$year <- 2018
sib_m_19$year <- 2019
sib_m_20$year <- 2020
sib_m_21$year <- 2021
sib_m_22$year <- 2022

sib_m <- rbind(sib_m_04,
             sib_m_05,
             sib_m_06,
             sib_m_07,
             sib_m_08,
             sib_m_09,
             sib_m_10,
             sib_m_11,
             sib_m_12,
             sib_m_13,
             sib_m_14,
             sib_m_15,
             sib_m_16,
             sib_m_17,
             sib_m_18,
             sib_m_19,
             sib_m_20,
             sib_m_21,
             sib_m_22)

sib_m <- sib_m[sib_m$P_VALUE<0.05,]

sib_m$END_DATE <- as.Date(sib_m$END_DATE)
sib_m$START_DATE <- as.Date(sib_m$START_DATE)

sib_m$months <- interval(sib_m$START_DATE, sib_m$END_DATE) %/% months(1) 
sib_m$year_grp <- NA
sib_m$year_grp[sib_m$year%in%c(2004:2009)] <- 1
sib_m$year_grp[sib_m$year%in%c(2010:2016)] <- 2
sib_m$year_grp[sib_m$year%in%c(2017:2022)] <- 3

sib_m <- as.data.frame(sib_m)
sib <- as.data.frame(sib)

sib_final <- merge(subset(sib,select=c("LOC_ID","year","CLUSTER")),
                   subset(sib_m,select=c("LOC_ID","year","year_grp","months","CLUSTER")),
                   by=c("year","CLUSTER"),all=T)

sib_final$LOC_ID.y <- NULL
names(sib_final)[3] <- "mun"

sib_grp <- sib_final %>%
  group_by(year_grp,mun) %>%
  summarise(total_months = sum(months))

sib_grp$perc_sig <- NA
sib_grp$perc_sig[sib_grp$year_grp==1] <- sib_grp$total_months[sib_grp$year_grp==1]/(6*12)
sib_grp$perc_sig[sib_grp$year_grp==2] <- sib_grp$total_months[sib_grp$year_grp==2]/(7*12)
sib_grp$perc_sig[sib_grp$year_grp==3] <- sib_grp$total_months[sib_grp$year_grp==3]/(6*12)

sib_grp$type <- "sink_between"

### SINK WITHIN ###
setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/SatScan/CURRENT -- SatScan -- 04 to 2022 with SINAN/Output/Sink within")
siw04 <- read_sf("sinkwtn2004.gis.shp")
siw05 <- read_sf("sinkwtn2005.gis.shp")
siw06 <- read_sf("sinkwtn2006.gis.shp")
siw07 <- read_sf("sinkwtn2007.gis.shp")
siw08 <- read_sf("sinkwtn2008.gis.shp")
siw09 <- read_sf("sinkwtn2009.gis.shp")
siw10 <- read_sf("sinkwtn2010.gis.shp")
siw11 <- read_sf("sinkwtn2011.gis.shp")
siw12 <- read_sf("sinkwtn2012.gis.shp")
siw13 <- read_sf("sinkwtn2013.gis.shp")
siw14 <- read_sf("sinkwtn2014.gis.shp")
siw15 <- read_sf("sinkwtn2015.gis.shp")
siw16 <- read_sf("sinkwtn2016.gis.shp")
siw17 <- read_sf("sinkwtn2017.gis.shp")
siw18 <- read_sf("sinkwtn2018.gis.shp")
siw19 <- read_sf("sinkwtn2019.gis.shp")
siw20 <- read_sf("sinkwtn2020.gis.shp")
siw21 <- read_sf("sinkwtn2021.gis.shp")
siw22 <- read_sf("sinkwtn2022.gis.shp")

siw04$year <- 2004
siw05$year <- 2005
siw06$year <- 2006
siw07$year <- 2007
siw08$year <- 2008
siw09$year <- 2009
siw10$year <- 2010
siw11$year <- 2011
siw12$year <- 2012
siw13$year <- 2013
siw14$year <- 2014
siw15$year <- 2015
siw16$year <- 2016
siw17$year <- 2017
siw18$year <- 2018
siw19$year <- 2019
siw20$year <- 2020
siw21$year <- 2021
siw22$year <- 2022

siw <- rbind(siw04,
             siw05,
             siw06,
             siw07,
             siw08,
             siw09,
             siw10,
             siw11,
             siw12,
             siw13,
             siw14,
             siw15,
             siw16,
             siw17,
             siw18,
             siw19,
             siw20,
             siw21,
             siw22)

siw <- siw[siw$P_VALUE<0.05,]

siw_m_04 <- read_sf("sinkwtn2004.col.shp")
siw_m_05 <- read_sf("sinkwtn2005.col.shp")
siw_m_06 <- read_sf("sinkwtn2006.col.shp")
siw_m_07 <- read_sf("sinkwtn2007.col.shp")
siw_m_08 <- read_sf("sinkwtn2008.col.shp")
siw_m_09 <- read_sf("sinkwtn2009.col.shp")
siw_m_10 <- read_sf("sinkwtn2010.col.shp")
siw_m_11 <- read_sf("sinkwtn2011.col.shp")
siw_m_12 <- read_sf("sinkwtn2012.col.shp")
siw_m_13 <- read_sf("sinkwtn2013.col.shp")
siw_m_14 <- read_sf("sinkwtn2014.col.shp")
siw_m_15 <- read_sf("sinkwtn2015.col.shp")
siw_m_16 <- read_sf("sinkwtn2016.col.shp")
siw_m_17 <- read_sf("sinkwtn2017.col.shp")
siw_m_18 <- read_sf("sinkwtn2018.col.shp")
siw_m_19 <- read_sf("sinkwtn2019.col.shp")
siw_m_20 <- read_sf("sinkwtn2020.col.shp")
siw_m_21 <- read_sf("sinkwtn2021.col.shp")
siw_m_22 <- read_sf("sinkwtn2022.col.shp")

siw_m_04$year <- 2004
siw_m_05$year <- 2005
siw_m_06$year <- 2006
siw_m_07$year <- 2007
siw_m_08$year <- 2008
siw_m_09$year <- 2009
siw_m_10$year <- 2010
siw_m_11$year <- 2011
siw_m_12$year <- 2012
siw_m_13$year <- 2013
siw_m_14$year <- 2014
siw_m_15$year <- 2015
siw_m_16$year <- 2016
siw_m_17$year <- 2017
siw_m_18$year <- 2018
siw_m_19$year <- 2019
siw_m_20$year <- 2020
siw_m_21$year <- 2021
siw_m_22$year <- 2022

siw_m <- rbind(siw_m_04,
               siw_m_05,
               siw_m_06,
               siw_m_07,
               siw_m_08,
               siw_m_09,
               siw_m_10,
               siw_m_11,
               siw_m_12,
               siw_m_13,
               siw_m_14,
               siw_m_15,
               siw_m_16,
               siw_m_17,
               siw_m_18,
               siw_m_19,
               siw_m_20,
               siw_m_21,
               siw_m_22)

siw_m <- siw_m[siw_m$P_VALUE<0.05,]

siw_m$END_DATE <- as.Date(siw_m$END_DATE)
siw_m$START_DATE <- as.Date(siw_m$START_DATE)

siw_m$months <- interval(siw_m$START_DATE, siw_m$END_DATE) %/% months(1) 
siw_m$year_grp <- NA
siw_m$year_grp[siw_m$year%in%c(2004:2009)] <- 1
siw_m$year_grp[siw_m$year%in%c(2010:2016)] <- 2
siw_m$year_grp[siw_m$year%in%c(2017:2022)] <- 3

siw_m <- as.data.frame(siw_m)
siw <- as.data.frame(siw)

siw_final <- merge(subset(siw,select=c("LOC_ID","year","CLUSTER")),
                   subset(siw_m,select=c("LOC_ID","year","year_grp","months","CLUSTER")),
                   by=c("year","CLUSTER"),all=T)

siw_final$LOC_ID.y <- NULL
names(siw_final)[3] <- "mun"

siw_grp <- siw_final %>%
  group_by(year_grp,mun) %>%
  summarise(total_months = sum(months))

siw_grp$perc_sig <- NA
siw_grp$perc_sig[siw_grp$year_grp==1] <- siw_grp$total_months[siw_grp$year_grp==1]/72
siw_grp$perc_sig[siw_grp$year_grp==2] <- siw_grp$total_months[siw_grp$year_grp==2]/84
siw_grp$perc_sig[siw_grp$year_grp==3] <- siw_grp$total_months[siw_grp$year_grp==3]/72

siw_grp$type <- "sink_within"

### SOURCE BETWEEN ###
setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/SatScan/CURRENT -- SatScan -- 04 to 2022 with SINAN/Output/Source between")
sob04 <- read_sf("sourcebtw2004.gis.shp")
sob05 <- read_sf("sourcebtw2005.gis.shp")
sob06 <- read_sf("sourcebtw2006.gis.shp")
sob07 <- read_sf("sourcebtw2007.gis.shp")
sob08 <- read_sf("sourcebtw2008.gis.shp")
sob09 <- read_sf("sourcebtw2009.gis.shp")
sob10 <- read_sf("sourcebtw2010.gis.shp")
sob11 <- read_sf("sourcebtw2011.gis.shp")
sob12 <- read_sf("sourcebtw2012.gis.shp")
sob13 <- read_sf("sourcebtw2013.gis.shp")
sob14 <- read_sf("sourcebtw2014.gis.shp")
sob15 <- read_sf("sourcebtw2015.gis.shp")
sob16 <- read_sf("sourcebtw2016.gis.shp")
sob17 <- read_sf("sourcebtw2017.gis.shp")
sob18 <- read_sf("sourcebtw2018.gis.shp")
sob19 <- read_sf("sourcebtw2019.gis.shp")
sob20 <- read_sf("sourcebtw2020.gis.shp")
sob21 <- read_sf("sourcebtw2021.gis.shp")
sob22 <- read_sf("sourcebtw2022.gis.shp")

sob04$year <- 2004
sob05$year <- 2005
sob06$year <- 2006
sob07$year <- 2007
sob08$year <- 2008
sob09$year <- 2009
sob10$year <- 2010
sob11$year <- 2011
sob12$year <- 2012
sob13$year <- 2013
sob14$year <- 2014
sob15$year <- 2015
sob16$year <- 2016
sob17$year <- 2017
sob18$year <- 2018
sob19$year <- 2019
sob20$year <- 2020
sob21$year <- 2021
sob22$year <- 2022

sob <- rbind(sob04,
             sob05,
             sob06,
             sob07,
             sob08,
             sob09,
             sob10,
             sob11,
             sob12,
             sob13,
             sob14,
             sob15,
             sob16,
             sob17,
             sob18,
             sob19,
             sob20,
             sob21,
             sob22)

sob <- sob[sob$P_VALUE<0.05,]

sob_m_04 <- read_sf("sourcebtw2004.col.shp")
sob_m_05 <- read_sf("sourcebtw2005.col.shp")
sob_m_06 <- read_sf("sourcebtw2006.col.shp")
sob_m_07 <- read_sf("sourcebtw2007.col.shp")
sob_m_08 <- read_sf("sourcebtw2008.col.shp")
sob_m_09 <- read_sf("sourcebtw2009.col.shp")
sob_m_10 <- read_sf("sourcebtw2010.col.shp")
sob_m_11 <- read_sf("sourcebtw2011.col.shp")
sob_m_12 <- read_sf("sourcebtw2012.col.shp")
sob_m_13 <- read_sf("sourcebtw2013.col.shp")
sob_m_14 <- read_sf("sourcebtw2014.col.shp")
sob_m_15 <- read_sf("sourcebtw2015.col.shp")
sob_m_16 <- read_sf("sourcebtw2016.col.shp")
sob_m_17 <- read_sf("sourcebtw2017.col.shp")
sob_m_18 <- read_sf("sourcebtw2018.col.shp")
sob_m_19 <- read_sf("sourcebtw2019.col.shp")
sob_m_20 <- read_sf("sourcebtw2020.col.shp")
sob_m_21 <- read_sf("sourcebtw2021.col.shp")
sob_m_22 <- read_sf("sourcebtw2022.col.shp")

sob_m_04$year <- 2004
sob_m_05$year <- 2005
sob_m_06$year <- 2006
sob_m_07$year <- 2007
sob_m_08$year <- 2008
sob_m_09$year <- 2009
sob_m_10$year <- 2010
sob_m_11$year <- 2011
sob_m_12$year <- 2012
sob_m_13$year <- 2013
sob_m_14$year <- 2014
sob_m_15$year <- 2015
sob_m_16$year <- 2016
sob_m_17$year <- 2017
sob_m_18$year <- 2018
sob_m_19$year <- 2019
sob_m_20$year <- 2020
sob_m_21$year <- 2021
sob_m_22$year <- 2022

sob_m <- rbind(sob_m_04,
               sob_m_05,
               sob_m_06,
               sob_m_07,
               sob_m_08,
               sob_m_09,
               sob_m_10,
               sob_m_11,
               sob_m_12,
               sob_m_13,
               sob_m_14,
               sob_m_15,
               sob_m_16,
               sob_m_17,
               sob_m_18,
               sob_m_19,
               sob_m_20,
               sob_m_21,
               sob_m_22)

sob_m <- sob_m[sob_m$P_VALUE<0.05,]

sob_m$END_DATE <- as.Date(sob_m$END_DATE)
sob_m$START_DATE <- as.Date(sob_m$START_DATE)

sob_m$months <- interval(sob_m$START_DATE, sob_m$END_DATE) %/% months(1) 
sob_m$year_grp <- NA
sob_m$year_grp[sob_m$year%in%c(2004:2009)] <- 1
sob_m$year_grp[sob_m$year%in%c(2010:2016)] <- 2
sob_m$year_grp[sob_m$year%in%c(2017:2022)] <- 3

sob_m <- as.data.frame(sob_m)
sob <- as.data.frame(sob)

sob_final <- merge(subset(sob,select=c("LOC_ID","year","CLUSTER")),
                   subset(sob_m,select=c("LOC_ID","year","year_grp","months","CLUSTER")),
                   by=c("year","CLUSTER"),all=T)

sob_final$LOC_ID.y <- NULL
names(sob_final)[3] <- "mun"

sob_grp <- sob_final %>%
  group_by(year_grp,mun) %>%
  summarise(total_months = sum(months))

sob_grp$perc_sig <- NA
sob_grp$perc_sig[sob_grp$year_grp==1] <- sob_grp$total_months[sob_grp$year_grp==1]/72
sob_grp$perc_sig[sob_grp$year_grp==2] <- sob_grp$total_months[sob_grp$year_grp==2]/84
sob_grp$perc_sig[sob_grp$year_grp==3] <- sob_grp$total_months[sob_grp$year_grp==3]/72

sob_grp$type <- "source_between"

### SOURCE WITHIN ###
setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/SatScan/CURRENT -- SatScan -- 04 to 2022 with SINAN/Output/Source within")
sow04 <- read_sf("sourcewtn2004.gis.shp")
sow05 <- read_sf("sourcewtn2005.gis.shp")
sow06 <- read_sf("sourcewtn2006.gis.shp")
sow07 <- read_sf("sourcewtn2007.gis.shp")
sow08 <- read_sf("sourcewtn2008.gis.shp")
sow09 <- read_sf("sourcewtn2009.gis.shp")
sow10 <- read_sf("sourcewtn2010.gis.shp")
sow11 <- read_sf("sourcewtn2011.gis.shp")
sow12 <- read_sf("sourcewtn2012.gis.shp")
sow13 <- read_sf("sourcewtn2013.gis.shp")
sow14 <- read_sf("sourcewtn2014.gis.shp")
sow15 <- read_sf("sourcewtn2015.gis.shp")
sow16 <- read_sf("sourcewtn2016.gis.shp")
sow17 <- read_sf("sourcewtn2017.gis.shp")
sow18 <- read_sf("sourcewtn2018.gis.shp")
sow19 <- read_sf("sourcewtn2019.gis.shp")
sow20 <- read_sf("sourcewtn2020.gis.shp")
sow21 <- read_sf("sourcewtn2021.gis.shp")
sow22 <- read_sf("sourcewtn2022.gis.shp")

sow04$year <- 2004
sow05$year <- 2005
sow06$year <- 2006
sow07$year <- 2007
sow08$year <- 2008
sow09$year <- 2009
sow10$year <- 2010
sow11$year <- 2011
sow12$year <- 2012
sow13$year <- 2013
sow14$year <- 2014
sow15$year <- 2015
sow16$year <- 2016
sow17$year <- 2017
sow18$year <- 2018
sow19$year <- 2019
sow20$year <- 2020
sow21$year <- 2021
sow22$year <- 2022

sow <- rbind(sow04,
             sow05,
             sow06,
             sow07,
             sow08,
             sow09,
             sow10,
             sow11,
             sow12,
             sow13,
             sow14,
             sow15,
             sow16,
             sow17,
             sow18,
             sow19,
             sow20,
             sow21,
             sow22)

sow <- sow[sow$P_VALUE<0.05,]

sow_m_04 <- read_sf("sourcewtn2004.col.shp")
sow_m_05 <- read_sf("sourcewtn2005.col.shp")
sow_m_06 <- read_sf("sourcewtn2006.col.shp")
sow_m_07 <- read_sf("sourcewtn2007.col.shp")
sow_m_08 <- read_sf("sourcewtn2008.col.shp")
sow_m_09 <- read_sf("sourcewtn2009.col.shp")
sow_m_10 <- read_sf("sourcewtn2010.col.shp")
sow_m_11 <- read_sf("sourcewtn2011.col.shp")
sow_m_12 <- read_sf("sourcewtn2012.col.shp")
sow_m_13 <- read_sf("sourcewtn2013.col.shp")
sow_m_14 <- read_sf("sourcewtn2014.col.shp")
sow_m_15 <- read_sf("sourcewtn2015.col.shp")
sow_m_16 <- read_sf("sourcewtn2016.col.shp")
sow_m_17 <- read_sf("sourcewtn2017.col.shp")
sow_m_18 <- read_sf("sourcewtn2018.col.shp")
sow_m_19 <- read_sf("sourcewtn2019.col.shp")
sow_m_20 <- read_sf("sourcewtn2020.col.shp")
sow_m_21 <- read_sf("sourcewtn2021.col.shp")
sow_m_22 <- read_sf("sourcewtn2022.col.shp")

sow_m_04$year <- 2004
sow_m_05$year <- 2005
sow_m_06$year <- 2006
sow_m_07$year <- 2007
sow_m_08$year <- 2008
sow_m_09$year <- 2009
sow_m_10$year <- 2010
sow_m_11$year <- 2011
sow_m_12$year <- 2012
sow_m_13$year <- 2013
sow_m_14$year <- 2014
sow_m_15$year <- 2015
sow_m_16$year <- 2016
sow_m_17$year <- 2017
sow_m_18$year <- 2018
sow_m_19$year <- 2019
sow_m_20$year <- 2020
sow_m_21$year <- 2021
sow_m_22$year <- 2022

sow_m <- rbind(sow_m_04,
               sow_m_05,
               sow_m_06,
               sow_m_07,
               sow_m_08,
               sow_m_09,
               sow_m_10,
               sow_m_11,
               sow_m_12,
               sow_m_13,
               sow_m_14,
               sow_m_15,
               sow_m_16,
               sow_m_17,
               sow_m_18,
               sow_m_19,
               sow_m_20,
               sow_m_21,
               sow_m_22)

sow_m <- sow_m[sow_m$P_VALUE<0.05,]

sow_m$END_DATE <- as.Date(sow_m$END_DATE)
sow_m$START_DATE <- as.Date(sow_m$START_DATE)

sow_m$months <- interval(sow_m$START_DATE, sow_m$END_DATE) %/% months(1) 
sow_m$year_grp <- NA
sow_m$year_grp[sow_m$year%in%c(2004:2009)] <- 1
sow_m$year_grp[sow_m$year%in%c(2010:2016)] <- 2
sow_m$year_grp[sow_m$year%in%c(2017:2022)] <- 3

sow_m <- as.data.frame(sow_m)
sow <- as.data.frame(sow)

sow_final <- merge(subset(sow,select=c("LOC_ID","year","CLUSTER")),
                   subset(sow_m,select=c("LOC_ID","year","year_grp","months","CLUSTER")),
                   by=c("year","CLUSTER"),all=T)

sow_final$LOC_ID.y <- NULL
names(sow_final)[3] <- "mun"

sow_grp <- sow_final %>%
  group_by(year_grp,mun) %>%
  summarise(total_months = sum(months))

sow_grp$perc_sig <- NA
sow_grp$perc_sig[sow_grp$year_grp==1] <- sow_grp$total_months[sow_grp$year_grp==1]/72
sow_grp$perc_sig[sow_grp$year_grp==2] <- sow_grp$total_months[sow_grp$year_grp==2]/84
sow_grp$perc_sig[sow_grp$year_grp==3] <- sow_grp$total_months[sow_grp$year_grp==3]/72

sow_grp$type <- "source_within"

###### RBIND DATA #######

sources_sinks_merge <- rbind(siw_grp,sow_grp,sib_grp,sob_grp)
write.csv(sources_sinks_merge,"C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/Figures/Figure 5/source and sink cluster data for maps.csv")

###################
### GMR WITHIN ###

setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/SatScan Data/GMR within/Output")
GMRw03 <- read_sf("GMR within 2003.gis.shp")
GMRw04 <- read_sf("GMR within 2004.gis.shp")
GMRw05 <- read_sf("GMR within 2005.gis.shp")
GMRw06 <- read_sf("GMR within 2006.gis.shp")
GMRw07 <- read_sf("GMR within 2007.gis.shp")
GMRw08 <- read_sf("GMR within 2008.gis.shp")
GMRw09 <- read_sf("GMR within 2009.gis.shp")
GMRw10 <- read_sf("GMR within 2010.gis.shp")
GMRw11 <- read_sf("GMR within 2011.gis.shp")
GMRw12 <- read_sf("GMR within 2012.gis.shp")
GMRw13 <- read_sf("GMR within 2013.gis.shp")
GMRw14 <- read_sf("GMR within 2014.gis.shp")
GMRw15 <- read_sf("GMR within 2015.gis.shp")
GMRw16 <- read_sf("GMR within 2016.gis.shp")
GMRw17 <- read_sf("GMR within 2017.gis.shp")
GMRw18 <- read_sf("GMR within 2018.gis.shp")
GMRw19 <- read_sf("GMR within 2019.gis.shp")
GMRw20 <- read_sf("GMR within 2020.gis.shp")

GMRw03$year <- 2003
GMRw04$year <- 2004
GMRw05$year <- 2005
GMRw06$year <- 2006
GMRw07$year <- 2007
GMRw08$year <- 2008
GMRw09$year <- 2009
GMRw10$year <- 2010
GMRw11$year <- 2011
GMRw12$year <- 2012
GMRw13$year <- 2013
GMRw14$year <- 2014
GMRw15$year <- 2015
GMRw16$year <- 2016
GMRw17$year <- 2017
GMRw18$year <- 2018
GMRw19$year <- 2019
GMRw20$year <- 2020

GMRw <- rbind(GMRw03,
             GMRw04,
             GMRw05,
             GMRw06,
             GMRw07,
             GMRw08,
             GMRw09,
             GMRw10,
             GMRw11,
             GMRw12,
             GMRw13,
             GMRw14,
             GMRw15,
             GMRw16,
             GMRw17,
             GMRw18,
             GMRw19,
             GMRw20)

GMRw <- GMRw[GMRw$P_VALUE<0.05,]

GMRw_m_03 <- read_sf("GMR within 2003.col.shp")
GMRw_m_04 <- read_sf("GMR within 2004.col.shp")
GMRw_m_05 <- read_sf("GMR within 2005.col.shp")
GMRw_m_06 <- read_sf("GMR within 2006.col.shp")
GMRw_m_07 <- read_sf("GMR within 2007.col.shp")
GMRw_m_08 <- read_sf("GMR within 2008.col.shp")
GMRw_m_09 <- read_sf("GMR within 2009.col.shp")
GMRw_m_10 <- read_sf("GMR within 2010.col.shp")
GMRw_m_11 <- read_sf("GMR within 2011.col.shp")
GMRw_m_12 <- read_sf("GMR within 2012.col.shp")
GMRw_m_13 <- read_sf("GMR within 2013.col.shp")
GMRw_m_14 <- read_sf("GMR within 2014.col.shp")
GMRw_m_15 <- read_sf("GMR within 2015.col.shp")
GMRw_m_16 <- read_sf("GMR within 2016.col.shp")
GMRw_m_17 <- read_sf("GMR within 2017.col.shp")
GMRw_m_18 <- read_sf("GMR within 2018.col.shp")
GMRw_m_19 <- read_sf("GMR within 2019.col.shp")
GMRw_m_20 <- read_sf("GMR within 2020.col.shp")

GMRw_m_03$year <- 2003
GMRw_m_04$year <- 2004
GMRw_m_05$year <- 2005
GMRw_m_06$year <- 2006
GMRw_m_07$year <- 2007
GMRw_m_08$year <- 2008
GMRw_m_09$year <- 2009
GMRw_m_10$year <- 2010
GMRw_m_11$year <- 2011
GMRw_m_12$year <- 2012
GMRw_m_13$year <- 2013
GMRw_m_14$year <- 2014
GMRw_m_15$year <- 2015
GMRw_m_16$year <- 2016
GMRw_m_17$year <- 2017
GMRw_m_18$year <- 2018
GMRw_m_19$year <- 2019
GMRw_m_20$year <- 2020

GMRw_m <- rbind(GMRw_m_03,
               GMRw_m_04,
               GMRw_m_05,
               GMRw_m_06,
               GMRw_m_07,
               GMRw_m_08,
               GMRw_m_09,
               GMRw_m_10,
               GMRw_m_11,
               GMRw_m_12,
               GMRw_m_13,
               GMRw_m_14,
               GMRw_m_15,
               GMRw_m_16,
               GMRw_m_17,
               GMRw_m_18,
               GMRw_m_19,
               GMRw_m_20)

GMRw_m <- GMRw_m[GMRw_m$P_VALUE<0.05,]

GMRw_m$END_DATE <- as.Date(GMRw_m$END_DATE)
GMRw_m$START_DATE <- as.Date(GMRw_m$START_DATE)

GMRw_m$months <- interval(GMRw_m$START_DATE, GMRw_m$END_DATE) %/% months(1) 
GMRw_m$year_grp <- NA
GMRw_m$year_grp[GMRw_m$year%in%c(2003:2009)] <- 1
GMRw_m$year_grp[GMRw_m$year%in%c(2010:2016)] <- 2
GMRw_m$year_grp[GMRw_m$year%in%c(2017:2020)] <- 3

GMRw_m <- as.data.frame(GMRw_m)
GMRw <- as.data.frame(GMRw)

GMRw_final <- merge(subset(GMRw,select=c("LOC_ID","year","CLUSTER")),
                   subset(GMRw_m,select=c("LOC_ID","year","year_grp","months","CLUSTER")),
                   by=c("year","CLUSTER"),all=T)

GMRw_final$LOC_ID.y <- NULL
names(GMRw_final)[3] <- "mun"

GMRw_grp <- GMRw_final %>%
  group_by(year_grp,mun) %>%
  summarise(total_months = sum(months))

GMRw_grp$perc_sig <- NA
GMRw_grp$perc_sig[GMRw_grp$year_grp==1] <- GMRw_grp$total_months[GMRw_grp$year_grp==1]/72
GMRw_grp$perc_sig[GMRw_grp$year_grp==2] <- GMRw_grp$total_months[GMRw_grp$year_grp==2]/72
GMRw_grp$perc_sig[GMRw_grp$year_grp==3] <- GMRw_grp$total_months[GMRw_grp$year_grp==3]/48

GMRw_grp$type <- "GMR within"

### GMR BETWEEN ###

setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/SatScan Data/GMR between/Output")
GMRb03 <- read_sf("GMR between 2003.gis.shp")
GMRb04 <- read_sf("GMR between 2004.gis.shp")
GMRb05 <- read_sf("GMR between 2005.gis.shp")
GMRb06 <- read_sf("GMR between 2006.gis.shp")
GMRb07 <- read_sf("GMR between 2007.gis.shp")
GMRb08 <- read_sf("GMR between 2008.gis.shp")
GMRb09 <- read_sf("GMR between 2009.gis.shp")
GMRb10 <- read_sf("GMR between 2010.gis.shp")
GMRb11 <- read_sf("GMR between 2011.gis.shp")
GMRb12 <- read_sf("GMR between 2012.gis.shp")
GMRb13 <- read_sf("GMR between 2013.gis.shp")
GMRb14 <- read_sf("GMR between 2014.gis.shp")
GMRb15 <- read_sf("GMR between 2015.gis.shp")
GMRb16 <- read_sf("GMR between 2016.gis.shp")
GMRb17 <- read_sf("GMR between 2017.gis.shp")
GMRb18 <- read_sf("GMR between 2018.gis.shp")
GMRb19 <- read_sf("GMR between 2019.gis.shp")
GMRb20 <- read_sf("GMR between 2020.gis.shp")

GMRb03$year <- 2003
GMRb04$year <- 2004
GMRb05$year <- 2005
GMRb06$year <- 2006
GMRb07$year <- 2007
GMRb08$year <- 2008
GMRb09$year <- 2009
GMRb10$year <- 2010
GMRb11$year <- 2011
GMRb12$year <- 2012
GMRb13$year <- 2013
GMRb14$year <- 2014
GMRb15$year <- 2015
GMRb16$year <- 2016
GMRb17$year <- 2017
GMRb18$year <- 2018
GMRb19$year <- 2019
GMRb20$year <- 2020

GMRb <- rbind(GMRb03,
              GMRb04,
              GMRb05,
              GMRb06,
              GMRb07,
              GMRb08,
              GMRb09,
              GMRb10,
              GMRb11,
              GMRb12,
              GMRb13,
              GMRb14,
              GMRb15,
              GMRb16,
              GMRb17,
              GMRb18,
              GMRb19,
              GMRb20)

GMRb <- GMRb[GMRb$P_VALUE<0.05,]

GMRb_m_03 <- read_sf("GMR between 2003.col.shp")
GMRb_m_04 <- read_sf("GMR between 2004.col.shp")
GMRb_m_05 <- read_sf("GMR between 2005.col.shp")
GMRb_m_06 <- read_sf("GMR between 2006.col.shp")
GMRb_m_07 <- read_sf("GMR between 2007.col.shp")
GMRb_m_08 <- read_sf("GMR between 2008.col.shp")
GMRb_m_09 <- read_sf("GMR between 2009.col.shp")
GMRb_m_10 <- read_sf("GMR between 2010.col.shp")
GMRb_m_11 <- read_sf("GMR between 2011.col.shp")
GMRb_m_12 <- read_sf("GMR between 2012.col.shp")
GMRb_m_13 <- read_sf("GMR between 2013.col.shp")
GMRb_m_14 <- read_sf("GMR between 2014.col.shp")
GMRb_m_15 <- read_sf("GMR between 2015.col.shp")
GMRb_m_16 <- read_sf("GMR between 2016.col.shp")
GMRb_m_17 <- read_sf("GMR between 2017.col.shp")
GMRb_m_18 <- read_sf("GMR between 2018.col.shp")
GMRb_m_19 <- read_sf("GMR between 2019.col.shp")
GMRb_m_20 <- read_sf("GMR between 2020.col.shp")

GMRb_m_03$year <- 2003
GMRb_m_04$year <- 2004
GMRb_m_05$year <- 2005
GMRb_m_06$year <- 2006
GMRb_m_07$year <- 2007
GMRb_m_08$year <- 2008
GMRb_m_09$year <- 2009
GMRb_m_10$year <- 2010
GMRb_m_11$year <- 2011
GMRb_m_12$year <- 2012
GMRb_m_13$year <- 2013
GMRb_m_14$year <- 2014
GMRb_m_15$year <- 2015
GMRb_m_16$year <- 2016
GMRb_m_17$year <- 2017
GMRb_m_18$year <- 2018
GMRb_m_19$year <- 2019
GMRb_m_20$year <- 2020

GMRb_m <- rbind(GMRb_m_03,
                GMRb_m_04,
                GMRb_m_05,
                GMRb_m_06,
                GMRb_m_07,
                GMRb_m_08,
                GMRb_m_09,
                GMRb_m_10,
                GMRb_m_11,
                GMRb_m_12,
                GMRb_m_13,
                GMRb_m_14,
                GMRb_m_15,
                GMRb_m_16,
                GMRb_m_17,
                GMRb_m_18,
                GMRb_m_19,
                GMRb_m_20)

GMRb_m <- GMRb_m[GMRb_m$P_VALUE<0.05,]

GMRb_m$END_DATE <- as.Date(GMRb_m$END_DATE)
GMRb_m$START_DATE <- as.Date(GMRb_m$START_DATE)

GMRb_m$months <- interval(GMRb_m$START_DATE, GMRb_m$END_DATE) %/% months(1) 
GMRb_m$year_grp <- NA
GMRb_m$year_grp[GMRb_m$year%in%c(2003:2009)] <- 1
GMRb_m$year_grp[GMRb_m$year%in%c(2010:2016)] <- 2
GMRb_m$year_grp[GMRb_m$year%in%c(2017:2020)] <- 3

GMRb_m <- as.data.frame(GMRb_m)
GMRb <- as.data.frame(GMRb)

GMRb_final <- merge(subset(GMRb,select=c("LOC_ID","year","CLUSTER")),
                    subset(GMRb_m,select=c("LOC_ID","year","year_grp","months","CLUSTER")),
                    by=c("year","CLUSTER"),all=T)

GMRb_final$LOC_ID.y <- NULL
names(GMRb_final)[3] <- "mun"

GMRb_grp <- GMRb_final %>%
  group_by(year_grp,mun) %>%
  summarise(total_months = sum(months))

GMRb_grp$perc_sig <- NA
GMRb_grp$perc_sig[GMRb_grp$year_grp==1] <- GMRb_grp$total_months[GMRb_grp$year_grp==1]/72
GMRb_grp$perc_sig[GMRb_grp$year_grp==2] <- GMRb_grp$total_months[GMRb_grp$year_grp==2]/72
GMRb_grp$perc_sig[GMRb_grp$year_grp==3] <- GMRb_grp$total_months[GMRb_grp$year_grp==3]/48

GMRb_grp$type <- "GMR between"


### GMR TOTAL ###

setwd("C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Current analyses/SatScan Data/GMR total/Output")
GMRt03 <- read_sf("GCMR total 2003.gis.shp")
GMRt04 <- read_sf("GCMR total 2004.gis.shp")
GMRt05 <- read_sf("GCMR total 2005.gis.shp")
GMRt06 <- read_sf("GCMR total 2006.gis.shp")
GMRt07 <- read_sf("GCMR total 2007.gis.shp")
GMRt08 <- read_sf("GCMR total 2008.gis.shp")
GMRt09 <- read_sf("GCMR total 2009.gis.shp")
GMRt10 <- read_sf("GCMR total 2010.gis.shp")
GMRt11 <- read_sf("GCMR total 2011.gis.shp")
GMRt12 <- read_sf("GCMR total 2012.gis.shp")
GMRt13 <- read_sf("GCMR total 2013.gis.shp")
GMRt14 <- read_sf("GCMR total 2014.gis.shp")
GMRt15 <- read_sf("GCMR total 2015.gis.shp")
GMRt16 <- read_sf("GCMR total 2016.gis.shp")
GMRt17 <- read_sf("GCMR total 2017.gis.shp")
GMRt18 <- read_sf("GCMR total 2018.gis.shp")
GMRt19 <- read_sf("GCMR total 2019.gis.shp")
GMRt20 <- read_sf("GCMR total 2020.gis.shp")

GMRt03$year <- 2003
GMRt04$year <- 2004
GMRt05$year <- 2005
GMRt06$year <- 2006
GMRt07$year <- 2007
GMRt08$year <- 2008
GMRt09$year <- 2009
GMRt10$year <- 2010
GMRt11$year <- 2011
GMRt12$year <- 2012
GMRt13$year <- 2013
GMRt14$year <- 2014
GMRt15$year <- 2015
GMRt16$year <- 2016
GMRt17$year <- 2017
GMRt18$year <- 2018
GMRt19$year <- 2019
GMRt20$year <- 2020

GMRt <- rbind(GMRt03,
              GMRt04,
              GMRt05,
              GMRt06,
              GMRt07,
              GMRt08,
              GMRt09,
              GMRt10,
              GMRt11,
              GMRt12,
              GMRt13,
              GMRt14,
              GMRt15,
              GMRt16,
              GMRt17,
              GMRt18,
              GMRt19,
              GMRt20)

GMRt <- GMRt[GMRt$P_VALUE<0.05,]

GMRt_m_03 <- read_sf("GCMR total 2003.col.shp")
GMRt_m_04 <- read_sf("GCMR total 2004.col.shp")
GMRt_m_05 <- read_sf("GCMR total 2005.col.shp")
GMRt_m_06 <- read_sf("GCMR total 2006.col.shp")
GMRt_m_07 <- read_sf("GCMR total 2007.col.shp")
GMRt_m_08 <- read_sf("GCMR total 2008.col.shp")
GMRt_m_09 <- read_sf("GCMR total 2009.col.shp")
GMRt_m_10 <- read_sf("GCMR total 2010.col.shp")
GMRt_m_11 <- read_sf("GCMR total 2011.col.shp")
GMRt_m_12 <- read_sf("GCMR total 2012.col.shp")
GMRt_m_13 <- read_sf("GCMR total 2013.col.shp")
GMRt_m_14 <- read_sf("GCMR total 2014.col.shp")
GMRt_m_15 <- read_sf("GCMR total 2015.col.shp")
GMRt_m_16 <- read_sf("GCMR total 2016.col.shp")
GMRt_m_17 <- read_sf("GCMR total 2017.col.shp")
GMRt_m_18 <- read_sf("GCMR total 2018.col.shp")
GMRt_m_19 <- read_sf("GCMR total 2019.col.shp")
GMRt_m_20 <- read_sf("GCMR total 2020.col.shp")

GMRt_m_03$year <- 2003
GMRt_m_04$year <- 2004
GMRt_m_05$year <- 2005
GMRt_m_06$year <- 2006
GMRt_m_07$year <- 2007
GMRt_m_08$year <- 2008
GMRt_m_09$year <- 2009
GMRt_m_10$year <- 2010
GMRt_m_11$year <- 2011
GMRt_m_12$year <- 2012
GMRt_m_13$year <- 2013
GMRt_m_14$year <- 2014
GMRt_m_15$year <- 2015
GMRt_m_16$year <- 2016
GMRt_m_17$year <- 2017
GMRt_m_18$year <- 2018
GMRt_m_19$year <- 2019
GMRt_m_20$year <- 2020

GMRt_m <- rbind(GMRt_m_03,
                GMRt_m_04,
                GMRt_m_05,
                GMRt_m_06,
                GMRt_m_07,
                GMRt_m_08,
                GMRt_m_09,
                GMRt_m_10,
                GMRt_m_11,
                GMRt_m_12,
                GMRt_m_13,
                GMRt_m_14,
                GMRt_m_15,
                GMRt_m_16,
                GMRt_m_17,
                GMRt_m_18,
                GMRt_m_19,
                GMRt_m_20)

GMRt_m <- GMRt_m[GMRt_m$P_VALUE<0.05,]

GMRt_m$END_DATE <- as.Date(GMRt_m$END_DATE)
GMRt_m$START_DATE <- as.Date(GMRt_m$START_DATE)

GMRt_m$months <- interval(GMRt_m$START_DATE, GMRt_m$END_DATE) %/% months(1) 
GMRt_m$year_grp <- NA
GMRt_m$year_grp[GMRt_m$year%in%c(2003:2009)] <- 1
GMRt_m$year_grp[GMRt_m$year%in%c(2010:2016)] <- 2
GMRt_m$year_grp[GMRt_m$year%in%c(2017:2020)] <- 3

GMRt_m <- as.data.frame(GMRt_m)
GMRt <- as.data.frame(GMRt)

GMRt_final <- merge(subset(GMRt,select=c("LOC_ID","year","CLUSTER")),
                    subset(GMRt_m,select=c("LOC_ID","year","year_grp","months","CLUSTER")),
                    by=c("year","CLUSTER"),all=T)

GMRt_final$LOC_ID.y <- NULL
names(GMRt_final)[3] <- "mun"

GMRt_grp <- GMRt_final %>%
  group_by(year_grp,mun) %>%
  summarise(total_months = sum(months))

GMRt_grp$perc_sig <- NA
GMRt_grp$perc_sig[GMRt_grp$year_grp==1] <- GMRt_grp$total_months[GMRt_grp$year_grp==1]/72
GMRt_grp$perc_sig[GMRt_grp$year_grp==2] <- GMRt_grp$total_months[GMRt_grp$year_grp==2]/72
GMRt_grp$perc_sig[GMRt_grp$year_grp==3] <- GMRt_grp$total_months[GMRt_grp$year_grp==3]/48

GMRt_grp$type <- "GMR total"

###### RBIND DATA #######

GMR_merge <- rbind(GMRw_grp,GMRb_grp,GMRt_grp)
write.csv(GMR_merge,"C:/Users/nicho/OneDrive/Dissertation/Chapter I/Mobility paper/Figures/Figure 5/GMR_data.csv")
