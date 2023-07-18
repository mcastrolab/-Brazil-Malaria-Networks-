library(lubridate)
library(ggplot2)
library(zoo)
library(data.table)
install.packages('tidyverse')
library(tibble)
library(scales)
library(ggplot2)

d3 <- read.csv(file="Panels_C_D_data.csv")

d3$category2003 <- 5
d3$category2003[d3$API2003<50] <- 4
d3$category2003[d3$API2003<10] <- 3
d3$category2003[d3$API2003<1] <- 2
d3$category2003[d3$API2003==0] <- 1

d3$category2020 <- 5
d3$category2020[d3$API2020<50] <- 4
d3$category2020[d3$API2020<10] <- 3
d3$category2020[d3$API2020<1] <- 2
d3$category2020[d3$API2020==0] <- 1

ftable(d3$category2003,d3$category2020)

write.csv(d3,file="Panel C Heatmap Data.csv")
write.csv(d3,file="Panel D Map Data.csv")
