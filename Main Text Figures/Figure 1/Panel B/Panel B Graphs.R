library(lubridate)
library(ggplot2)
library(zoo)
library(data.table)
install.packages('tidyverse')
library(tibble)
library(scales)
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)

d3 <- read.csv(file="Panel_B_Data.csv")

custom.col <- c("#D16103","#C4961A","#FFDB6D","#F4EDCA","#52854C","#4A6F92","#293352")

p2 <- ggplot(d3[d3$perc_change>0,], aes(x = API2003, y = perc_change, col=as.factor(uf))) + 
  geom_point(size=3,alpha=0.75) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(10^-2.5, 10^3)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic() +
  scale_colour_manual(values = custom.col) +
  coord_cartesian(clip = "off")+
  theme(legend.position = "none")
p2 + annotation_logticks()  

custom.col <- c("#D16103","#C4961A","#FFDB6D","#F4EDCA","#52854C","#C07272","#C3D7A4","#4A6F92","#293352")

d3$perc_change[d3$perc_change==-100] <- -200
p3 <- ggplot(d3[d3$perc_change<0,], aes(x = API2003, y = -1*(perc_change), col=as.factor(uf))) + 
  geom_point(size=3,alpha=0.75) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits= c(10^-2.5, 10^3)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic() +
  scale_colour_manual(values = custom.col) +
  geom_hline(yintercept = 56.63035) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none")
p3 + annotation_logticks()  
log(56)
