library(data.table)
library(nnet)

dat <- fread(file="/Supplemental Table 3/Model Data.csv")

## Fix a few variables ##
## Occupation garimpo, forestry, agriculture, domestica, cacapesca
dat$occupation2 <- "other"
dat$occupation2[dat$occupation%in%c(1,2)] <- "agriculture"
dat$occupation2[dat$occupation%in%c(6)] <- "forestry"
dat$occupation2[dat$occupation%in%c(5,9)] <- "garimpo"
dat$occupation2[dat$occupation%in%c(3)] <- "domestic"
dat$occupation2[dat$occupation%in%c(7)] <- "hunting/fishing"

## Set factor levels ##
dat$occupation2 <- relevel(as.factor(dat$occupation2), ref="other")
dat$age_group <- relevel(as.factor(dat$age_group), ref="1")
dat$exam <- relevel(as.factor(dat$exam), ref="vivax")
dat$time_to_care2 <- relevel(as.factor(dat$time_to_care2), ref="<24hrs")
dat$ap <- relevel(as.factor(dat$ap), ref="passive")
dat$outcome <- relevel(as.factor(dat$outcome), ref="local")

## For the years 2004 to 2022 ##
model04to22 <- multinom(outcome ~ 
                          as.factor(age_group) + 
                          as.factor(male) +
                          as.factor(occupation2) +
                          indigenous_case +
                          rural_case +
                          garimpo_case +
                          settlement_case +
                          as.factor(exam) +
                          as.factor(month_inf) +
                          as.factor(time_to_care2),
                        data = dat[dat$year>2003,])

summary_mod04to22 <- summary(model04to22)

z04to22 <- summary_mod04to22$coefficients/summary_mod04to22$standard.errors
p04to22 <- (1 - pnorm(abs(z04to22), 0, 1)) * 2

outcome04to22 <- t(rbind(summary_mod04to22$coefficients,p04to22))
colnames(outcome04to22) <- c("Imp_Between_Coef","Imp_Within_Coef","P_Btw","P_Within")

write.csv(outcome04to22, file="Supplemental Table 3/Supp Table 3 Model04to22_summary_output.csv")


