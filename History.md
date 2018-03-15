
## R Hisotry
install.packages("devtools")
devtools::install_github("jhudsl/collegeIncome")
library(collegeIncome)
data(college)
devtools::install_github("jhudsl/matahari")
library(matahari)
library(matahari)
devtools::install_github("jhudsl/matahari")
library(matahari)
q()
setwd("~/Desktop/Data Science/Regression Models/Optional_Quizz")
library(collegeIncome)
data(college)
library(matahari)
library(dplyr)
dim(college)
head(college,3)
str(college)
mean(college$perc_employed+college$perc_unemployed)
mean(college$perc_men + college$perc_women)
sum(is.na(college))
unique(college$major_category)
with(college,aggregate(median,list(major_category), function(x) {c(Length= round(length(x),1) , Mean= round(mean(x),1), Var= round(var(x),1) )} ))
college <- college %>% filter(major_category != "Interdisciplinary")
apply(college,2,range)
college[college$perc_employed_fulltime>1,]
college <- college %>% filter(perc_employed_fulltime <= 1 | perc_employed_fulltime != "Inf")
college$major_category <- as.factor(college$major_category)
# Total number of people with major
par(mar=c(8,4,2,2), cex.axis=0.5 )
plot(as.factor(college$major_category),college$total, las=2)
# See the median in earnings for each category
plot(as.factor(college$major_category), college$median, las=2)
# See the % of people unemployed per category
par(mar=c(8,4,2,2), cex.axis=0.5 )
plot(as.factor(college$major_category),college$perc_unemployed, las=2)
# Percentage of people with low wage jobs out of total
par(mar=c(8,4,2,2), cex.axis=0.5 )
plot(as.factor(college$major_category),college$perc_low_wage_jobs, las=2)
college$No.Women <- college$total*college$perc_women
college$No.Men <- college$total*college$perc_men
college$No.Employed <- college$total*college$perc_employed
college$No.Unemployed <- college$total*college$perc_unemployed
college$No.Employed_Fulltime <- college$No.Employed*college$perc_employed_fulltime
college$No.Employed_Parttime <- college$No.Employed*college$perc_employed_parttime
college$No.Employed_fulltime_yearround <- college$No.Employed_Fulltime*college$perc_employed_fulltime_yearround
college$No.College_jobs <- college$No.Employed*college$perc_college_jobs
college$No.Non_college_jobs <- college$No.Employed*college$perc_non_college_jobs
college$No.Low_wage_jobs <- college$total*college$perc_low_wage_jobs
college2 <- college[,c(4,5,9,20:29)]
college2[rowSums(is.na(college2)) > 0,]
college2 <- college2[complete.cases(college2),]
plot(density(college2$median), xlab = "Median Income", ylab = "Density", main = "Density Plot median Income")
abline(v=mean(college2$median), col="blue")
abline(v=median(college2$median), col="magenta")
legend("topright", legend = c("Mean", "Median"), col = c("Blue","Magenta"), lty=1, cex=0.8)
fit <- lm(median ~ major_category - 1 , data=college2)
summary(fit)
plot(fit)
plot(predict(fit), resid(fit), pch = '.')
fit2 <- lm(median ~ major_category - 1 , data=college2[-c(16,80,62),])
summary(fit2)
par(mfrow = c(2, 2))
plot(fit2)
college3 <- college2[-c(16,80,62),]
college2[c(62,80,16)]
college2[c(62,80,16),]
college2[75:82,]
?influence.measures
influence.measures(fit1)
influence.measures(fit)
influence.measures(fit)[inf=="*"]
influence.measures(fit)[,"inf"=="*"]
influence.measures(fit)[,"inf"]
InfluenceM <- influence.measures(fit)
summary(InfluenceM)
which(apply(InfluenceM$is.inf,1,any))
influencial.rows <- which(apply(InfluenceM$is.inf,1,any))
influencial.rows
influencial.rows <- which(apply(InfluenceM$is.inf,1,any))
college3 <- college2[-influencial.rows,]
fit2 <- lm(median ~ major_category - 1 , data=college3)
summary(fit2)
par(mfrow = c(2, 2))
plot(fit2)
college2[influencial.rows,]
dance_save("~/Desktop/college_major_analysis.rds")
?dance_start
q()