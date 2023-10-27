install.packages("readxl")
library(readxl)

#importing data set
file <- "C:/Egyetem/Data analysis/2021/Descriptive/Practice.xlsx"
Practice <- read_excel(file, sheet=1)
Practice <- data.frame(Practice)

str(Practice)

summary(Practice)

#stratifying data by condition - summary
by(Practice[,c(2,3,5)],Practice$Condition,summary)

#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/lapply

lapply(Practice[,c(2,3,5)],mean)
lapply(Practice[,c(2,3,5)],median)
sapply(Practice[,c(2,3,5)],quantile)

#deviation
lapply(Practice[,c(2,3,5)],range)
lapply(Practice[,c(2,3,5)],IQR)
lapply(Practice[,c(2,3,5)],var)
lapply(Practice[,c(2,3,5)],sd)

#https://www.r-bloggers.com/2020/11/skewness-and-kurtosis-in-statistics/
install.packages("moments")

lapply(Practice[,c(2,3,5)],moments::skewness)
lapply(Practice[,c(2,3,5)],moments::kurtosis)
