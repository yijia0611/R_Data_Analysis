library(readxl)
Practice <- read_excel("Practice.xlsx")
View(Practice)

Practice

summary(Practice)
#下载并使用Practice包
psych::describe(Practice)
summary(Practice)
sapply(Practice[,c(2,3,4,5,6)] ,moments::skewness)
#trimmed是去除异常值之后的mean，一种mean

#standard normal distribution
Samplemean <-mean(Practice$Price)
#quantile of the distribution
margin_of_error <- qnorm(0.975)*sd(Practice$Price)/sqrt(150)
intlow <- Samplemean - margin_of_error
intup <- Samplemean + margin_of_error
intlow
intup
#在 sample里面，97.5% 的可能性这个interval （c定义的）包含 mean
c(intlow,intup)

#confidence interval from t distribution (student t distribution)
margin_of_error_t <- qt(0.975, df = 150-1)*sd(Practice$Price)/sqrt(150)
intlow_T <- Samplemean - margin_of_error_t
intup_T <- Samplemean + margin_of_error_t
intlow_T
intup_T
#对比T分布和sd 正态分布的interval不太一样，shape of T is fatter。
#small sample 用T，因为large 用T和sd几乎一样
#总的来讲就是都用T就不会错
c(intlow_T,intup_T)
c(intlow,intup)

library(Hmisc)

smean.cl.normal(Practice$Price)
#different T and Z
qt(0.975, df=150-1)
qnorm(0.975)
qt(0.975, df=30-1)
qt(0.975, df=1500-1)
#number 越多， uncertainty会更高
#T的这个值会不断converge 往sd的情况，但是如果sample越大， 它会更加贴近。
