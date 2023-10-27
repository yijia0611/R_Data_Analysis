file <- "C:/Egyetem/Data analysis/2021/Descriptive/Practice.xlsx"
library(readxl)
Practice <- read_excel(file)



library(readxl)
Practice <- read_excel("~/Practice.xlsx")
View(Practice)

Practice

summary(Practice)

install.packages("psych")
psych::describe(Practice)

#standard normal distribution
Samplemean <- mean(Practice$Price)
margin_of_error <- qnorm(0.975)*sd(Practice$Price)/sqrt(150)
intlow <- Samplemean - margin_of_error
intup <-Samplemean + margin_of_error
intlow
intup
c(intlow,intup)

# Student t distribution
margin_of_error_t <- qt(0.975, df=150-1)*sd(Practice$Price)/sqrt(150)
intlow_T <- Samplemean - margin_of_error_t
intup_T <-Samplemean + margin_of_error_t
intlow_T
intup_T
c(intlow_T,intup_T)
c(intlow,intup)


install.packages("Hmisc")
library(Hmisc)
smean.cl.normal(Practice$Price)

#difference between t and z
qt(0.975, df=150-1)
qnorm(0.975)
qt(0.975, df=30-1)
qt(0.975, df=15000-1)

#estimation by inbuilt function and using stratification
library ( Hmisc ) 
smean.cl.normal ( Practice$Price  )
by(Practice$Price, Practice$Condition, smean.cl.normal)


# confidence interval for Proportion

alpha=0.05 #significance value
n=length(Practice$Condition)
tableCondition=table(Practice$Condition)
Propofgood=tableCondition['good']/n
Propofgood
z = qnorm(1-alpha/2)
Propofgood + c(-1,1)*z*sqrt(Propofgood*(1-Propofgood)/n)

#int(p) =p+/- Z *sqrt (p(1-p)/n)计算P的interval区间
#central limit theory
#Why do we use normal distribution?

#Another method: bootstrap. It is a resampling method, general in econometrics and machine learning techniques
library(boot)

#create a mean function to calculate mean for each subsample
mean.function <- function(x, index) {
  d <- x[index]
  return(mean(d))  }
#10000 bootstrap iterations - mean for every resample
BootDist <- boot(data = Practice$Price, statistic = mean.function, R=10000)
head(BootDist$t)

#Distribution of mean of resamples
BootDist.graph <- data.frame(xbar=BootDist$t)
ggplot(BootDist.graph, aes(x=xbar)) +
  geom_histogram() + 
  ggtitle('Estimated Sampling distribution of Price' )

#quantaile based interval for Price
quantile( BootDist$t, probs=c(.025, .975) )

######################################################################
#######################Hypothesis testing#############################
######################################################################

###one-sample parametric tests

# t-test for expected value

#statement: the mean Price is 30000(H0)

onesample.t.test1 <- t.test ( Practice$Price , 
                              alternative = "two.sided" ,
                              mu = 30000 )  
#(bigger or smaller than 30000, two sided)
onesample.t.test1

#statement: the mean Price is greater than 30000

onesample.t.test2 <- t.test ( Practice$Price , 
                              alternative = "greater" ,
                              mu = 30000 )  
#here the statement is u >30000, so here the H0 :u<=30000, HT0 ; u=30000. if 
#, if we cant reject , we can only accept HT0, but not H0 , bcoz H0 has infinite
# values .
onesample.t.test2

#statement: the mean Price is greater than 30000

onesample.t.test2 <- t.test ( Practice$Price , 
                              alternative = "greater" ,
                              mu = 30000 )  
onesample.t.test2

#statement: the mean Price is at least 27000

onesample.t.test3 <- t.test ( Practice$Price , 
                              alternative = "less" ,
                              mu = 27000 )  
onesample.t.test3
#ho :u >=27000, h1: u< 27000
#be careful with statement , sometimes could get contradiction. 

# testing proportion
#create a dummy variable
Practice$dummy <- ifelse(Practice$Condition == 'excellent', 1, 0)


#dummy using 1 or 0 to tell the condition is excellent or not. 
#if it is excellent , it would be 1, if not then 0 .
Tableexcellent=table(Practice$dummy)
Tableexcellent
prop.test(Tableexcellent, alternative='greater', p=.5, conf.level=.95, 
          correct=FALSE)
# p=.5 mean 5%
###two-sample parametric test (independent)

#precondition - variances should be equal
var.test(x = Practice$Price[Practice$dummy == 1] , 
         y = Practice$Price[Practice$dummy == 0])


ind.t.test <- t.test ( x = Practice$Price[Practice$dummy == 1] , 
                       y = Practice$Price[Practice$dummy == 0] , 
                       var.equal = F )
ind.t.test

ind.t.test2 <- t.test ( Practice$Price ~ Practice$dummy , data = Practice , 
                        var.equal = F, alternative = "less" )
ind.t.test2


#http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r


###non-parametric tests

#normality test
shapiro.test(Practice$Price)

install.packages("ggpubr")
library(ggpubr)

ggqqplot(Practice$Price)

#independence test for qualitative variables
install.packages("gmodels")
library ( gmodels ) 

CrossTable ( Practice$Town , Practice$Condition , 
             chisq = T , sresid = T , 
             fisher = T , 
             format = "SPSS" ) 
#df=(r-1)*(c-1)

#ANOVA - relation between one qualitative and one quantitative

ggboxplot(Practice, x = "Condition", y = "Price", 
          color = "Condition", 
          ylab = "Price", xlab = "Condition")



mod <- aov (Practice$Price ~ Practice$Condition , data = Practice ) 
summary ( mod ) 

#http://www.sthda.com/english/wiki/one-way-anova-test-in-r

#correlation - relation between quantitative variables
install.packages("ggcorrplot")
library ( ggcorrplot )


num <- sapply ( Practice , is.numeric ) 
cov ( Practice [ , num ] )
cor ( Practice [ , num ] ) 

ggcorrplot ( cor ( Practice [ , num ]  )  , 
             method = "square" , 
             lab = T , 
             hc.order = T ) 
#correlation heatmap is a good figure to depict relation
#http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization