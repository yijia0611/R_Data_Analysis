##Project work: Analyzation For World Happiness Report 
##Group 3
##Group member: Zhao Weiyi, Gu Yijia, Xiong Zijian, WANGKE
##Data Source: Kaggle


#Importing Dataset
library(readxl)
DA_project_work <- read_excel("Desktop/fisrt/DA_project work.xlsx")
View(DA_project_work)
PW <- DA_project_work

#Short description about variables, type of variables
str(PW)
summary(PW)

#Descriptive statistics for numeric variables: mean, median, quartiles, deviation, skewness, kurtosis
install.packages("psych")
psych::describe(PW)

#Outlier detecting and removing
PW$Income_z <- (PW$Income-mean(PW$Income))/sd(PW$Income)
PW$outlier <- ifelse(PW$Income_z>3,1,0)
PW1 <- PW[PW$outlier == 0, ]
view(PW1)

#Boxplot
ggplot(PW1, aes(x=Continent, y=HappyScore))+
  geom_boxplot()+stat_summary(fun=mean, geom = "Point", shape= 23, size=4)

###### rename the variables #####
continent <- PW1$Continent
summary(continent)

dev <- PW1$Dev
summary(dev)

happyscore <- PW1$HappyScore
summary(happyscore)

gdp <- PW1$GDP
summary(gdp)

satisfacation <- PW1$Satisfaction
summary(satisfacation)

income <- PW1$Income
summary(income)

incomeinequality <- PW1$IncomeInequality
summary(incomeinequality)


## Ratios, frequencies and figures for qualitative variables

#Frequencies
tableContinent=table(PW1$Continent)
tableContinent

tableDev=table(PW1$Dev)
tableDev

#Ratios
n <- 109

PropofAfrica=tableContinent['Africa']/n
PropofAfrica

PropofAmerica=tableContinent['America']/n
PropofAmerica

PropofAsia=tableContinent['Asia']/n
PropofAsia

PropofEurope=tableContinent['Europe']/n
PropofEurope

PropofOceania=tableContinent['Oceania']/n
PropofOceania

PropofDeveloped=tableDev['developed']/n
PropofDeveloped

PropofDeveloping=tableDev['developing']/n
PropofDeveloping


#Graph for qualitative variables
ggplot(PW1, aes(continent))+geom_bar()

ggplot(PW1, aes(continent))+geom_bar(aes(fill=dev), width=0.5)+
  labs(title="Freqencies of developed or developing within continents",
       subtitle="Bar plot",
       caption="Source: Kaggle: World Happy Score dataset,own construction")


#happyscore_Mean_Africa 
filter(PW1, continent == "Africa")
Africa_mean <- filter(PW1, continent == "Africa")
lapply(Africa_mean[,c(3)],mean)

#hapyscore_Mean_America
filter(PW1, Continent == "America")
America_mean <- filter(PW1, Continent == "America")
lapply(America_mean[,c(3)],mean)

#happyscore_Mean_Asia
filter(PW1, Continent == "Asia")
Asia_mean <- filter(PW1, Continent == "Asia")
lapply(Asia_mean[,c(3)],mean)

#happyscore_Mean_Europe
filter(PW1, Continent == "Europe")
Europe_mean <- filter(PW1, Continent == "Europe")
lapply(Europe_mean[,c(3)],mean)

#happyscore_Mean_Oceania
filter(PW1, Continent == "Oceania")
Oceania_mean <- filter(PW1, Continent == "Oceania")
lapply(Oceania_mean[,c(3)],mean)


##Confidence interval estimation for variables

happyscore_mean <- mean(PW1$HappyScore)
margin_of_error <- qnorm(0.975)*sd(PW1$HappyScore)/sqrt(109) #CI=95%
intlow <- happyscore_mean - margin_of_error
intup <- happyscore_mean + margin_of_error
intlow
intup
c(intlow,intup)


library (Hmisc) 
smean.cl.normal (PW1$HappyScore)
by(PW1$HappyScore, PW1$Continent, smean.cl.normal)


GDP_mean <- mean(PW1$GDP)
margin_of_error <- qnorm(0.975)*sd(PW1$GDP)/sqrt(109) #CI=95%
intlow <- GDP_mean - margin_of_error
intup <- GDP_mean + margin_of_error
intlow
intup
c(intlow,intup)

Income_mean <- mean(PW1$Income)
margin_of_error <- qnorm(0.975)*sd(PW1$Income)/sqrt(109) #CI=95%
intlow <- Income_mean - margin_of_error
intup <- Income_mean + margin_of_error
intlow
intup
c(intlow,intup)

Satisfaction_mean <- mean(PW1$Satisfaction)
margin_of_error <- qnorm(0.975)*sd(PW1$Satisfaction)/sqrt(109) #CI=95%
intlow <- Satisfaction_mean - margin_of_error
intup <- Satisfaction_mean + margin_of_error
intlow
intup
c(intlow,intup)

IncomeInequality_mean <- mean(PW1$IncomeInequality)
margin_of_error <- qnorm(0.975)*sd(PW1$IncomeInequality)/sqrt(109) #CI=95%
intlow <- IncomeInequality_mean - margin_of_error
intup <- IncomeInequality_mean + margin_of_error
intlow
intup
c(intlow,intup)


## Analysis of relation between variables (crosstabs, Anova, covariance and correlation)

#crosstabs
install.packages("gmodels")
library ( gmodels ) 
CrossTable(PW1$Dev, PW1$Continent, 
           chisq = T, sresid = T, 
           fisher = T, 
           format = "SPSS")


#ANOVA between Continent and GDP 01
install.packages("ggpubr")
library(ggpubr)

ggboxplot(PW1, x = "Continent", y = "GDP", 
          color = "Continent", 
          ylab = "GDP", xlab = "Continent")

aov1 <- aov (PW1$GDP ~ PW$Continent , data = PW1 )
summary ( aov1 )


#ANOVA between Continent and HappyScore 02
ggboxplot(PW1, x = "Continent", y = "HappyScore", 
          color = "Continent", 
          ylab = "HappyScore", xlab = "Continent")

aov2 <- aov (PW1$HappyScore ~ PW1$Continent , data = PW1 )
summary ( aov2 )


#covariance
num <- sapply ( PW1 , is.numeric ) 
cov ( PW1 [ , num ] )


#correlation
install.packages("ggcorrplot")
library ( ggcorrplot )
num <- sapply ( PW1 , is.numeric ) 
cor ( PW1 [ , num ] ) 

ggcorrplot ( cor ( PW1 [ , num ]  )  , 
             method = "square" , 
             lab = T , 
             hc.order = T ) 


##3 rational hypothesis testing for variables (different group means, ratios etc.)

#mean test for Income , Statement seen from BBC news :"Let's put the world's average GDP income-- almost $18,000a year "(2020)
t.test ( PW1$Income , 
         alternative = "two.sided" ,
         mu = 18000 )  
#statement  : proportion of developed  countries should be as least as  50% 
PW1$dummy_Dev <- ifelse(PW1$Dev == "developing" , 1, 0)
Tabledev=table(PW1$dummy_Dev)
prop.test(Tabledev, alternative='less', p=.5, conf.level=.95, 
          correct=FALSE)

#statement Proportion = 50% for greater happyscore that is above average and smaller happyscore that is below average
#create  dummy variables
PW1$dummy_HappyScore <- ifelse(PW1$HappyScore> 5.42, 1, 0)
PW1$dummy_HappyScore
PW1

#Proportion test 
Tablehappy=table(PW1$dummy_HappyScore)
Tablehappy
prop.test(Tablehappy, alternative='greater', p=.5, conf.level=.95, 
          correct=FALSE)
# p = 0.1702, fail to reject H0 :p=50%  the real proportion could be around 54.54%

# Two independent samples variance T-test about average happiness score
var.test(x = PW1$HappyScore[PW1$dummy_Dev == 1] , 
         y = PW1$HappyScore[PW1$dummy_Dev == 0])


t.test ( x = PW1$HappyScore[PW1$dummy_Dev == 1] , 
         y = PW1$HappyScore[PW1$dummy_Dev == 0] , 
         var.equal = T )

# Two independent samples variance T-test about average income
var.test(x = PW1$Income[PW1$dummy_Dev == 1] , 
         y = PW1$Income[PW1$dummy_Dev == 0])


t.test ( x = PW1$Income[PW1$dummy_Dev == 1] , 
         y = PW1$Income[PW1$dummy_Dev == 0] , 
         var.equal = F )

#Statement : ratio between number of happy score that is upper and lower than mean 
#should be equal to 1
var.test(x = PW1$HappyScore[PW1$dummy_HappyScore == 1] , 
         y = PW1$HappyScore[PW1$dummy_HappyScore == 0])
#p =0.488, fail to reject H0 : ratio of variance is equal to 1 .


#statement  :proportion between number of lower GDP countries and higher GDP countries should be 50%
PW1$dummy_GDP <- ifelse(PW1$GDP > 0.84, 1, 0)
TableGDP=table(PW1$dummy_GDP)
TableGDP
var.test(x = PW1$GDP[PW1$dummy_GDP == 1] , 
         y = PW1$GDP[PW1$dummy_GDP == 0])
#p= 0.01001  , reject H0 :ratio is equal to 50% 


#Statement  :Proportion  of higher incomes that is above average and lower income that is lower than average , is  equal to 50%
#lower and upper income ratio
PW1$dummy_Income <- ifelse(PW1$Income > 6477.44, 1, 0)
var.test(x = PW1$Income[PW1$dummy_Income == 1] , 
         y = PW1$Income[PW1$dummy_Income == 0])

var.test(x = PW1$HappyScore[PW1$dummy_Dev == 1] , 
         y = PW1$HappyScore[PW1$dummy_Dev == 0])




