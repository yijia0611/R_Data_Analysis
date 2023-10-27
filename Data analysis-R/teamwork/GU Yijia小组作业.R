library(readxl)
DA_project_work_ <- read_excel("DA_project work_.xlsx")
View(DA_project_work_)

DA_project_work_

# 7. At least 3 rational hypothesis testing for variables
# (different group means, ratios etc.)

#detect and remove outlier for income
pw <- DA_project_work_
pw$Income_z <- (pw$Income-mean(pw$Income))/sd(pw$Income)
pw$outlier <- ifelse(pw$Income_z>3,1,0)
pw1 <- pw[pw$outlier == 0, ]
#mean test for Income , Statement seen from BBC news :"Let's put the world's average GDP income-- almost $18,000a year "(2020)
t.test ( pw1$Income , 
         alternative = "two.sided" ,
         mu = 18000 )  
#statement  : proportion of developed  countries should be as least as  50% 
pw$dummy_Dev <- ifelse(pw$Dev == "developing" , 1, 0)
Tabledev=table(pw$dummy_Dev)
prop.test(Tabledev, alternative='less', p=.5, conf.level=.95, 
          correct=FALSE)

#statement  £ºPorpotion=50% for greater happyscore that is above average and smaller happyscore that is below average
#create  dummy variables
pw$dummy_HappyScore <- ifelse(pw$HappyScore> 5.42, 1, 0)
pw$dummy_HappyScore
pw
#proportion test 
Tablehappy=table(pw$dummy_HappyScore)
Tablehappy
prop.test(Tablehappy, alternative='greater', p=.5, conf.level=.95, 
          correct=FALSE)
# p = 0.1702, fail to reject H0 :p=50% £¬ the real proportion could be around 54.54%

#Statement : ratio between number of happy score that is upper and lower than mean 
#should be equali to 1

var.test(x = pw$HappyScore[pw$dummy_HappyScore == 1] , 
         y = pw$HappyScore[pw$dummy_HappyScore == 0])

#p =0.488, fail to reject H0 : ratio of variance is equal to 1 .


#statement  :proportion between number of lower GDP countries and higher GDP countries should be 50%
pw$dummy_GDP <- ifelse(pw$GDP > 0.84, 1, 0)
TableGDP=table(pw$dummy_GDP)
TableGDP
var.test(x = pw$GDP[pw$dummy_GDP == 1] , 
         y = pw$GDP[pw$dummy_GDP == 0])
#p= 0.01001  , reject H0 :ratio is equal to 50% 

#Statement  :Proportion  of higher incomes that is above average and lower income that is lower than average , is  equal to 50%
#lower and upper income ratio
pw$dummy_Income <- ifelse(pw$Income > 6477.44, 1, 0)
var.test(x = pw$Income[pw$dummy_Income == 1] , 
         y = pw$Income[pw$dummy_Income == 0])

#Statement £ºthe average income of developed and developing countries should be the same
pw$dummy_dev <- ifelse(pw$Dev == "developed" , 1 ,0) 
t.test(x = pw$Income[pw$dummy_dev == 1] , 
         y = pw$Income[pw$dummy_dev == 0] ,
         var.equal = F)
