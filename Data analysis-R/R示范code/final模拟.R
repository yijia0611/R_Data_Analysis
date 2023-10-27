library(readxl)
ExamData2 <- read_excel("ExamData2.xlsx")
View(ExamData2)

ExamData2
d <- ExamData2 

#2 .detect outlier 
d$price_z <- (d$price - mean(d$price))/sd(d$price)
d$outlier_price <- ifelse( d$price_z >3 , 1 , 0)
d$outlier_price
d1 <- d[d$outlier_price==0,]

d1$size_z <- (d1$size - mean(d1$size))/sd(d1$size)
d1$outlier_size <- ifelse( d1$size_z >3 , 1 , 0)
d1$outlier_size
d2 <- d1[d1$outlier_size==0,]

#boxplot and hist
boxplot(d$price)
boxplot(d$size)
hist(d$price)
hist(d$size,breaks = 10)


d2$type_z <- (d2$type - mean(d2$type))/sd(d2$type)
d2$outlier_type <- ifelse( d2$type_z >3 , 1 , 0)
d2$outlier_type


#d2 is the one that has removed outlier

#3.
summary(d2)
psych::describe(d2)


#5.
me_price <- qt(0.95,989)*sd(d2$price)/sqrt(990)
lower_higher_price <- mean(d2$price)+ c(-1,1)*me_price
lower_higher_price

me_size <- qt(0.95,989)*sd(d2$size)/sqrt(990)
lower_higher_size <- mean(d2$size)+ c(-1,1)*me_size
lower_higher_size

n = length(d2$type)
propoofbrick <- table(d2$type)["1"]/n
brick <- propoofbrick+c(-1,1)*qnorm(0.95)*sqrt((propoofbrick*(1-propoofbrick))/n)
brick

#6.furniture and price
mod1 <- aov(d2$price ~ d2$furniture , data= d2)
summary(mod1)
d2$dummy_fur <- ifelse(d2$furniture== "1", 1, 0)
t.test(x=d2$price[d2$dummy_fur== 1],
       y=d2$price[d2$dummy_fur== 0] ,
       var.equal = F)

employee$dummy <- ifelse(employee$gender == 'Female' , 1, 0)
t.test( x= employee$salary[employee$dummy == 1] ,
        y= employee$salary[employee$dummy == 0] ,
        var.equal = F )
#7.
mod2 <- aov(d2$condition ~ d2$price , data= d2)
summary(mod2)

#8.crosstab
CrossTable(d2$condition, d2$type , chisq = T , fisher = T , format = "SPSS" )
num <- sapply ( Practice , is.numeric ) 
num <- sapply(d2,is.numeric)
cov (d2[,num])


cov ( d [ , num ] )
cor ( d [ , num ] ) 

