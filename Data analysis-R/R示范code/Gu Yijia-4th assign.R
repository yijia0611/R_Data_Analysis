library(readr)
employee <- read_delim("~/employee.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(employee)

employee


#Estimate 90% confidence interval for the expected value in case of two quantitative variables.
qt(0.95,472)
mean(employee$salary)
mean(employee$prevexp)
ev_salary<- mean(employee$salary)+c(-1,1)*qt(0.95,472)*sd(employee$salary)/sqrt(473)
ev_salary


ev_prevexp<- mean(employee$prevexp)+c(-1,1)*qt(0.95,472)*sd(employee$prevexp)/sqrt(473)
ev_prevexp

#IC 98%  for minority porpotion
alpha=0.02 #significance value
n=length(employee$minority)
tableminority=table(employee$minority)
table(employee$minority)['Yes']
Propofminority=tableminority['Yes']/n
z = qnorm(1-alpha/2)
minority_prop_estimate <- Propofminority + c(-1,1)*z*sqrt(Propofminority*(1-Propofminority)/n)
minority_prop_estimate


#Statement:proportion of female workers should be not higher than  48%
employee$dummy_female <- ifelse(employee$gender == "Female", 1, 0)
Tablefemale=table(employee$dummy_female)
prop.test(Tablefemale, alternative='greater', p=.48, conf.level=.95, 
          correct=FALSE)


#Statement:the average years of education is small or equal to 13 years
t.test (employee$educ , 
         alternative = "greater" ,
         mu = 13 ) 

#Statement:the average salary of men and women is equal
#first approch_group by 
man_salary <-employee %>% 

  filter(str_detect(gender , "Male")) %>% 
  pull(salary)

woman_salary <-employee %>% 
  filter(str_detect(gender , "Female")) %>% 
  pull(salary)
salary_ratio <- mean(man_salary)/mean(woman_salary)
t.test (man_salary,woman_salary )

#second approach_use dummy
employee$dummy <- ifelse(employee$gender == 'Female' , 1, 0)
t.test( x= employee$salary[employee$dummy == 1] ,
          y= employee$salary[employee$dummy == 0] ,
          var.equal = F )


#Test the relation between:
#two qualitative variables : gender and jobcat
#independence test for qualitative variables
install.packages("gmodels")
library ( gmodels ) 


CrossTable ( employee$gender , employee$jobcat , 
             chisq = T , sresid = T , 
             fisher = T , 
             format = "SPSS" ) 


employee %>% 
ggplot(aes(gender))+
geom_bar(aes(fill=jobcat))+
  labs( x= "jobcat",y = "gender"
  )



#df=(r-1)*(c-1)

#Anova£ºone quantitative and one qualitative variables :jobcat¡¢salary
#for jobcat and salary
library( ggpubr )
ggboxplot(employee, x = "jobcat", y = "salary", 
          color = "jobcat", 
          ylab = "salary", xlab = "jobcat")

mod1 <- aov (employee$salary ~ employee$jobcat , data = employee ) 
summary ( mod1 ) 

#for gender and education
ggboxplot(employee, x ="gender", y = "educ",
          color = "gender",
          xlab =  "gender", ylab = "education")

mod2 <- aov(employee$educ ~ employee$gender , data = employee)
summary(mod2)
#heatmap£º at least two quantitative variables.
install.packages("ggcorrplot")
library ( ggcorrplot )
library(readr)
employee_copy <- read_delim("employee - copy.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(employee_copy)

employee_copy

num <- sapply ( employee_copy , is.numeric ) 
cov ( employee_copy [ , num ] )
cor ( employee_copy [ , num ] ) 

ggcorrplot ( cor ( employee_copy [ , num ]  )  , 
             method = "square" , 
             lab = T , 
             hc.order = T ) 
