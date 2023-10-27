file <- "C:/Egyetem/Data analysis/2021/Descriptive/Practice.xlsx"
library(readxl)
Practice <- read_excel(file)

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
