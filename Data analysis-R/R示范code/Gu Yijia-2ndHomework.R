#Name: Gu Yijia
#E-mail (uni-corvinus):
gu.yijia@stud.uni-corvinus.hu
#Used cars data
# Variables: Brand (3 types), Mileage (in 1000 km), Power (horse power), Price (1000 HUF)

#Task 1: import the data set
library(readxl)
Usedcars <- read_excel("Usedcars.xlsx")
View(Usedcars)

Usedcars

#Task 2: ask a summary, check data type, are the variables stored appropriately?
##name values
Price<-Usedcars$Price
Brand<-Usedcars$Brand
Mileage<-Usedcars$Mileage
##summary data type 
typeof(Price)
typeof(Brand)
typeof(Mileage)
typeof(Usedcars)

# Task 3: ask a summary about variables
summary(Usedcars)

##group by and summary Mercedes brand
Usedcars %>% 
  filter(Brand == "Mercedes") %>% 
  summary(Price)

##group by and summary BMW brand
Usedcars %>% 
  filter(Brand == "BMW") %>% 
  summary(Price)

##group by and summary Audi brand
Usedcars %>% 
  filter(Brand == "Audi") %>% 
  summary(Price)

# Task 4: depict boxplot for Price variable by Brands
library(ggplot2)
g <- ggplot(Usedcars, aes(Brand, Price))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="Price variable by Brands",
       caption="Source: Usedcars",
       x="Brand of vichecle",
       y="Price")

# Task 5: depict at least 3 informative graphs by ggplot2
install.packages("ggplot2")
library(ggplot2)

## Scatterplot

gg<-ggplot(Usedcars, aes(x=Brand, y=Price))+
  geom_point(aes(col=,size=))+
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(subtitle="Brand Vs Price", 
       y="Price", 
       x="Brand", 
       title="Scatterplot", 
       caption = "Source: Usedcars")
plot(gg)
##bar
ggplot(Usedcars, aes(Mileage))+geom_bar(aes(fill=Price), width=0.5)+
  labs(title="Freqencies of price by Mileage",
       subtitle="Bar plot",
       caption="Source: Usedcars dataset, own construction by YijiA")

##create crosstabs
table(Usedcars$Brand,Usedcars$Power)
prop.table(table(Usedcars$Brand,Usedcars$Power))
round(prop.table(table(Usedcars$Brand,Usedcars$Power)),3)
#mosaicplot 
mosaicplot(table(Usedcars$Mileage,Usedcars$Price),
           color = TRUE,
           xlab = "Mileage",
           ylab = "Price")
##boxplot
ggplot(Usedcars, aes(x=Power, y=Price))+
  geom_boxplot()+stat_summary(fun.y = mean, geom = "point", shape=23, size=4)


# Task 6: calculate the following indicators for quantitative variables:
#mean, median, quartiles, deciles, standard deviation, relative standard deviation
lapply(Usedcars[,c(2,3,4)],mean)
lapply(Usedcars[,c(2,3,4)],median)
sapply(Usedcars[,c(2,3,4)],quantile)
lapply(Usedcars[,c(2,3,4)],quantile)
#calculate deciles
quantile(Power, probs = (1:9)/10)
quantile(Price, probs = (1:9)/10)
quantile(Mileage, probs = (1:9)/10)

lapply(Usedcars[,c(2,3,4)],var)
lapply(Usedcars[,c(2,3,4)],sd)
lapply(Usedcars[,c(2,3,4)],range)
sd<-lapply(Usedcars[,c(2,3,4)],sd)
#relative stad. deviation
##Price
sd(Price)/mean(Price)
##Power
sd(Power)/mean(Power)
##MIleage
sd(Mileage)/mean(Mileage)

#skewness, kurtosis
lapply(Usedcars[,c(2,3,4)],moments::skewness)
lapply(Usedcars[,c(2,3,4)],moments::kurtosis)

# Task 7: do outlier filtering if it is necessary 


boxplot(Mileage)
boxplot(Price)
Power<-Usedcars$Power
boxplot(Power)


g <- ggplot(Usedcars, aes(Brand, Price))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="Price variable by Brands",
       caption="Source: Usedcars",
       x="Brand of vichecle",
       y="Price")
outlier_values <- boxplot.stats(Usedcars$Price)$out  # outlier values.
boxplot(Usedcars$Price, main="Price", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)
#detect Z area by sd
sd<-sd(Price)
sd(Price)
mean_Price<-mean(Price)
Usedcars_sd<- (Price-mean_Price)/sd
#see if there are outliers
if (Usedcars_sd>3) {
  print("outlier")
} else {
  print("no outlier")
}
if (Usedcars_sd<-3) {
  print("outlier")
} else {
  print("no outlier")
}


###Write an essay (at least 5000 characters) about your result (word document).
#It should contain:
# short description about the variables (data type, unit of measure, scale of measurement)
# analyse the depicted graphs and boxplot
# analyse and interpret the calculated desriptive statistics
# write about outlier issues and the method of outlier filtering
# 15 points can be obtained for the task!
#Deadline: 17th of October, 23:55

