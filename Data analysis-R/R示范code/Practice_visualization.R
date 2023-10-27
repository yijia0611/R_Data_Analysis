library(readxl)

#import the file
file <- "C:/Egyetem/Data analysis/2021/Descriptive/Practice.xlsx"
Practice <- read_excel(file,sheet = 1)

Practice <- data.frame(Practice)

#GGPLot
install.packages("ggplot2")
library(ggplot2)

#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Histogram

#bar plot
ggplot(Practice, aes(Town))+geom_bar()

ggplot(Practice, aes(Town))+geom_bar(aes(fill=Condition), width=0.5)+
  labs(title="Freqencies of Conditions within towns",
       subtitle="Bar plot",
       caption="Source: Practice dataset, own construction")

#create crosstabs
table(Practice$Town,Practice$Condition)
prop.table(table(Practice$Town,Practice$Condition))
round(prop.table(table(Practice$Town,Practice$Condition)),3)

#crosstabs visualization
mosaicplot(table(Practice$Town,Practice$Condition),
           color = TRUE,
           xlab = "Name of Town",
           ylab="Condition of the house")
# distribution analysis
hist(Practice$Size)
#optimal number of intervals: k=log2(n) --- 2^k>n
hist(Practice$Size,
     breaks = 10,
     main = "Histogram for Size variable",
     xlab = "Size in sqm",
     ylab = "Frequencies",
     col = "lightblue")

#boxplot
boxplot(Practice$Size)

boxplot(Practice$Size~Practice$Condition, varwidth=T)

ggplot(Practice, aes(x=Condition, y=Size))+
  geom_boxplot()+stat_summary(fun.y = mean, geom = "point", shape=23, size=4)

#detecting outliers by standardization

Practice$Price_z <- (Practice$Price-mean(Practice$Price))/sd(Practice$Price)

boxplot(Practice$Price_z)
hist(Practice$Price_z)
#|z-score|>3 ---> it is an outlier
#observation (not standardized)>Q3+1.5*IQR or <q1-1.5*IQR - also outlier
#IQR: Q3-Q1

Practice$outlier <- ifelse(Practice$Price_z>3,1,0)
Practice$outlier
#create a new data frame and would like to remove some outliers 
Practice2 <- Practice[Practice$outlier == 0,]
