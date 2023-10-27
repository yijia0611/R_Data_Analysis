ggplot(Practice, aes(Town))+geom_bar(aes(fill=Condition), width=0.5)+
  labs(title="Freqencies of Conditions within towns"
       subtitle="Bar plot"
       caption="Source: Practice dataset, own construction")

ggplot(Practice, aes(Town))+geom_bar(aes(fill=Condition), width=0.5)+
  labs(title="Freqencies of Conditions within towns",
       subtitle="Bar plot",
       caption="Source: Practice dataset, own construction")


table(Practice$Town,Practice$Condition)
prop.table(table(Practice$Town,Practice$Condition))
round(prop.table(table(Practice$Town,Practice$Condition)),3)

mosaicplot(table(Practice$Town,Practice$Condition),
           color = TRUE,
           xlab = "Name of Town"
           ylab="Condition of the house")

mosaicplot(table(Practice$Town,Practice$Condition),
           color = TRUE,
           xlab = "Name of Town",
           ylab="Condition of the house")

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

hist(Practice$Size,
     breaks = 10,
     main = "Histogram for Size variable",
     xlab = "Size in sqm",
     ylab = "Frequencies",
     col = "lightblue")

ggplot(Practice, aes(x=Condition, y=Size))+
  geom_boxplot()+stat_summary(fun.y = mean, geom = "point", shape=23, size=4)