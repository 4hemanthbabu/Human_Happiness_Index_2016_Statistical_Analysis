# Analysing HUMNA HAPPINESS INDEX 2016
library(tidyverse)
# Reading the data
print(getwd())
setwd('~/Downloads')
print(getwd())
file = read.csv('2016.csv')
print(file)
# first 10 values
print(head(file, 10))
# last 10 values
print(tail(file, 10))
# column names of the data
print(colnames(file))
# dimention of the data
print(dim(file))
# information about the data
print(?file)
# summary of the data
print(summary(file))
# Minimum and Maximum Valuevalue Happiness Rank Column
print(min(file$Happiness.Score))
print(max(file$Happiness.Score))
# Minimum and Maximum value of Freedom column
print(min(file$Freedom))
print(max(file$Freedom))
# Mean and Median Value of Columns
# 1. Happiness Rank
print(mean(file$Happiness.Score))
print(median(file$Happiness.Score))
# 2. Lower.ConfidencenInterval
print(mean(file$Lower.Confidence.Interval))
print(median(file$Lower.Confidence.Interval))
# 3. Upper.Confiedence.Ineterval
print(mean(file$Upper.Confidence.Interval))
print(median(file$Upper.Confidence.Interval))
# 4. Economy..GDP.per.Capita.
print(mean(file$Economy..GDP.per.Capita.))
print(median(file$Economy..GDP.per.Capita.))
# Freedom
print(mean(file$Freedom))
print(median(file$Freedom))
# Frist, Secon and Third Quartile Of Column Values
# 1. Happiness Score
print(quantile(file$Happiness.Score, 0.25))
print(quantile(file$Happiness.Score,0.50)) 
print(quantile(file$Happiness.Score, 0.75))
# 2. Economy GDP Per Capita
print(quantile(file$Economy..GDP.per.Capita., 0.25))
print(quantile(file$Economy..GDP.per.Capita.,0.50)) 
print(quantile(file$Economy..GDP.per.Capita., 0.75))
# Visulaization
x = file$Country
y = file$Happiness.Score
plot(x, y,  type = 'l', cex = 1, col = 'red', xlab = 'Countries', ylab = 'Freedom_Values', main = 'Countries_with_Freedom_Values', lwd = 2, lty = 6)
plot(x,y)
# Barplot of Countris with Happiness_Score.
x = file$Country
y = file$Happiness.Score
barplot(y, names.arg = x, col = 'red', density = 1, horiz = FALSE)
# Histogram of Economy GDP per capita
v = file$Economy..GDP.per.Capita.
hist(v, col = 'red')
hist(v, col = 'yellow', density = 15, xlab= 'Economy_GDPperCapita', ylab = 'Frequancy', border= 'Black')
# Visualization with ggplot library.
print(ggplot(data = file) + geom_bar(mapping = aes(x = Freedom)))
print(ggplot(data = file) + geom_bar(mapping = aes(x = Happiness.Score)))
print(ggplot(data = file) + geom_bar(mapping = aes(x = Lower.Confidence.Interval)))
print(ggplot(data = file) + geom_bar(mapping = aes(x = Upper.Confidence.Interval)))
print(ggplot(data = file) + geom_histogram(mapping = aes(x = Generosity), binwidth = 0.1))
print(ggplot(data = file) + geom_histogram(mapping = aes(x = Family), binwidth = 0.1))
print(ggplot(data = file) + geom_histogram(mapping = aes(x =Trust..Government.Corruption.), binwidth = 0.1))
print(ggplot(file) + geom_histogram(mapping = aes(x = Happiness.Rank), binwidth = 0.01))
print(ggplot(file) + geom_histogram(mapping = aes(x = Freedom), binwidth = 0.01))
print(ggplot(file) + geom_histogram(mapping = aes(x = Health..Life.Expectancy.), binwidth = 0.01)
print(ggplot(file) + geom_histogram(mapping = aes(x = Generosity), binwidth = 0.01))
(ggplot(data = file) + geom_histogram(mapping = aes(x = y), binwidth = 0.1) + facet_wrap(~Family))
(ggplot(data = file) + geom_histogram(mapping = aes(x = y), binwidth = 0.1) + facet_wrap(~Country))
  