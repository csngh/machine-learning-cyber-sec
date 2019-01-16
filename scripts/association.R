#[1.]-----Importing Libraries-----
#Association rules package
library(arules)
#Package to visualize association rules
library(arulesViz)

#[2.]-----Importing dataset-----
#With headers and making columns as factor variables
dataset <- read.csv('system_logs_sample.csv', header = T, colClasses = 'factor')
#Removing irrelavant features w.r.t results from 'feature_ranking.R' script
dataset <- dataset[, c(-2, -3, -4, -5, -6, -7, -10)]

#[3.]-----Generating rules with limitations of minimum 2 and maximum 3-----
rules <- apriori(dataset, parameter = list(minlen = 2, maxlen = 3))
#Summarizing the rules (quick look)
summary(rules)

#[4.]-----Plotting rules-----
plot(rules, method = 'grouped')
