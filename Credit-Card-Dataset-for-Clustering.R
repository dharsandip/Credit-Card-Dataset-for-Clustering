#-----Credit Card Dataset for Clustering---------------------------

setwd('C:\\Sandip\\Machine-Learning\\Practice-Datasets\\Credit-Card-Dataset-for-Clustering')
getwd()
dataset_orig = read.csv('CC GENERAL.csv')
dataset = dataset_orig[2:18]
head(dataset)
str(dataset)

#----------------------------------------------------------
#levels(factor(is.na(dataset$MINIMUM_PAYMENTS)))

dataset$CREDIT_LIMIT = ifelse(is.na(dataset$CREDIT_LIMIT), ave(dataset$CREDIT_LIMIT, 
                                                                   FUN = function(x) mean(x, na.rm = TRUE)), dataset$CREDIT_LIMIT)

dataset$MINIMUM_PAYMENTS = ifelse(is.na(dataset$MINIMUM_PAYMENTS), ave(dataset$MINIMUM_PAYMENTS, 
                                                               FUN = function(x) mean(x, na.rm = TRUE)), dataset$MINIMUM_PAYMENTS)

#------Exploratory Data Analysis----------------------------------
library(ggplot2)
r = ggplot(data=dataset, aes(x=CREDIT_LIMIT, y=PURCHASES))
r + geom_point()
r + geom_point(aes(colour=TENURE))

s <- ggplot(data=dataset, aes(x=PURCHASES_TRX))
s + geom_histogram(binwidth = 10, fill='Green', colour='Black') + xlab('NO. OF PURCHASE TRANSACTIONS MADE BY CUSTOMERS')

t = ggplot(data=dataset, aes(x=PAYMENTS, y=PURCHASES))
t + geom_point()
t + geom_point(aes(colour=TENURE))

u <- ggplot(data=dataset, aes(x=CASH_ADVANCE_TRX))
u + geom_histogram(binwidth = 10, fill='Green', colour='Black') + xlab('NO. OF CASH ADVANCE TRANSACTIONS MADE BY CUSTOMERS')

v = ggplot(data=dataset, aes(x=BALANCE, y=ONEOFF_PURCHASES))
v + geom_point()
v + geom_point(aes(colour=TENURE, size = CREDIT_LIMIT))

#---------Applying kmeans clustering-------------------------------------------
#set.seed(6)
library(cluster)

wcss = vector()

for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)

plot(1:10, wcss, type = 'b', main=paste('Credit Card Users'), xlab = 'no. of clusters')

kmeans = kmeans(dataset, 6, iter.max = 300, nstart = 10)

kmeans1 = data.frame(kmeans$cluster)
dataset1 = data.frame(dataset_orig, kmeans1)

#--------Plotting and Analyzing the results with clusters------------------------

w <- ggplot(data=dataset1, aes(x=kmeans.cluster))
w + geom_histogram(binwidth = 1, fill='Blue', colour='Black') + 
  xlab('Cluster Number') + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  scale_y_continuous(breaks = seq(0, 6000, by = 500)) + ggtitle('Number of Credit Card Customers in different Clusters')

a = ggplot(data=dataset1, aes(x=PURCHASES, y=PAYMENTS))
a + geom_point()
a + geom_point(aes(colour=kmeans.cluster, size = CREDIT_LIMIT))

b = ggplot(data=dataset1, aes(x=CREDIT_LIMIT, y=PURCHASES))
b + geom_point()
b + geom_point(aes(colour=kmeans.cluster))

b = ggplot(data=dataset1, aes(x=BALANCE, y=ONEOFF_PURCHASES))
b + geom_point()
b + geom_point(aes(colour=kmeans.cluster))





