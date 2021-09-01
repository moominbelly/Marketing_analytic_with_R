## marketing analytic with R   

# computing recency, frequnecy, monetary value 
```
data <- read.delim('purchase.txt', header= FALSE, sep = '\t', dec = '.')
```
### Adding headers as date and year of pruchase
```colnames(data) <- c('customer_id', 'purchase_amount', 'date_of_purchase')
data$data_of_purchase = as.Date(data$data_of_purchase, "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$data_of_purchase, "%Y"))
data$days_since = as.numeric(difftime(time1 =  "2016-01-01",
                                      time2 = data$data_of_purchase,
                                      units = "days"))
```
### Explore the data 
```
head(data)
```
summary(data)

#compute key marketing indicatier using SQL language
library(sqldf)
customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                      MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data 
                       GROUP BY 1")
head(customers_2015)
summary(customers_2015)
hist(customers_2015$recency)
hist(customers_2015$frequency)
hist(customers_2015$amount)
hist(customers_2015$amount, breaks = 100)

#how to create managerial segmentation ----
#simple 2- segment solution based on recency alone 
customers_2015$segment =ifelse(test = customers_2015$recency >365*3, yes = 'inactive', no = 'NA')
View(customers_2015)
table(customers_2015$segment)
aggregate(x= customers_2015[,2:5], by = list(customers_2015$segment), mean)

#A more complex 3 -segment solution based on recency alone 
customers_2015$segment =ifelse(test = customers_2015$recency> 365*3,
                               yes = 'inactive',
                               no =ifelse(test = customers_2015$recency > 365*2,
                                          yes ='cold',
                                          no = 'NA')
                              
                              )
table(customers_2015$segment)
aggregate(x= customers_2015[,2:5], by = list(customers_2015$segment), mean)

#simple 2-segment solution using the which statement
customers_2015$segment ='NA'
customers_2015$segment[which(customers_2015$recency > 365*3)] = 'inactive'
table(customers_2015$segment)
aggregate(x= customers_2015[,2:5], by = list(customers_2015$segment), mean)

#more complex 4 segment solution using which 
customers_2015$segment ='NA'
customers_2015$segment[which(customers_2015$recency > 365*3)] = 'inactive'
customers_2015$segment[which(customers_2015$recency <= 365*3&
                               customers_2015$recency > 365*2)] = 'cold'
customers_2015$segment[which(customers_2015$recency <= 365*2 &
                               customers_2015$recency > 365*1 )] = 'warm'
customers_2015$segment[which(customers_2015$recency <= 365*1 )] = 'active'
table(customers_2015$segment)
aggregate(x= customers_2015[,2:5], by = list(customers_2015$segment), mean)

#complete segment solution using which, and exploiting previous test as input
customers_2015$segment ='NA'
customers_2015$segment[which(customers_2015$recency > 365*3)] = 'inactive'

customers_2015$segment[which(customers_2015$recency <= 365*3&
                               customers_2015$recency > 365*2)] = 'cold'

customers_2015$segment[which(customers_2015$recency <= 365*2 &
                               customers_2015$recency > 365*1 )] = 'warm'

customers_2015$segment[which(customers_2015$recency <= 365*1 )] = 'active'

customers_2015$segment[which(customers_2015$segment == 'warm' &
                               customers_2015$first_purchase <= 365*2 )] = 'new warm'

customers_2015$segment[which(customers_2015$segment == 'warm' &
                               customers_2015$amount < 100 )] = 'warm low value'

customers_2015$segment[which(customers_2015$segment == 'warm' &
                               customers_2015$amount >= 100 )] = 'warm high value'

customers_2015$segment[which(customers_2015$segment == 'active' &
                               customers_2015$first_purchase <= 365 )] = 'new active'

customers_2015$segment[which(customers_2015$segment == 'active' &
                               customers_2015$amount < 100 )] = 'active low value'

customers_2015$segment[which(customers_2015$segment == 'active' &
                               customers_2015$amount > 100 )] = 'active high value'

table(customers_2015$segment)
aggregate(x= customers_2015[,2:5], by = list(customers_2015$segment), mean)

#re-order factor in a way that makes sense 
customers_2015$segment =factor(x= customers_2015$segment, levels = c('inactive','cold',
                                                                     'warm high value', 'warm low value',
                                                                     'new warm', 'active high value',
                                                                     'active low value','new active' ))
table(customers_2015$segment)
aggregate(x= customers_2015[,2:5], by = list(customers_2015$segment), mean)
 
#segmenting a database retrospectively ----

#compute key marketing indicators using SQL language 
library(sqldf)

#compute recency, frequencey, and average purchase amount 
customers_2014 =sqldf("SELECT customer_id,
                      MIN(days_since) -365 AS 'recency',
                      MAX(days_since) -365 AS 'first_purchase',
                      count(*) AS 'frequnecy',
                      AVG(purchase_amount) AS 'amount'
                      FROM data
                      Where days_since >365
                      GROUP BY 1" )

customers_2014$segment ='NA'
customers_2014$segment[which(customers_2014$recency > 365*3)] = 'inactive'

customers_2014$segment[which(customers_2014$recency <= 365*3&
                               customers_2014$recency > 365*2)] = 'cold'

customers_2014$segment[which(customers_2014$recency <= 365*2 &
                               customers_2014$recency > 365*1 )] = 'warm'

customers_2014$segment[which(customers_2014$recency <= 365*1 )] = 'active'

customers_2014$segment[which(customers_2014$segment == 'warm' &
                               customers_2014$first_purchase <= 365*2 )] = 'new warm'

customers_2014$segment[which(customers_2014$segment == 'warm' &
                               customers_2014$amount < 100 )] = 'warm low value'

customers_2014$segment[which(customers_2014$segment == 'warm' &
                               customers_2014$amount >= 100 )] = 'warm high value'

customers_2014$segment[which(customers_2014$segment == 'active' &
                               customers_2014$first_purchase <= 365 )] = 'new active'

customers_2014$segment[which(customers_2014$segment == 'active' &
                               customers_2014$amount < 100 )] = 'active low value'

customers_2014$segment[which(customers_2014$segment == 'active' &
                               customers_2014$amount > 100 )] = 'active high value'
customers_2014$segment =factor(x= customers_2014$segment, levels = c('inactive','cold',
                                                                     'warm high value', 'warm low value',
                                                                     'new warm', 'active high value',
                                                                     'active low value','new active' ))
table(customers_2014$segment)
pie(table(customers_2014$segment),col = rainbow(24))
aggregate(x= customers_2014[,2:5], by = list(customers_2014$segment), mean)

#computing revenue generation per segment ----
revenue_2015 = sqldf("SELECT customer_id,
                     SUM (purchase_amount) AS 'revenue_2015'
                     FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")
summary(revenue_2015)
#merge 2015 customers and 2015 revenue
actual =merge(customers_2015,revenue_2015, all.x = TRUE)
View(actual)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0

#show average revenue per customer and per segement 
aggregate(x= actual$revenue_2015, by = list(customers_2015$segment), mean)

forward =merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] =0
View(forward)
r =aggregate(x= forward$revenue_2015, by = list(customers_2014$segment), mean)
print(r)

#re-order and display result 
r= r[order(r$x, decreasing = TRUE), ]
print(r)
barplot(r$x, names.arg = r$Group.1)

