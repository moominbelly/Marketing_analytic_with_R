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
```
## customer_id purchase_amount data_of_purchase year_of_purchase days_since
## 1         760              25       2009-11-06             2009  2247.2083
## 2         860              50       2012-09-28             2012  1190.2083
## 3        1200             100       2005-10-25             2005  3720.2083
## 4        1420              50       2009-07-09             2009  2367.2083
## 5        1940              70       2013-01-25             2013  1071.2083
## 6        1960              40       2013-10-29             2013   794.2083
```
```
summary(data)
```
```
 customer_id     purchase_amount   data_of_purchase     year_of_purchase   days_since      
 Min.   :    10   Min.   :   5.00   Min.   :2005-01-02   Min.   :2005     Min.   :   1.208  
 1st Qu.: 57720   1st Qu.:  25.00   1st Qu.:2009-01-17   1st Qu.:2009     1st Qu.: 733.208  
 Median :102440   Median :  30.00   Median :2011-11-23   Median :2011     Median :1500.208  
 Mean   :108935   Mean   :  62.34   Mean   :2011-07-14   Mean   :2011     Mean   :1632.148  
 3rd Qu.:160525   3rd Qu.:  60.00   3rd Qu.:2013-12-29   3rd Qu.:2013     3rd Qu.:2540.208  
 Max.   :264200   Max.   :4500.00   Max.   :2015-12-31   Max.   :2015     Max.   :4016.208 
```
****
### Compute key marketing indicatier using SQL language
```
library(sqldf)
```
```
customers_2015 = sqldf("SELECT customer_id,
                       MIN(days_since) AS 'recency',
                      MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data 
                       GROUP BY 1")
```
```
head(customers_2015)
```
```
 customer_id   recency first_purchase frequency    amount       
1          10 3829.2083       3829.208         1  30.00000        
2          80  343.2083       3751.208         7  71.42857 
3          90  758.2083       3783.208        10 115.80000        
4         120 1401.2083       1401.208         1  20.00000    
5         130 2970.2083       3710.208         2  50.00000         
6         160 2963.2083       3577.208         2  30.00000         
```

```
summary(customers_2015)
```
```
  customer_id        recency         first_purchase       frequency          amount                        
 Min.   :    10   Min.   :   1.208   Min.   :   1.208   Min.   : 1.000   Min.   :   5.00    
 1st Qu.: 81990   1st Qu.: 244.208   1st Qu.: 988.208   1st Qu.: 1.000   1st Qu.:  21.67    
 Median :136430   Median :1070.208   Median :2087.208   Median : 2.000   Median :  30.00  
 Mean   :137574   Mean   :1253.246   Mean   :1984.218   Mean   : 2.782   Mean   :  57.79  
 3rd Qu.:195100   3rd Qu.:2130.208   3rd Qu.:2992.208   3rd Qu.: 3.000   3rd Qu.:  50.00   
 Max.   :264200   Max.   :4014.208   Max.   :4016.208   Max.   :45.000   Max.   :4500.00  
                                                                                          
 ```
 #### The higer Recency = less frequently purchase, Smaller recency = recent the last purchase

```
hist(customers_2015$recency)
````
```
![image](https://user-images.githubusercontent.com/77463110/131763588-02582baf-b537-454b-a41a-9c5c8e85a29d.png)


```
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

