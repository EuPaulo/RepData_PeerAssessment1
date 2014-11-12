# Reproducible Research: Peer Assessment 1

Loading packages to be used:

```r
library(lubridate)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

As I'm from Brazil, week days labels would change if I didn't set the system locale:

```r
Sys.setlocale("LC_TIME","C") 
```

```
## [1] "C"
```


## Loading and preprocessing the data
Loading data:

```r
dados<-read.csv("activity.csv", h = T)
```

Transforming date to `POSIXct` class:

```r
dados<-transform(dados, date = ymd(date))
```

Wrapping the data frame in a dplyr tbl_df:

```r
dados<-tbl_df(dados)
```


## What is mean total number of steps taken per day?
Grouping data by date (using dplyr package):

```r
tsdados<-summarise(group_by(dados,date), sum(steps, na.rm = T))
names(tsdados)<-c("date", "sumSteps")
```

Mean and median of total number of steps per day:

```r
mean(tsdados$sumSteps)
```

```
## [1] 9354.23
```

```r
median(tsdados$sumSteps)
```

```
## [1] 10395
```

So, the mean of total numer of steps per day is 9354.2295082 and it's median is 10395.

To illustrate all this, here comes a histogram:

```r
hist(tsdados$sumSteps, breaks = 30, xlab = "Total number of steps per day", main = NULL)
```

![](./PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


## What is the average daily activity pattern?

Grouping data by interval:

```r
daDados<-summarise(group_by(dados,interval), mean(steps, na.rm = T))
names(daDados)<-c("interval","meanSteps")
```

Time series plot of 5-minute intervals and average number of steps per day:

```r
plot(daDados$interval, daDados$meanSteps, type = "l", xlab = "5-minute intervals", ylab = "Average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


Interval that contains, on average, the maximum number of steps:


```r
daDados[daDados$meanSteps == max(daDados$meanSteps),]$interval
```

```
## [1] 835
```



So, the interval then interval that contains, on average, the maximum number of steps is ``835``, with an average number of steps = ``206.1698113``.

## Imputing missing values
Calculating total number of missing values in the data set:


```r
sum(is.na(dados$steps))
```

```
## [1] 2304
```

The total number of missing values (``NA``) in this data set is ``2304``.


Function for imputing missing values in the data set.

```r
imputation<-function (dataframe){
    vetor = vector()
    
    for (linha in 1:nrow(dataframe)){
        passos=dataframe[linha,]$steps
            
        if (is.na(passos)){
            intervalo = dataframe[linha,]$interval
            x = daDados[daDados$interval == intervalo,]$meanSteps
            vetor<-append(vetor, x)
        }
        
        else {
            vetor<-append(vetor, passos)
        }
    }
    
    novoDataframe<-mutate(dataframe, steps = vetor)
    novoDataframe    
}
```


New data set, equal to original, with missing data filled in:

```r
dadosComp<-imputation(dados)
```



Grouping data by date (using dplyr package):

```r
tsDadosComp<-summarise(group_by(dadosComp,date), sum(steps))
names(tsDadosComp)<-c("date", "sumSteps")
```


Mean and median of total number of steps per day:

```r
mean(tsDadosComp$sumSteps)
```

```
## [1] 10766.19
```

```r
median(tsDadosComp$sumSteps)
```

```
## [1] 10766.19
```

So, the mean of total numer of steps per day is 1.0766189\times 10^{4} and it's median is 1.0766189\times 10^{4}.

To illustrate all this, here comes a histogram:

```r
hist(tsDadosComp$sumSteps, breaks = 30, xlab = "Total number of steps per day", main = NULL)
```

![](./PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

######Falta resposta




## Are there differences in activity patterns between weekdays and weekends?


Function to detect weekend or weekday

```r
weekend<- function(data){
    dia<-weekdays(data)
    if (dia == "Saturday" | dia == "Sunday"){
        dia = "weekend"
    }
    else {
        dia = "weekday"
    }
    dia
}
```

Creating a new factor variable in the data frame:

```r
dadosWeek<-mutate(dadosComp, week = factor(do.call(rbind, lapply(dadosComp$date, weekend))))
```

Grouping data by time interval and weekday factor ("week"), and extracting mean:

```r
tsDadosWeek<-summarise(group_by(dadosWeek, week, interval), mean(steps))
names(tsDadosWeek)<-c("week", "interval", "steps")
```

Plotting:

```r
xyplot(steps ~ interval | week, data = tsDadosWeek, layout = c(1,2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-21-1.png) 


