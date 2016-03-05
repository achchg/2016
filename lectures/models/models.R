## ------------------------------------------------------------------------
20/19 * 9/19 - 18/19*10/19

## ------------------------------------------------------------------------
B <- 10^5
theta = -1/19
Y <- sample( c(-1,1), B, replace=TRUE, prob=c(10/19, 9/19))
error <- Y - theta
prop.table(table(error))
sqrt(mean(error^2))
2*sqrt(10/19*9/19)

## ------------------------------------------------------------------------
data("father.son", package="UsingR")
y <- father.son$sheight

## ------------------------------------------------------------------------
set.seed(1)
Y <- sample(y, 25, replace = TRUE)

## ------------------------------------------------------------------------
## N is 
length(Y)
## theta is 
theta <- mean(y)
## the firest error is 
error_1 <- Y[1] - theta

## ------------------------------------------------------------------------
error <- y - mean(y)
hist(error)
##the standard error is:
sigma <- sqrt( mean( (error)^2 ))
qqnorm(error/sigma)
abline(0,1)

## ---- message=FALSE------------------------------------------------------
library(readr)
library(dplyr)

filename <- "https://raw.githubusercontent.com/datasciencelabs/data/master/blue-bead-comp-results.csv"
tab <- read_csv(filename)
names(tab)<-c("timestamp", "name", "estimate","poll_sample_size","ci")
tab <- mutate(tab,estimate=ifelse(estimate<1, estimate*100, estimate)) %>%
  filter(estimate>20)

## ------------------------------------------------------------------------
filter( tab, abs(poll_sample_size-100)<51) %>% nrow

## ------------------------------------------------------------------------
tab <- filter(tab, abs(poll_sample_size-100)<51)

## ------------------------------------------------------------------------
theta <- 0.534
Y <- tab$estimate/100 
error <- Y - theta
hist(error)
se <- sqrt(theta*(1-theta))/sqrt(100)
qqnorm(error/se)
abline(0,1)

## ------------------------------------------------------------------------
sd(Y) ##compared to 
sqrt(theta*(1-theta))/sqrt(100) ##or to
theta_hat <- mean(Y)
sqrt(theta_hat*(1-theta_hat))/sqrt(100) ##or to

## ------------------------------------------------------------------------
theta_hat <- mean(Y)
s <- sd(Y)
## confidence interval:
theta_hat + c(-1,1)*qnorm(0.975)*s/sqrt(length(Y))

## ------------------------------------------------------------------------

## or using the t-distribution
theta_hat + c(-1,1)*qt(0.975, df=length(Y)-1)*s/sqrt(length(Y))

## ------------------------------------------------------------------------
library(XML)
theurl <- paste0("http://www.pollster.com/08USPresGEMvO-2.html")
html_tables <- readHTMLTable(theurl,stringsAsFactors=FALSE)

## ------------------------------------------------------------------------
tab <- html_tables[[1]]

## ------------------------------------------------------------------------
head(tab$Dates)

## ------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

## ------------------------------------------------------------------------
##use separate to split start and end dates
### we convert it to a dplyr table:
tab <- tbl_df(tab)
tab <- tab %>% separate(col=Dates, into=c("start_date","end_date"), sep="-",
                        fill="right") 
select(tab, start_date, end_date)

## if no end_data it means only one day was provided so use that as end as well
tab <- tab %>% mutate(end_date = ifelse(is.na(end_date), start_date, end_date))
select(tab, start_date, end_date)

## no use seprate again to get month, day and year for start_date
tab <- tab %>% separate(start_date, c("smonth", "sday", "syear"), sep = "/", 
                        convert = TRUE, fill = "right") 
select(tab, smonth:syear, end_date)

### if end date has only 1 / then it is missing month, add it
tab <- tab %>% mutate(end_date = ifelse(str_count(end_date, "/") == 1, 
                                   paste(smonth, end_date, sep = "/"), end_date))
select(tab, smonth:syear, end_date)

## now use lubridate function mdy to conver to date
tab <- tab %>% mutate(end_date = mdy(end_date)) 
select(tab, smonth:syear, end_date)

## add 2000 to year since it is currently 7 or 8
tab <- tab %>% mutate(syear = ifelse(is.na(syear), year(end_date), syear + 2000)) 
select(tab, smonth:syear, end_date)

## now use unite to create a m/d/y string
tab <- tab %>% unite(start_date, smonth, sday, syear) 
select(tab, start_date, end_date)

## covert it to date class
tab <- tab %>% mutate(start_date = mdy(start_date))
select(tab, start_date, end_date)

## ------------------------------------------------------------------------
head(tab$`N/Pop`)

## ------------------------------------------------------------------------
tab <- separate(tab, `N/Pop`, into=c("N","population_type"), sep=" ", 
                convert=TRUE, fill="left")

## ------------------------------------------------------------------------
tab <- mutate(tab, Obama = as.numeric(Obama)/100, 
              McCain=as.numeric(McCain)/100,
              diff = Obama - McCain,
              day=as.numeric(start_date - mdy("11/04/2008")),
              week = floor(day/7))

## ------------------------------------------------------------------------
filter(tab, N==800) %>% nrow

## ------------------------------------------------------------------------
Y <- filter(tab, N==800)$diff
theta <- 7.2/100
error <- Y- theta
hist(error)
se <- 2*sqrt(theta*(1-theta)/800)
qqnorm(error/se)
abline(0,1)

## ------------------------------------------------------------------------
error <- tab$diff - 7.2/100
hist(error)

## ------------------------------------------------------------------------
## Assessment 8
library(ggplot2)
theme_set(theme_bw())

tab %>% ggplot( aes(start_date, diff)) + geom_point() +
  geom_hline(aes(yintercept=0))


## ------------------------------------------------------------------------
tab %>% filter(start_date > "2008-01-01") %>%
  ggplot( aes(start_date, diff)) + geom_point() +
  geom_hline(aes(yintercept=0))

## ------------------------------------------------------------------------
group_by(tab, week)

## ------------------------------------------------------------------------
group_by(tab, week) %>% summarize(avg=mean(diff))

## ------------------------------------------------------------------------
group_by(tab, week) %>% summarize(avg=mean(diff)) %>% 
  ggplot(aes(week, avg)) + geom_line() +
  geom_hline(aes(yintercept=0))

## ------------------------------------------------------------------------
group_by(tab, week) %>% summarize(num_polls=n()) %>%
  ggplot(aes(week, num_polls)) + geom_point()

## ------------------------------------------------------------------------
group_by(tab, week) %>% mutate(num_polls=n()) %>% select(Pollster, num_polls) %>% ungroup

## ------------------------------------------------------------------------
group_by(tab, week) %>% mutate(num_polls=n()) %>% select(Pollster, num_polls) %>% filter(num_polls>=10) %>% ungroup %>% nrow

## ------------------------------------------------------------------------
group_by(tab, week) %>% mutate(num_polls=n()) %>% 
  filter(num_polls>=5) %>% 
  summarize(avg=mean(diff)) %>% 
  ggplot(aes(week, avg)) + geom_line() +
  geom_hline(aes(yintercept=0))


## ------------------------------------------------------------------------
group_by(tab, week) %>% mutate(num_polls=n()) %>% 
  filter(num_polls>=5) %>% 
  summarize(avg=mean(diff) , sd=sd(diff)) %>% 
  ggplot(aes(week, avg, ymin=avg-2*sd, ymax=avg+2*sd)) + 
  geom_point() + geom_errorbar() +
  geom_hline(aes(yintercept=0))+ 
  geom_hline(aes(yintercept=0.072))

## ------------------------------------------------------------------------
filter(tab, start_date>"2008-01-01") %>% ggplot(aes(start_date, diff)) + geom_point() + geom_smooth(span=0.25) + geom_hline(aes(yintercept=0)) + 
   geom_hline(aes(yintercept=0.072))

## ------------------------------------------------------------------------
2* sqrt(0.5*0.5) / sqrt(2000)

## ------------------------------------------------------------------------
tab %>% filter( N>2000 & week > -4) %>% summarize(sd(diff))

## ------------------------------------------------------------------------
tab %>% filter(week > -4) %>% group_by(Pollster) %>% filter(n()>4) %>% 
  ggplot(aes(Pollster, diff , col=Pollster)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
## ------------------------------------------------------------------------
tab2 <- filter(tab, start_date > "2008-01-01") %>% group_by(Pollster) %>% filter(n() > 10)
fit <- lm(diff ~ week + Pollster, data=tab2)
summary( aov(fit) )

