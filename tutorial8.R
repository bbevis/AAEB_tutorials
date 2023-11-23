library(ggplot2)
library(tidyverse)
library(lubridate)

# Exercise 9.1
# 
# The oj.csv dataset contains monthly data on the log price of (frozen) orange juice concentrate (for the US) as the variable lnp.
# A large part of the orange production in the US originates from Florida.
# Hence, the weather in Florida is potentially an important factor in the orange price.
# Frost is rare in Florida. But when it happens it is particularly detrimental for the orange harvest.
# The fdd contains the number of freezing degree days in a 


oj_dat = read.csv("https://github.com/mondpanther/datastorieshub/raw/master/data/oj.csv")  

oj_dat = oj_dat %>%
  mutate(date=as.Date(date))

print(oj_dat$date)

# using lubridate
oj_dat = oj_dat %>%
  mutate(date=as_date(date))

print(oj_dat$date)


# Creating a string vector 
dates <- c("27 / 02 / 92")

# Conversion into date format  
result<-as.Date(dates, "%d / %m / %y")
print(result) 

# The wrong date format wouldn't work. For example...
result<-as.Date(dates, "%d/ %m/ %y")  
print(result)

# working with time
# raw character times
time1 <- "13:45" 
time2 <- "15:20"

# Times converted to a datetime class
time1_clean <- strptime(time1, format = "%H:%M")
time2_clean <- strptime(time2, format = "%H:%M")

# Difference is of class "difftime" by default, here converted to numeric hours 
as.numeric(time2_clean - time1_clean)   # difference in hours

# For more on date formats, check ot: https://www.r-bloggers.com/2013/08/date-formats-in-r/

# Back to the exercises
oj_dat %>%
  ggplot(aes(x=date,y=lnp))+
  geom_line(color="#FF0066")+
  theme_bw()+
  labs(x="Monthly data",y="log(Price)")

# a) Run a regression of the (log) orange juice price on freezing degree days in
# the previous month and interpret the regression.

oj_dat %>%
  mutate(fdd_m1 = lag(fdd)) %>%
  with(summary(lm(lnp ~ fdd_m1)))

# interpretations - as the number of frozen days in a month increases, price increases
# by .2%. Not signficant. Given that this is monthly data, would this make sense?

# b) Can you suggest reasons why the result in (a) might not be an unbiased estimate of the effect of freezing on orange juice prices?
oj_dat %>%
  select(date, lnp, fdd) %>%
  pivot_longer(!date, names_to = "metric", values_to = "val")

# Now we can call gpplot directly after this
oj_dat %>%
  select(date, lnp, fdd) %>%
  pivot_longer(!date, names_to = "metric", values_to = "val") %>%
  ggplot(aes(x = date, y = val, color = metric)) +
  geom_line() +
  facet_wrap(~ metric, scales = "free") +
  theme_bw() +
  labs(x="Monthly data",y="") +
  theme(text=element_text(size=21),
        axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"))

# c) Can you check if the price series has a unit root?
 # You are not going to need to know this!!
install.packages('urca')
library(urca)
summary(ur.df(oj_dat$lnp))

# check the test statistics

# d) Can you suggest an alternative (unbiased) approach to estimating the effect of freezing on price?
# In other words, what else might we need to control for and why?

oj_dat %>%
  mutate(lnp_diff = lag(lnp) - lnp) %>%
  with(summary(lm(lnp_diff ~ fdd)))

oj_dat %>%
  mutate(lnp_diff = lag(lnp) - lnp,
         fdd_m1 = lag(fdd)) %>%
  with(summary(lm(lnp_diff ~ fdd_m1 )))

oj_dat %>%
  mutate(lnp_diff = lag(lnp) - lnp,
         fdd_m1 = lag(fdd),
         trend = 1:n()) %>%
  with(summary(lm(lnp_diff ~ fdd + trend)))

