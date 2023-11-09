install.packages('car')
install.packages('AER')

library(ggplot2)
library(dplyr)
library(haven)
library(car)
library(AER)

# load data
d4=read_dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AABQXwWvdZdvGlT6Y8LIfoMha/dataset4.dta?dl=1")


# Part (a)
# Run an OLS regression of the log quantity on the log price, controlling for ice,
# indicating that the Great Lakes were frozen preventing transport by ship,
# and a set of seasonal dummy variables (to capture seasonality in demand;
# note that dataset has 12 seasonal dummies; i.e. they tread every month as a season).

# lets look at the variables
names(d4)
head(d4)

hist(d4$quantity)
hist(d4$price)

d4 %>%
  ggplot(aes(x = quantity, y = price))+
  geom_point()

d4 %>%
  ggplot(aes(x = quantity, y = price))+
  geom_smooth(method = 'lm')

# linear model
linear_mod = lm(quantity ~ price + ice, data = d4)
summary(linear_mod)

d4 %>%
  with(summary(lm(quantity ~ price + ice)))


# a little trick when you have lots of variables
IVs = c('price', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')

f = as.formula(paste("quantity", paste(IVs, collapse = " + "), sep = " ~ "))
mod = lm(f, data = d4)
summary(mod)

IVs = c('log(price)', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
f = as.formula(paste("log(quantity)", paste(IVs, collapse = " + "), sep = " ~ "))
mod = lm(f, data = d4)
summary(mod)

# What is the estimated price elasticity?
# The elasticity is approximately -.65. As price increases by 1%, we expect quantity
# to decrease by .65%

# Do you think you are estimating a demand curve? Explain.
# Yes we can. Elasticity = % change in price over % change in quantity which
# represents the demand curve.

# Think of what the the economic rationale is of including the variable “ice” in the regression?
# Slower roads and lack of water transportation options


# Part (b) and (c)
# Consider using cartel as an instrument for price in order to identify the demand curve.
# 
# Is the instrument is likely to satisfy the conditions for a valid instrument?

# Cartels restrict supply of certain commodities (e.g. OPEC and oil), creating
# a monopoly and inflating price.
# Hypothesis 1: Cartel -> increases price
# Hypothesis 2: Cartel -> restricts supply
# 
# Hypothesis 1
IVs = c('cartel', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
f = as.formula(paste("quantity", paste(IVs, collapse = " + "), sep = " ~ "))
mod_quant = lm(f, data = d4)
summary(mod_quant)

linearHypothesis(mod_quant,"cartel=0") # strong negative effect

# Hypothesis 2
IVs = c('cartel', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
f = as.formula(paste("price", paste(IVs, collapse = " + "), sep = " ~ "))
mod_price = lm(f, data = d4)
summary(mod_price)

linearHypothesis(mod_price,"cartel=0") # strong positive effect

# Part (d)
# Estimate the demand function by IV. What is your estimated demand elasiticity?

# This question asks you to use the 2 stage least squares approach

IVs_1 = c('log(price)', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
IVs_2 = c('cartel', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')

IVs_all = paste(paste(IVs_1, collapse = " + "), paste(IVs_2, collapse = " + "), sep = " | ")

mod_iv = ivreg(as.formula(paste("log(quantity)", paste(IVs_all), sep = " ~ ")), data = d4)
summary(mod_iv)
# coeff on price = -0.88930
# coeff on price WAS = -0.649479

# Price is more elastic in the IV model than the OLS model.
# Because cartels push up prices, excluding them from the model means that
# prices are more inelastic. Therefore, accounting for cartels in an IV model
# means we have a more accurate estimate of price elasticity which is more elastic 
# than the original estimate

# check. Compare to previous regression
IVs = c('log(price)', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
f = as.formula(paste("log(quantity)", paste(IVs, collapse = " + "), sep = " ~ "))
mod = lm(f, data = d4)
summary(mod)
# coeff on price WAS = -0.649479

