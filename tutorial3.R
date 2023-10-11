##################################
# Recap on ggplot
##################################

# install.packages('ggplot')
library(ggplot2)
library(dplyr)

auto %>%
  ggplot(aes(x = mpg, y = price)) +
  geom_point()

auto %>%
  ggplot(aes(x = mpg, y = price))  +
  geom_smooth() +
  geom_point(color = "#C77CFF") +
  theme_bw()

# A bit more sophisticated
# split into 2 parts. 1st, manipulate the data in the correct format
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(deathsOcases=deaths/cases * 100)

# then, add ggplot and overlay customised formatting
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  ggplot(aes(x = state, y = deathsOcases,)) +
  geom_bar(stat = 'identity', aes(fill = state)) +
  theme_void() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#C77CFF", '#FF0066')) +
  labs(x="",y="") +
  theme(axis.text=element_text(size=30),
        panel.grid = element_blank(),
        axis.title=element_text(size=30,face="bold"),
        legend.position = 'none',
        axis.text.x = element_blank()) +
  geom_text(aes(label=round(deathsOcases, 1)), position=position_dodge(width=0.9), hjust=-0.25, size = 10) +
  coord_flip()

##################################
# Recap on ggplot
##################################

# Exercise 4.1
thresh_0.05 = 1.96
thresh_0.01 = 2.56
# t-value = beta / standard error

beta = -0.12
se = 0.07
abs(beta/se)

abs(beta/se) > thresh_0.05
abs(beta/se) > thresh_0.01

# Exercise 4.2
library(foreign)
wage_dat <- read.dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AADZ4MZhDDk9R8sFSjBvmcRma/WAGE1.DTA?dl=1")

mod = lm(wage ~ educ, data = wage_dat)
summary(mod)

# alternatively...
wage_dat %>% with(summary(lm(wage ~ educ)))

wage_dat %>%
  with(summary(lm(tenure ~ educ)))

# We cannot reject the null hypothesis

# Exercise 4.3
library(foreign)
auto_dat <- read.dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AACNkMy47ilXAMh3nmiIs_Bqa/auto.dta?dl=1")

auto_dat %>%
  ggplot(aes(y=price, x = weight)) +
  geom_point()

mod = lm(price ~ weight, data = auto_dat)
summary(mod)

# What is the predicted price of a car weighing 2,500lb?
mod$coefficients
mod$coefficients[1]
mod$coefficients[[1]]

mod$coefficients[[1]] + mod$coefficients[[2]] * 2500

# alternatively
value_to_forecast <- data.frame(weight=c(2500,4000))
forecast <- predict(mod,value_to_forecast)

# 1kg = 2.2046 lbs
diff = 2.2046 * 500
mod$coefficients[[2]]*diff

# Exercise 4.4
obs <- 100
eps <- rnorm(obs)
x <- runif(obs)*eps

y <- 2+0.5*x+eps
mod<-lm(y~x)
summary(mod)

hist(x)

# lets repeat this a few times to get the average
beta <- double(1000)

for(i in 1:1000){
  print(i)
  
  df = data.frame(eps = rnorm(obs)) %>%
    mutate(X = runif(obs)) %>%
    mutate(Y = 2 + 0.5*X + eps)
  
  beta[i] <- lm(Y ~ X, df)$coefficients[[2]]
}

mean(beta)
hist(beta)
