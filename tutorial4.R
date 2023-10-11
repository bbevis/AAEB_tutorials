library(haven)
library(dplyr)
library(ggplot2)
resume_dat <- read_dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AABua74TH54FcmOsAs0ayMY5a/bm.dta?dl=1")
summary(resume_dat)

# Exercise 5.1
# a
table(resume_dat$female,resume_dat$black)
table(resume_dat$female,resume_dat$computerskills)
# 1,1 = female with computer skills

prop.table(table(resume_dat$female,resume_dat$black))

# If you find the above confusing (you don't know which axis refers to female or comp skills),
#use the dplyr method:
resume_dat %>% 
  group_by(black, female, computerskills) %>% 
  summarise(n=n()) %>%
  mutate(share = n / sum(n))

# b
# Do a similar tabulation for education and the number of jobs previous held (ofjobs).
# These variables take on 5 and 7 different values, respectively.
round(prop.table(table(resume_dat$education,resume_dat$black)),3)

round(prop.table(table(resume_dat$ofjobs,resume_dat$black)),3)


# Does education and the number of previous jobs look balanced across race groups?
resume_dat %>%
  with(summary(lm(black ~ ofjobs)))

# c
# Look at the mean and standard deviation for the variable for years of experience (yearsexp) separately for black and whites.
# Does this variable look similar by race?
resume_dat %>%
  group_by(black) %>%
  summarise(m = mean(yearsexp),
            sd = sd(yearsexp))

# Part (e)
# The variable of interest on the data set is the variable call, which indicates a call back for an interview.
# What percentage of people receive a call back (rounded up to 2 decimal places)?

round(prop.table(table(resume_dat$call)) * 100, 2)
# 8.05% receive a call back


round(prop.table(resume_dat$call,resume_dat$black), 2) * 100, 2)
# Note that by specifying 2 in the prop.table function, we report proportions by column
# You can see differences between race... but is it significant?

resume_dat %>%
  with(summary(lm(call ~ black)))

# check with controls
resume_dat %>%
  with(summary(lm(call ~ black + female + yearsexp + ofjobs)))

# Exercise 5.2
# When the DV is a binary variable, you should use a logit regression
resume_dat %>%
  with(summary(glm(call ~ black + female + yearsexp + ofjobs, family = binomial)))
# Note that you cannot compare the coefficients directly because in a logit,
# the coefficient is the log odds (related to probability of the outcome = 1)

# Exercise 5.3
cps_dat <- read.dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AAAOb_v-Y2V0NN4-rxahZjl4a/cps.dta?dl=1")
summary(cps_dat)

# What percentage of respondents has at least some college education (rounded up to the nearest percent)?
unique(cps_dat$education)
cps_dat %>%
  mutate(college = case_when(education == 'come col' ~ 1,
                             education == 'col+' ~ 1))
# but... all non college educated people now have no values

cps_dat %>%
  mutate(college = case_when(education == 'come col' ~ 1,
                             education == 'col+' ~ 1,
                             TRUE ~ 0)) # turns NAs to 0

cps_dat = cps_dat %>%
  mutate(college = case_when(education == 'some col' ~ 1,
                             education == 'col+' ~ 1,
                             TRUE ~ 0))

round(mean(cps_dat$college),2)

# Some functional forms... but lets just use the auto dataset to predict price
# Interaction terms
auto %>%
  with(summary(lm(price ~ mpg + weight + foreign + gear_ratio)))

auto %>%
  with(summary(lm(price ~ mpg + weight * foreign + gear_ratio)))
# and now... we can see that weight is conditional on foreign
# What we really have is price ~ mpg + weight + foreign + weight * foreign + gear_ratio

# log-linear models
auto %>%
  with(summary(lm(log(price) ~ weight + foreign)))

# log-log models
auto %>%
  with(summary(lm(log(price) ~ log(weight) + log(foreign))))
# we CANOT log 0

auto %>%
  with(summary(lm(log(price) ~ log(weight))))
