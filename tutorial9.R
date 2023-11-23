library(dplyr)
library(ggplot2)

ets_dat=read.csv("https://www.dropbox.com/s/urro3ty46kr4f7z/ets_thres_final.csv?dl=1")
names(ets_dat)

ets_dat %>%
  summarise_all(typeof)

# nonfree = from 2013 onwards when permits where only given to certain firms and sectors
# that were deemed at risk from foreign competition

table(ets_dat$nonfree, ets_dat$year)
table(ets_dat$free, ets_dat$year)

ets_dat %>%
  group_by(year) %>%
  summarise(nonfree = sum(nonfree))
  ggplot(aes(x = year, y = nonfree))


# a) Examine this hypothesis by running a regression of lnco2 on the nonfree variable.
# Report what you find.

ets_dat %>%
  with(summary(lm(lnco2 ~ nonfree)))

# firms that received nonfree permits pullot 50% less than firms that
# received free permits

# The variable period is a categorical variable equal to 1 for observations from before 2013 and
# equal to 2 for observations from year 2013 onward. Convert it into a factor variable and
# run a regression of lnco2 on period. Provide an interpretation of the estimated coefficients.

table(ets_dat$period, ets_dat$year)

ets_dat %>%
  with(summary(lm(lnco2 ~ as.factor(period))))

# pollution in 2013 onwards was 16% less than before 2013

# d) Would you say your results in part (a) provide a causal estimate of the effect of not giving free permits?

# Is there a pattern between those who would hae to buy perits?

# Exercise 10.3
halsx=read.csv("https://mondpanther.github.io/datastorieshub/data/hals1prep.csv")

table(halsx$region)

halsx %>%
  with(summary(lm(bmi ~ region)))

halsx %>%
  with(summary(lm(bmi ~ 0 + region)))

mod = lm(bmi ~ 0 + region, data = halsx)

mod$coefficients
max(as.vector(mod$coefficients))

# b)
# The variable ownh_num records responses to the question 
# “Would you say that forsomeone of your age your own health in general is…”
# where users had the following response options:
#   
# • Excellent (1)
# • Good (2)
# • Fair (3)
# • Poor (4)

# The numbers in brackets indicate how these options were recorded in the ownh_num variable.
# Run a regression of ownh_num on bmi and provide a discussion of what you find.
# Is it in line with your expectations on this?

halsx %>%
  with(summary(lm(ownh_num ~ bmi)))

# Exercise 10.4 - USE BROWSER VERSION
# Air pollution has been shown to have a variety of adverse health effects.
# Recently, researchers have also started to investigate other negative effects.
# Below we report regression tables from a study that investigates a link between air pollution and car accidents.
# 
# Can you suggest a causal mechanism that might explain why air pollution could have an effect on car accidents?
