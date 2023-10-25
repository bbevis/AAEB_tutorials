library(dplyr)
library(ggplot2)
library(haven)
install.packages('car')
library(car)

brexit_dat=read_dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AAC_4UZXJG9kmImypJXTZ9IOa/brexit.dta?dl=1")
colnames(brexit_dat)

# Exercise 6.1
###################################
# Part A
###################################
# Consider the variable b_migr11. It records the share (in %) of foreign born residents
# in an area (according to the last census, which was in 2011). 
# There is no shortage of politicians claiming that the vote for Brexit was due to immigration particularly after 2005
# when Eastern European countries joined the EU and their residents could freely move to countries like Britain.
# Hence, we would expect there to be a strong effect from the presence of foreigners in an area to the vote outcome.
# Explore this using the pct_leave and b_migr11 variables using graphical and regression analysis.

# Which way is the line of best fit sloping on your scatter plot?
brexit_dat %>%
  ggplot(aes(x = b_migr11, y = pct_leave)) +
  geom_point() + # specify scatter plot
  geom_smooth(method = 'lm') + # use this to add line of best fit
  theme_bw() + # a slightly prettier theme
  labs(x="% share of foreign born residents",y="% share of leave votes") +
  theme(axis.text=element_text(size=24), # always make axis labels more legible
        panel.grid = element_blank(),
        axis.title=element_text(size=24,face="bold"))

# What is the constant (rounded to 3 decimal places)?
brexit_dat %>%
  with(summary(lm(pct_leave~b_migr11)))

# Ans = 58.553
# alternatively...
mod = lm(pct_leave ~ b_migr11, data = brexit_dat)
summary(mod)
round(mod$coefficients[[1]],3)

# What is the slope coefficient (rounded to 3 decimal places)?
round(mod$coefficients[[2]],3)
# Ans = -0.504

###################################
# Part B
###################################
# Various commentators have suggested that it might not be so much the level of immigrants as such,
# but the experience of a change due to more foreigners in an area that was driving the vote.
# The variable b_migr contains the share of immigrants in 1991.

# Construct a new variable recording the change in the share of immigrants between 2011 and 1992.
# Explore its impact by extending the regression model from part a).

brexit_dat['migr11_92'] = brexit_dat$b_migr11-brexit_dat$b_migr

brexit_dat = brexit_dat %>% # I always try to use dplyr
  mutate(migr11_92 = b_migr11 - b_migr)

# What is the coefficient on this new variable (rounded to 3 decimal places)?
mod = lm(pct_leave ~ b_migr + migr11_92, data = brexit_dat)
summary(mod)
round(mod$coefficients[[2]],3)
# Ans = -1.099

# Is it statistically significant?
# Check t-values > 1.96 and 2.56. P-values very low.

###################################
# Part C
###################################
# Work out the change in an area’s leave percentage if the 2011 migrant share
# would move to back to its level in 1991 in every area.

# According to your model from part (b), what would have happened to the vote
# if there would not have been any change in the share of migrants between 1991 and 2011?

# Ans:
# from B, we know that the increase in migrant population decreases vote leave share
# by -0.173. Therefore, NO increase in migrant population means that vote leave
# share would have been 0.173 percentage points higher

# Support for Brexit would INCREASE!
# There would be no flip FROM a majority. We would just get a larger majority

###################################
# Part D
###################################
# Can you think of any reason why the estimates in (b) might not adequately reflect the causal impact of
# immigration on the vote? What are plausible confounding forces?
#
# Immigration is confounded with other factors.
# 1) Incrased Polarisation facilitated by social media, taken advantage by politicians
# 2) Closer European integration especially with Eastern European countries joining the EU.

###################################
# Part E
###################################
# The dataset contains a large number of additional characteristics about a local area.
# Which variable would you add to your model from part b) to test the alternative explanation mentioned in d)?
colnames(brexit_dat)

brexit_dat %>%
  mutate(urate2011_2004 =  urate2011 -  urate2004,
         popgrowth11_91 = pop11 - pop91) %>%
  with(summary(lm(pct_leave ~ migr11_92 + b_migr11 + urate2011_2004 + urate2004 + popgrowth11_91 + pop11)))

# Exercise 6.2
prod=read_dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AACD9OHn_yCnKFAX7hbEASVha/prod.dta?dl=1")
colnames(prod)

###################################
# Part A
###################################
# Examine the data using a Cobb-Douglas production function in terms of value added;
# i.e. regress log value added on log capital and log labour
# (va contains the value added, k the capital stock and l labour all not in logs).
# Run the regression with and without time dummies and comment on any differences.

# Cobb-Douglas function: VA = A*L^alpha*K^beta
# VA = Value Added
# L = Labour
# K = Capital stock
# 
# On the basis of the regression with time dummies examine the hypothesis that the production function
# has constant returns to scale (i.e. the labour and capital coefficients would add to 1).
# 
# this is a multiplicative model. To convert to a linear model, we can take logs
# log(VA) = alpha*log(L) + beta*log(K)

mod = lm(log(va) ~ log(l) + log(k), data = prod)
summary(mod)

# create time dummies
prod$year
mod1 = lm(log(va) ~ log(l) + log(k) + year, data = prod)
summary(mod1)

# if you turn year into a categorical variable, you can see the effect of each year
# on value added
mod2 = lm(log(va) ~ log(l) + log(k) + factor(year), data = prod)
summary(mod2)

# Testing for constant returns. # Constant returns to scale occur when the
# long-run average between a company's inputs and outputs are proportional to each other.
linearHypothesis(mod1,"log(k)+log(l)=1") # remember, mod1 contains time

###################################
# Part B
###################################
# The variable sic3dig contains an industry classifier which groups the firms into 17 industries.
# 
# Why might it be useful to include industry classifiers in order to estimate the production function better?
# To reduce bias

prod$sic3dig
# we see that industries are numeric values. We need to turn these into characters
# or categories

mod=lm(log(va)~log(k)+log(l)+year+factor(sic3dig), prod)
summary(mod)

linearHypothesis(mod, "log(k)+log(l)=1")
# No constant returns to scale

###################################
# Part C
###################################
# Which of the 17 industries has the largest number of observations?

prod %>%
  group_by(sic3dig) %>%
  summarize(n = n()) %>%
  arrange(-n) %>% # sort by descending order
  slice(1) # get top row

# Lets pick industries 311 and 321.
# For each of the two industries separately, estimate a Cobb-Douglas production function.

prod %>%
  filter(sic3dig == 311) %>%
  with(summary(lm(log(va)~log(k)+log(l)+year)))

prod %>%
  filter(sic3dig == 321) %>%
  with(summary(lm(log(va)~log(k)+log(l)+year)))

# Would you say the functions are very different in the two industries?
# coeff for capital much larger for 311

