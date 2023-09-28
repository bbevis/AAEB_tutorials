# Lets use the auto data

# Read data in from url
auto <- read.csv("https://www.dropbox.com/s/gm22o5efboc3q0w/auto.csv?dl=1", header=TRUE) %>%
  select(-X)

# simple histograms
auto %>%
  ggplot(aes(x = mpg)) +
  geom_histogram()

auto %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(binwidth = 10)


auto %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(binwidth = 2, color = 'blue', fill = 'lightblue') +
  theme_bw() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=18,face="bold"),
        legend.position = 'None',
        panel.grid = element_blank()) +
  labs(x="MPG",y="Count")


# Shall we do a line chart?
auto %>%
  ggplot(aes(x = mpg, y = price)) +
  geom_line()

# why does that look so weird?
auto %>%
  ggplot(aes(x = mpg, y = price)) +
  geom_point()

# lets try that line plot again... but smoother
auto %>%
  ggplot(aes(x = mpg, y = price))  +
  geom_smooth() +
  geom_point(color = "#C77CFF") +
  theme_bw()

# What if I just want a lie of best fit? e.g. a regression line?
auto %>%
  ggplot(aes(x = mpg, y = price))  +
  geom_smooth(method = 'lm') +
  theme_bw()

# Bar/Column charts
# get data
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(deathsOcases=deaths/cases * 100)

# basic chart
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  ggplot(aes(x = state, y = deathsOcases)) +
  geom_bar(stat = 'identity')

# lets add some colour
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  ggplot(aes(x = state, y = deathsOcases,)) +
  geom_bar(stat = 'identity', aes(fill = state)) +
  theme_bw() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#C77CFF", '#FF0066'))

# add the usual formatting
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  ggplot(aes(x = state, y = deathsOcases,)) +
  geom_bar(stat = 'identity', aes(fill = state)) +
  theme_bw() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#C77CFF", '#FF0066')) +
  labs(x="State",y="Deaths per Cases") +
  theme(axis.text=element_text(size=30),
        panel.grid = element_blank(),
        axis.title=element_text(size=30,face="bold"),
        legend.position = 'none')

# we could de-clutter further by removing the axis
covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  group_by(state) %>%
  summarise(deaths=sum(deaths),cases=sum(cases)) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  ggplot(aes(x = state, y = deathsOcases,)) +
  geom_bar(stat = 'identity', aes(fill = state)) +
  theme_void() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D", "#C77CFF", '#FF0066')) +
  labs(x="State",y="") +
  theme(axis.text=element_text(size=30),
        panel.grid = element_blank(),
        axis.title=element_text(size=30,face="bold"),
        legend.position = 'none',
        axis.text.y = element_blank()) +
  geom_text(aes(label=round(deathsOcases, 1)), position=position_dodge(width=0.9), vjust=-0.25, size = 10)

# One last thing... we can flip the axes
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

# What I normally use...

covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  group_by(state) %>%
  summarize(m=mean(deathsOcases,na.rm=T),
            se=sd(deathsOcases,na.rm=T)/sqrt(n())) +
  ggplot(aes(x=condition,y=m,
             ymin=m-se,ymax=m+se, color = condition))


covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  group_by(state) %>%
  summarize(m=mean(deathsOcases,na.rm=T),
            se=sd(deathsOcases,na.rm=T)/sqrt(n())) %>%
  ggplot(aes(x=state,y=m,
             ymin=m-se,ymax=m+se, color = state)) +
  geom_point(size = 5) +
  theme_bw()


covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  group_by(state) %>%
  summarize(m=mean(deathsOcases,na.rm=T),
            se=sd(deathsOcases,na.rm=T)/sqrt(n())) %>%
  ggplot(aes(x=state,y=m,
             ymin=m-se,ymax=m+se, color = state)) +
  geom_point(size = 5) +
  geom_errorbar(width=.2) + 
  geom_line() +
  theme_bw() +
  coord_flip() +
  theme(axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=20,face="bold"),
        legend.position = 'None',
        panel.grid.major = element_blank()) +
  labs(x="",y="Death per Cases")


# if we were to run a regression of States on death per cases, what would we expect?

covid %>%
  filter(state%in%c('Alabama', 'New York', 'Texas', 'California')) %>%
  mutate(deathsOcases=deaths/cases * 100) %>%
  with(summary(lm(deathsOcases ~ state)))

       