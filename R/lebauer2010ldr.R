library(dplyr)
library(ggplot2)
library(udunits2)
library(tidyr)
install.packages(c('ggplot2', 'udunits2', 'tidyr'))
dat <- read.csv("dat.all.csv")
hist(dat$obs.c.tot, 
     breaks = 1000, xlim = c(-30,100))
dat %>% gather(key, )

d <-  dat %>%
  select(id, substrate = substrate.x, spdiversity, time = Dt, g_litter, obs.c.tot, matches("t[123456789]"), matches("dc\\.")) %>%
  mutate(c = g_litter - ud.convert(obs.c.tot, "ug", "g")) %>%
  filter(time == 0 | obs.c.tot > 40) 

d %>% gather(key, -id, -substrate, -spdiversity, -time, -g_litter )
  


ggplot(data = d, aes(x = time, color = spdiversity)) + geom_point(aes(y=c)) + facet_wrap(~substrate)

ggplot(data = d, aes(x = time)) + geom_line(aes(y=c, group = id)) + facet_wrap(~substrate)


ggplot(data = d, aes(x = time)) + geom_smooth(aes(y=obs.c.tot, group = spdiversity)) + facet_wrap(~substrate)

str(d)
library(data.table)
d <- data.table(d)

## this is about the 
d[,sum(ud.convert(obs.c.tot, "ug", "mg")/g_litter)/length(obs.c.tot), by = 'spdiversity,substrate']


d[,hist(obs.c.tot)]
mutate(d, c = )