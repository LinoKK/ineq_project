# Indicators for regional comparison

library(survey)
library(convey)
library(plyr)
library(dplyr)

# take only data from 2010

silc.p1h.r <- subset(silc.p1, year>2009)
silc.p2h.r <- subset(silc.p2h, year>2009)

silc.p1h.r <- silc.p1h.r %>% mutate(region = as.factor(db040))
silc.p2h.r <- silc.p2h.r %>% mutate(region = as.factor(db040))


silc.pdr.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p1h.r) %>% convey_prep()

silc.hdr.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p2h.r) %>% convey_prep()


# Entwicklung des Gini in den Regionen #

# damit man nachher mergen kann als data frame
gini10 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2010), svygini, 
                              keep.var = FALSE))
names(gini10)[names(gini10) == 'statistic'] <- 'gini10'

gini11 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2011), svygini, 
                              keep.var = FALSE))
names(gini11)[names(gini11) == 'statistic'] <- 'gini11'

gini12 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2012), svygini, 
                              keep.var = FALSE))
names(gini12)[names(gini12) == 'statistic'] <- 'gini12'

gini13 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2013), svygini, 
                              keep.var = FALSE))
names(gini13)[names(gini13) == 'statistic'] <- 'gini13'

gini14 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2014), svygini, 
                              keep.var = FALSE))
names(gini14)[names(gini14) == 'statistic'] <- 'gini14'

gini15 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2015), svygini, 
                              keep.var = FALSE))
names(gini15)[names(gini15) == 'statistic'] <- 'gini15'

gini16 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2016), svygini, 
                              keep.var = FALSE))
names(gini16)[names(gini16) == 'statistic'] <- 'gini16'



# Tabellen zusammenfügen
library(plyr)
gini.regions <- join_all(list(gini10, gini11, gini12, gini13, gini14, gini15, gini16),
                  by = 'as.factor(region)')
names(gini.regions)[names(gini.regions) == 'as.factor(region)'] <- 'region'

rm(gini10, gini11, gini12, gini13, gini14, gini15, gini16)

# Survey mean

mean10 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2010), svymean, 
                              keep.var = FALSE))
names(mean10)[names(mean10) == 'statistic'] <- 'mean10'

mean11 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2011), svymean, 
                              keep.var = FALSE))
names(mean11)[names(mean11) == 'statistic'] <- 'mean11'

mean12 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2012), svymean, 
                              keep.var = FALSE))
names(mean12)[names(mean12) == 'statistic'] <- 'mean12'

mean13 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2013), svymean, 
                              keep.var = FALSE))
names(mean13)[names(mean13) == 'statistic'] <- 'mean13'

mean14 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2014), svymean, 
                              keep.var = FALSE))
names(mean14)[names(mean14) == 'statistic'] <- 'mean14'

mean15 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2015), svymean, 
                              keep.var = FALSE))
names(mean15)[names(mean15) == 'statistic'] <- 'mean15'

mean16 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, year==2016), svymean, 
                              keep.var = FALSE))
names(mean16)[names(mean16) == 'statistic'] <- 'mean16'

mean.regions <- join_all(list(mean10, mean11, mean12, mean13, mean14, mean15, mean16),
                         by = 'as.factor(region)')
names(mean.regions)[names(mean.regions) == 'as.factor(region)'] <- 'region'

rm(mean10, mean11, mean12, mean13, mean14, mean15, mean16)


# genauerer split der Regionen

year <- 2010
silc.d <- tbl(pg, "c10d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

region.10 <- left_join(subset(silc.p2h, pb010==2010), silc.d, by = c('id_h' = 'id_h', 'pb010' = 'db010'))

