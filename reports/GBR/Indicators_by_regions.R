# Indicators for regional comparison

# take only data from 2010

silc.p1h.r <- subset(silc.p1h, rb010>2009)
silc.p2h.r <- subset(silc.p2h, pb010>2009)

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
                              subset(silc.hdr.svy, pb010==2010), svygini, 
                              keep.var = FALSE))
names(gini10)[names(gini10) == 'statistic'] <- 'gini10'

gini11 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, pb010==2011), svygini, 
                              keep.var = FALSE))
names(gini11)[names(gini11) == 'statistic'] <- 'gini11'

gini12 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, pb010==2012), svygini, 
                              keep.var = FALSE))
names(gini12)[names(gini12) == 'statistic'] <- 'gini12'

gini13 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, pb010==2013), svygini, 
                              keep.var = FALSE))
names(gini13)[names(gini13) == 'statistic'] <- 'gini13'

gini14 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, pb010==2014), svygini, 
                              keep.var = FALSE))
names(gini14)[names(gini14) == 'statistic'] <- 'gini14'

gini15 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, pb010==2015), svygini, 
                              keep.var = FALSE))
names(gini15)[names(gini15) == 'statistic'] <- 'gini15'

gini16 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.hdr.svy, pb010==2016), svygini, 
                              keep.var = FALSE))
names(gini16)[names(gini16) == 'statistic'] <- 'gini16'



# Tabellen zusammenfügen
library(plyr)
gini.regions <- join_all(list(gini10, gini11, gini12, gini13, gini14, gini15, gini16),
                  by = 'as.factor(region)')
names(gini.regions)[names(gini.regions) == 'as.factor(region)'] <- 'region'

rm(gini10, gini11, gini12, gini13, gini14, gini15, gini16)


