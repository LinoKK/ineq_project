# Indicators for regional comparison

library(survey)
library(convey)
library(plyr)
library(dplyr)


# take only data from 2010

silc.p1h.r <- subset(silc.p1, year>2009)
silc.p2h.r <- subset(silc.p2h, year>2009)

silc.p1h.r <- silc.p1h.r %>% mutate(Region = as.factor(db040))
silc.p2h.r <- silc.p2h.r %>% mutate(Region = as.factor(db040))

#rename regions
silc.p1h.r[silc.p1h.r == "UKH"] <- "East"
silc.p1h.r[silc.p1h.r == "UKF"] <- "East_Midlands"
silc.p1h.r[silc.p1h.r == "UKI"] <- "London"
silc.p1h.r[silc.p1h.r == "UKC"] <- "North_East"
silc.p1h.r[silc.p1h.r == "UKD"] <- "North_West"
silc.p1h.r[silc.p1h.r == "UKN"] <- "Northern_Ireland"
silc.p1h.r[silc.p1h.r == "UKM"] <- "Scotland"
silc.p1h.r[silc.p1h.r == "UKJ"] <- "South_East"
silc.p1h.r[silc.p1h.r == "UKK"] <- "South_West"
silc.p1h.r[silc.p1h.r == "UKL"] <- "Wales"
silc.p1h.r[silc.p1h.r == "UKG"] <- "West_Midlands"
silc.p1h.r[silc.p1h.r == "UKE"] <- "Yorkshire"

silc.p1h.r <- silc.p1h.r %>% mutate(region = as.factor(db040))
silc.p2h.r <- silc.p2h.r %>% mutate(region = as.factor(db040))

silc.pdr.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p1h.r) %>% convey_prep()

silc.hdr.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p1) %>% convey_prep()


# Entwicklung des Gini in den Regionen #

# damit man nachher mergen kann als data frame
gini10 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2010, disp.inc > 0), svygini, 
                              keep.var = FALSE))
names(gini10)[names(gini10) == 'statistic'] <- 'gini10'

gini11 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2011, disp.inc > 0), svygini, 
                              keep.var = FALSE))
names(gini11)[names(gini11) == 'statistic'] <- 'gini11'

gini12 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2012, disp.inc > 0), svygini, 
                              keep.var = FALSE))
names(gini12)[names(gini12) == 'statistic'] <- 'gini12'

gini13 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2013, disp.inc > 0), svygini, 
                              keep.var = FALSE))
names(gini13)[names(gini13) == 'statistic'] <- 'gini13'

gini14 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2014, disp.inc > 0), svygini, 
                              keep.var = FALSE))
names(gini14)[names(gini14) == 'statistic'] <- 'gini14'

gini15 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2015, disp.inc > 0), svygini, 
                              keep.var = FALSE))
names(gini15)[names(gini15) == 'statistic'] <- 'gini15'

gini16 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2016, disp.inc > 0), svygini, 
                              keep.var = FALSE))
names(gini16)[names(gini16) == 'statistic'] <- 'gini16'



# Tabellen zusammenfügen
library(plyr)
gini.regions <- join_all(list(gini10, gini11, gini12, gini13, gini14, gini15, gini16),
                  by = 'as.factor(region)')
names(gini.regions)[names(gini.regions) == 'as.factor(region)'] <- 'Region'

rm(gini10, gini11, gini12, gini13, gini14, gini15, gini16)

gini.regions <- gini.regions %>% mutate(Percent.Change = (gini16/gini10 - 1) * 100)

gini.regions <- gini.regions %>% mutate_if(is.numeric, ~round(., 2))

# als excel speichern um eine Tabelle zu haben
write.csv(gini.regions, file = "reports/GBR/tables/gini.regions.csv",row.names=FALSE)



# Survey mean

mean10 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2010, disp.inc > 0), svymean, 
                              keep.var = FALSE))
names(mean10)[names(mean10) == 'statistic'] <- 'mean10'

mean11 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2011, disp.inc > 0), svymean, 
                              keep.var = FALSE))
names(mean11)[names(mean11) == 'statistic'] <- 'mean11'

mean12 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2012, disp.inc > 0), svymean, 
                              keep.var = FALSE))
names(mean12)[names(mean12) == 'statistic'] <- 'mean12'

mean13 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2013, disp.inc > 0), svymean, 
                              keep.var = FALSE))
names(mean13)[names(mean13) == 'statistic'] <- 'mean13'

mean14 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2014, disp.inc > 0), svymean, 
                              keep.var = FALSE))
names(mean14)[names(mean14) == 'statistic'] <- 'mean14'

mean15 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2015, disp.inc > 0), svymean, 
                              keep.var = FALSE))
names(mean15)[names(mean15) == 'statistic'] <- 'mean15'

mean16 <- as.data.frame(svyby(~disp.inc, ~as.factor(region), 
                              subset(silc.pdr.svy, year==2016, disp.inc > 0), svymean, 
                              keep.var = FALSE))
names(mean16)[names(mean16) == 'statistic'] <- 'mean16'

mean.regions <- join_all(list(mean10, mean11, mean12, mean13, mean14, mean15, mean16),
                         by = 'as.factor(region)')
names(mean.regions)[names(mean.regions) == 'as.factor(region)'] <- 'Region'

rm(mean10, mean11, mean12, mean13, mean14, mean15, mean16)

#Veränderung über die Jahre
mean.regions <- mean.regions %>% mutate(Delta = mean16 - mean10 )

mean.regions <- mean.regions %>% mutate_if(is.numeric, ~round(., 0))

write.csv(mean.regions, file = "reports/GBR/tables/mean.regions.csv",row.names=FALSE)


# genauerer split der Regionen

year <- 2010
silc.d <- tbl(pg, "c10d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

region.10 <- left_join(subset(silc.p2h, year==2010), silc.d, by = c('id_h' = 'id_h', 'year' = 'db010'))

library(readxl)
voting_detail <- read_excel("reports/GBR/voting_detail.xlsx")
voting_detail <- voting_detail %>% mutate(Leave = as.numeric(Leave))
voting_detail <- voting_detail %>% mutate(Remain = as.numeric(Remain))
voting_detail <- voting_detail %>% mutate_if(is.numeric, ~round(., 4))

names(region.10)[names(region.10) == 'db040.y'] <- 'Region'

region.10 <- region.10 %>% mutate(db090 = db090.x)

region.10.svy <- svydesign(ids = ~id_h,
                          strata = ~db020.x,
                          weights = ~db090,
                          data = region.10) %>% convey_prep()

# mean

mean.nuts2 <- as.data.frame(svyby(~disp.inc, ~as.factor(Region), 
                              subset(region.10.svy, disp.inc>0), svymean, 
                              keep.var = FALSE))
names(mean.nuts2)[names(mean.nuts2) == 'statistic'] <- 'mean'

mean.nuts2 <- mean.nuts2 %>% mutate_if(is.numeric, ~round(., 0))

# gini

gini.nuts2 <- as.data.frame(svyby(~disp.inc, ~as.factor(Region), 
                                  subset(region.10.svy, disp.inc>0), svygini, 
                              keep.var = FALSE))
names(gini.nuts2)[names(gini.nuts2) == 'statistic'] <- 'gini'

gini.nuts2 <- gini.nuts2 %>% mutate_if(is.numeric, ~round(., 4))

# median

med.nuts2 <- as.data.frame(svyby(~disp.inc, ~as.factor(Region), subset(region.10.svy, disp.inc > 0 ),
                     svyquantile, ~disp.inc, quantiles = c(0.5), keep.var = FALSE))

names(med.nuts2)[names(med.nuts2) == 'statistic'] <- 'median'

med.nuts2 <- med.nuts2 %>% mutate_if(is.numeric, ~round(., 0))

nuts2 <- join_all(list(mean.nuts2, med.nuts2, gini.nuts2),
                         by = 'as.factor(Region)')

names(nuts2)[names(nuts2) == "as.factor(region)"] <- 'Region'

rm(mean.nuts2, med.nuts2, gini.nuts2)

nuts2 <- left_join(nuts2, voting_detail, by = c("as.factor(Region)" = "Region"))


# for regression analysis:

library(readxl)
voting_by_region <- read_excel("reports/GBR/voting_by_region.xlsx")

gini.regions <- left_join(gini.regions, voting_by_region, by = c( "Region" = "Region"))

gini.reg <- lm( Leave ~ Percent.Change , gini.regions)
summary(gini.reg)

mean.regions <- left_join(mean.regions, voting_by_region, by = c( "Region" = "Region"))

mean.reg <- lm(Remain.Share ~ Mean, mean.regions)
summary(mean.reg)

# NUTS 2 Regressions
#subset is since there are no rsults for one region

nuts2 <- nuts2 %>% 
  mutate(gini.reg = gini*100,
         log.leave = log (Leave),
         mean.reg = mean*1000,
         median.reg = median*1000)



nuts2.table <- select(nuts2, "Region Name", "gini", "mean", "median", "Remain", "Leave")

write.csv(nuts2.table, file = "reports/GBR/tables/nuts2.csv",row.names=FALSE)
write.csv(nuts2, file = "reports/GBR/tables/nuts.csv",row.names=FALSE)

gini.reg.nuts <- lm(Leave ~ gini.scale , subset(nuts2, Leave > 0))
summary(gini.reg.nuts)

# says increase in Gini leads to a decrease in Leave vote which would mean more
# unequal regions tend to vote more for a remain. since we increased the scale of gini by 100, the
# coefficient is also scaled upwards which means that the result is not really economically significant

med.reg.nuts <- lm(Leave ~ median, subset(nuts2, Leave > 0))
summary(med.reg.nuts)

# result is not statistically significant but points in the expected direction with a negative sign

mean.reg.nuts <- lm(Leave ~ mean, subset(nuts2, Leave > 0))
summary(mean.reg.nuts)

# result is significant at the 5% level. It means that an increase in mean disposable income for 1000
# leads to a decrease of the Leave votes of 0.0044, so almost half a percentage point


