###############################################################################
#### Indicators base script ####


library(survey)
library(convey)
library(plyr)
library(dplyr)


# Subsetting for households --------------------------------------------------------------

#David: Hinzugefuegt. Vorsicht, ueberschreibt bisheriges silc.p2 / silc.p1. Koennte man vll. anders loesen.

#As per the rule, only households with income > 0 should be included

#silc.p2 <- silc.p2 %>% filter(hy010 > 0)

### Alle Einkommen, auch negative drin lassen
# silc.p1 <- silc.p1 %>% filter(disp.inc > 0)

# Creating Survey Objects -------------------------------------------------
## müsste man dann noch auf die oben kreierten silc.pd.inc umschreiben

# 

# personal cross section weighting
silc.pd.svy <- svydesign(ids =  ~ id_p,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.p1) %>% convey_prep()

# household cross section weighting
silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p1) %>% convey_prep()

# Indicators --------------------------------------------------------------

# Basisset der Indikatoren: Median, Mean, Gini, 80/20, Top 10 von fac.inc, 
# nat.inc & disp.inc für P1 & P2 über den Zeitraum 2005-2016


# P1

# Mean Income für alle Jahre 

mean.fac.tot <- svyby(~fac.inc, ~year, silc.hd.svy, svymean, keep.var = FALSE)
mean.nat.tot <- svyby(~nat.inc, ~year, silc.hd.svy, svymean, keep.var = FALSE)
mean.disp.tot <- svyby(~disp.inc, ~year, silc.hd.svy, svymean, keep.var = FALSE)

#Change column names:
names(mean.fac.tot)[names(mean.fac.tot) == 'statistic'] <- 'fac.inc.P1'
names(mean.nat.tot)[names(mean.nat.tot) == 'statistic'] <- 'nat.inc.P1'
names(mean.disp.tot)[names(mean.disp.tot) == 'statistic'] <- 'disp.inc.P1'

#Join mean values into one table:
mean.tot.p1 <- join_all(list(mean.fac.tot, mean.nat.tot, mean.disp.tot),
                        by = 'year')

#remove unnecessary tables
rm(mean.fac.tot, mean.nat.tot, mean.disp.tot)

# auf 2 Dezimalstellen runden
mean.tot.p1 <- round(mean.tot.p1, digits = 0)

# als excel speichern um eine Tabelle zu haben
write.csv(mean.tot.p1, file = "reports/GBR/tables/mean.tot.p1.csv",row.names=FALSE)


#### Median Income für alle Jahre #####

med.fac.tot <- svyby(~fac.inc, ~year, silc.hd.svy,
                     svyquantile, ~fac.inc, quantiles = c(0.5), keep.var = FALSE)
med.nat.tot <- svyby(~nat.inc, ~year, silc.hd.svy,
                     svyquantile, ~nat.inc, quantiles = c(0.5), keep.var = FALSE)
med.disp.tot <- svyby(~disp.inc, ~year, silc.hd.svy,
                      svyquantile, ~disp.inc, quantiles = c(0.5), keep.var = FALSE)

#change column names:
names(med.fac.tot)[names(med.fac.tot) == 'statistic'] <- 'fac.inc.P1'
names(med.nat.tot)[names(med.nat.tot) == 'statistic'] <- 'nat.inc.P1'
names(med.disp.tot)[names(med.disp.tot) == 'statistic'] <- 'disp.inc.P1'

#Join median values into one table:
med.tot.p1 <- join_all(list(med.fac.tot, med.nat.tot, med.disp.tot ),
                       by = 'year')

#remove unnecessary tables
rm(med.fac.tot, med.nat.tot, med.disp.tot)

med.tot.p1 <- round(med.tot.p1, digits = 0)

# als excel speichern um eine Tabelle zu haben
write.csv(med.tot.p1, file = "reports/GBR/tables/med.tot.p1.csv",row.names=FALSE)


# Gini für alle Jahre # alle Einkommen
# 
gini.fac.tot <- svyby(~fac.inc, ~year, silc.hd.svy, svygini, keep.var = FALSE)
gini.nat.tot <- svyby(~nat.inc, ~year, silc.hd.svy, svygini, keep.var = FALSE)
gini.disp.tot <- svyby(~disp.inc, ~year, silc.hd.svy, svygini, keep.var = FALSE)

#change column names:
names(gini.fac.tot)[names(gini.fac.tot) == 'statistic'] <- 'fac.inc.P1'
names(gini.nat.tot)[names(gini.nat.tot) == 'statistic'] <- 'nat.inc.P1'
names(gini.disp.tot)[names(gini.disp.tot) == 'statistic'] <- 'disp.inc.P1'

#Join gini values into one table:
gini.tot.p1 <- join_all(list(gini.fac.tot, gini.nat.tot, gini.disp.tot ),
                        by = 'year')

#remove unnecessary tables
rm(gini.fac.tot, gini.nat.tot, gini.disp.tot)

gini.tot.p1 <- round(gini.tot.p1, digits = 3)

# als excel speichern um eine Tabelle zu haben
write.csv(gini.tot.p1, file = "reports/GBR/tables/gini.tot.p1.csv",row.names=FALSE)

# decile points

#David: Fuer nat und disp hinzugefuegt. Brauchen wir nicht nur die top10 shares?

#decile.fac <- svyby(~fac.inc, ~pb010, silc.hd.svy,
#svyquantile, ~fac.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

#decile.nat <- svyby(~nat.inc, ~pb010, silc.hd.svy,
#svyquantile, ~fac.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

#decile.disp <- svyby(~disp.inc, ~pb010, silc.hd.svy,
#svyquantile, ~fac.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Quantile Share Ratio. 
#

quint.fac <- svyby(~fac.inc, ~year, silc.hd.svy, svyqsr, keep.var = FALSE)
quint.nat <- svyby(~nat.inc, ~year, silc.hd.svy, svyqsr, keep.var = FALSE)
quint.disp <- svyby(~disp.inc, ~year, silc.hd.svy, svyqsr, keep.var = FALSE)

names(quint.fac)[names(quint.fac) == 'statistic'] <- 'quint.fac.P1'
names(quint.nat)[names(quint.nat) == 'statistic'] <- 'quint.nat.P1'
names(quint.disp)[names(quint.disp) == 'statistic'] <- 'quint.disp.P1'

#Join into one table:
quint.tot.p1 <- join_all(list(quint.fac, quint.nat, quint.disp),
                         by = 'year')

#remove unnecessary tables
rm(quint.fac, quint.nat, quint.disp)

#Kommastellen entfernen
quint.tot.p1 <- round(quint.tot.p1, digits = 2)

# als excel speichern um eine Tabelle zu haben
write.csv(quint.tot.p1, file = "reports/GBR/tables/quint.tot.p1.csv",row.names=FALSE)

#quant.fac <- svyby(~fac.inc, ~pb010, silc.hd.svy, svyqsr, ~fac.inc,  0.2, 0.8,
#                   keep.var = FALSE)

svyqsr(~nat.inc, silc.hd.svy, 0.2, 0.8)
svyqsr(~disp.inc, silc.hd.svy, 0.2, 0.8)


# Top 10% Anteil

ten.fac <- as.data.frame(svyby(~fac.inc, ~year, subset(silc.hd.svy, fac.inc >=
                                                         svyby(~fac.inc, ~ year, silc.hd.svy, svyquantile, quantile = 0.9,
                                                               keep.var = FALSE), svytotal, keep.var = FALSE), svytotal, keep.var = FALSE) / svyby(~fac.inc, ~year, silc.hd.svy, svytotal, keep.var = FALSE))

ten.nat <- as.data.frame(svyby(~nat.inc, ~year,subset(silc.hd.svy, nat.inc >=                                                     svyby(~nat.inc, ~year, silc.hd.svy, svyquantile, quantile = 0.9, keep.var = FALSE), svytotal, keep.var = FALSE), svytotal, keep.var = FALSE) / svyby(~nat.inc, ~year, silc.hd.svy, svytotal, keep.var = FALSE))

ten.disp <- as.data.frame(svyby(~disp.inc, ~year,subset(silc.hd.svy, disp.inc >=                                                     svyby(~disp.inc, ~year, silc.hd.svy, svyquantile, quantile = 0.9, keep.var = FALSE), svytotal, keep.var = FALSE),svytotal, keep.var = FALSE) / svyby(~disp.inc, ~year, silc.hd.svy, svytotal, keep.var = FALSE))

names(ten.fac)[names(ten.fac) == 'statistic'] <- 'ten.fac'
names(ten.nat)[names(ten.nat) == 'statistic'] <- 'ten.nat'
names(ten.disp)[names(ten.disp) == 'statistic'] <- 'ten.disp'


ten.fac <- ten.fac %>% mutate(year = row.names(ten.fac))
ten.nat <- ten.nat %>% mutate(year = row.names(ten.nat))
ten.disp <- ten.disp %>% mutate(year = row.names(ten.disp))

#Join into one table # funktioniert nicht
ten.tot.p1 <- join_all(list(ten.fac, ten.nat, ten.disp), by="row.names")

#remove unnecessary tables
rm(ten.fac, ten.nat, ten.disp)



############################
######## P2 ################
############################

#-----------------------------------
#Indikatoren auf Haushaltsebene - P2
#-----------------------------------

silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p2h) %>% convey_prep()

# Mean Income für alle Jahre 

mean.fac.tot <- svyby(~fac.inc, ~year, silc.hd.svy, svymean, keep.var = FALSE)
mean.nat.tot <- svyby(~nat.inc, ~year, silc.hd.svy, svymean, keep.var = FALSE)
mean.disp.tot <- svyby(~disp.inc, ~year, silc.hd.svy, svymean, keep.var = FALSE)

#Change column names:
names(mean.fac.tot)[names(mean.fac.tot) == 'statistic'] <- 'fac.inc.P2'
names(mean.nat.tot)[names(mean.nat.tot) == 'statistic'] <- 'nat.inc.P2'
names(mean.disp.tot)[names(mean.disp.tot) == 'statistic'] <- 'disp.inc.P2'

#Join mean values into one table:
mean.tot.p2 <- join_all(list(mean.fac.tot, mean.nat.tot, mean.disp.tot),
                        by = 'year')

#remove unnecessary tables
rm(mean.fac.tot, mean.nat.tot, mean.disp.tot)

mean.tot.p2 <- round(mean.tot.p2, digits = 0)

# P1 und P2 zusammenfügen

mean.tot <- left_join(mean.tot.p1, mean.tot.p2, by = c("year"="year"))

# als excel speichern um eine Tabelle zu haben
write.csv(mean.tot, file = "reports/GBR/tables/mean.tot.csv",row.names=FALSE)

# Median Income für alle Jahre
med.fac.tot <- svyby(~fac.inc, ~year, silc.hd.svy,
                     svyquantile, ~fac.inc, quantiles = c(0.5), keep.var = FALSE)
med.nat.tot <- svyby(~nat.inc, ~year, silc.hd.svy,
                     svyquantile, ~nat.inc, quantiles = c(0.5), keep.var = FALSE)
med.disp.tot <- svyby(~disp.inc, ~year, silc.hd.svy,
                      svyquantile, ~disp.inc, quantiles = c(0.5), keep.var = FALSE)

#change column names:
names(med.fac.tot)[names(med.fac.tot) == 'statistic'] <- 'fac.inc.P2'
names(med.nat.tot)[names(med.nat.tot) == 'statistic'] <- 'nat.inc.P2'
names(med.disp.tot)[names(med.disp.tot) == 'statistic'] <- 'disp.inc.P2'

#Join median values into one table:
med.tot.p2 <- join_all(list(med.fac.tot, med.nat.tot, med.disp.tot ),
                       by = 'year')

#remove unnecessary tables
rm(med.fac.tot, med.nat.tot, med.disp.tot)

med.tot.p2 <- round(med.tot.p2, digits = 0)

#P1 und P2 zusammenfügen
med.tot <- left_join(med.tot.p1, med.tot.p2, by = c("year"="year"))

# als excel speichern um eine Tabelle zu haben
write.csv(med.tot, file = "reports/GBR/tables/med.tot.csv",row.names=FALSE)


# Gini für alle Jahre # nur jeweils die positiven Einkommen
gini.fac.tot <- svyby(~fac.inc, ~year, silc.hd.svy, svygini, keep.var = FALSE)
gini.nat.tot <- svyby(~nat.inc, ~year, silc.hd.svy, svygini, keep.var = FALSE)
gini.disp.tot <- svyby(~disp.inc, ~year, silc.hd.svy, svygini, keep.var = FALSE)

#change column names:
names(gini.fac.tot)[names(gini.fac.tot) == 'statistic'] <- 'fac.inc.P2'
names(gini.nat.tot)[names(gini.nat.tot) == 'statistic'] <- 'nat.inc.P2'
names(gini.disp.tot)[names(gini.disp.tot) == 'statistic'] <- 'disp.inc.P2'

#Join gini values into one table:
gini.tot.p2 <- join_all(list(gini.fac.tot, gini.nat.tot, gini.disp.tot ),
                        by = 'year')

#remove unnecessary tables
rm(gini.fac.tot, gini.nat.tot, gini.disp.tot)

# auf 3 Dezimalstellen runden
gini.tot.p2 <- round(gini.tot.p2, digits = 3)

# mit Gini P1 verbinden

gini.tot <- left_join(gini.tot.p1, gini.tot.p2, by = c("year"="year"))

# als excel speichern um eine Tabelle zu haben
write.csv(gini.tot, file = "reports/GBR/tables/gini.tot.csv",row.names=FALSE)


# decile points

#David: Fuer nat und disp hinzugefuegt. Brauchen wir nicht nur die top10 shares?

#decile.fac <- svyby(~fac.inc, ~pb010, silc.hd.svy,
#svyquantile, ~fac.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

#decile.nat <- svyby(~nat.inc, ~pb010, silc.hd.svy,
#svyquantile, ~fac.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

#decile.disp <- svyby(~disp.inc, ~pb010, silc.hd.svy,
#svyquantile, ~fac.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Quantile Share Ratio. 
#

quint.fac <- svyby(~fac.inc, ~year, silc.hd.svy, svyqsr, keep.var = FALSE)
quint.nat <- svyby(~nat.inc, ~year, silc.hd.svy, svyqsr, keep.var = FALSE)
quint.disp <- svyby(~disp.inc, ~year, silc.hd.svy, svyqsr, keep.var = FALSE)

names(quint.fac)[names(quint.fac) == 'statistic'] <- 'quint.fac.P2'
names(quint.nat)[names(quint.nat) == 'statistic'] <- 'quint.nat.P2'
names(quint.disp)[names(quint.disp) == 'statistic'] <- 'quint.disp.P2'

#Join into one table:
quint.tot.p2 <- join_all(list(quint.fac, quint.nat, quint.disp),
                         by = 'year')

#remove unnecessary tables
rm(quint.fac, quint.nat, quint.disp)

quint.tot.p2 <- round(quint.tot.p2, digits = 2)

#P1 & P2 zusammenfügen

quint.tot <- left_join(quint.tot.p1, quint.tot.p2, by = c("year"="year"))

# als excel speichern um eine Tabelle zu haben
write.csv(quint.tot, file = "reports/GBR/tables/quint.tot.csv",row.names=FALSE)

#quant.fac <- svyby(~fac.inc, ~pb010, silc.hd.svy, svyqsr, ~fac.inc,  0.2, 0.8,
#                   keep.var = FALSE)

svyqsr(~nat.inc, silc.hd.svy, 0.2, 0.8)
svyqsr(~disp.inc, silc.hd.svy, 0.2, 0.8)


# Top 10% Anteil

ten.fac <- as.data.frame(svyby(~fac.inc, ~year, subset(silc.hd.svy, fac.inc >=
                                                         svyby(~fac.inc, ~ year, silc.hd.svy, svyquantile, quantile = 0.9,
                                                               keep.var = FALSE), svytotal, keep.var = FALSE), svytotal, keep.var = FALSE) / svyby(~fac.inc, ~year, silc.hd.svy, svytotal, keep.var = FALSE))

ten.nat <- as.data.frame(svyby(~nat.inc, ~year,subset(silc.hd.svy, nat.inc >=                                                     svyby(~nat.inc, ~year, silc.hd.svy, svyquantile, quantile = 0.9, keep.var = FALSE), svytotal, keep.var = FALSE), svytotal, keep.var = FALSE) / svyby(~nat.inc, ~year, silc.hd.svy, svytotal, keep.var = FALSE))

ten.disp <- as.data.frame(svyby(~disp.inc, ~year,subset(silc.hd.svy, disp.inc >=                                                     svyby(~disp.inc, ~year, silc.hd.svy, svyquantile, quantile = 0.9, keep.var = FALSE), svytotal, keep.var = FALSE),svytotal, keep.var = FALSE) / svyby(~disp.inc, ~year, silc.hd.svy, svytotal, keep.var = FALSE))

names(ten.fac)[names(ten.fac) == 'statistic'] <- 'ten.fac'
names(ten.nat)[names(ten.nat) == 'statistic'] <- 'ten.nat'
names(ten.disp)[names(ten.disp) == 'statistic'] <- 'ten.disp'


ten.fac <- ten.fac %>% mutate(year = row.names(ten.fac))
ten.nat <- ten.nat %>% mutate(year = row.names(ten.nat))
ten.disp <- ten.disp %>% mutate(year = row.names(ten.disp))

#Join into one table # funktioniert nicht
ten.tot.p2 <- join_all(list(ten.fac, ten.nat, ten.disp), by="row.names")

#remove unnecessary tables
rm(ten.fac, ten.nat, ten.disp)




#-----------------------------------------
#Indikatoren auf Personenebene - P2
#-----------------------------------------

silc.pd.svy <- svydesign(ids = ~id_p,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.p2) %>% convey_prep()

#------------------------------------------
# Mean Income für alle Jahre - Individuen
#------------------------------------------

mean.p.fac.tot <- svyby(~fac.inc, ~year, silc.pd.svy, svymean, keep.var = FALSE)
mean.p.nat.tot <- svyby(~nat.inc, ~year, silc.pd.svy, svymean, keep.var = FALSE)
mean.p.disp.tot <- svyby(~disp.inc, ~year, silc.pd.svy, svymean, keep.var = FALSE)

#Change column names:
names(mean.p.fac.tot)[names(mean.p.fac.tot) == 'statistic'] <- 'mean.p.fac.inc'
names(mean.p.nat.tot)[names(mean.p.nat.tot) == 'statistic'] <- 'mean.p.nat.inc'
names(mean.p.disp.tot)[names(mean.p.disp.tot) == 'statistic'] <- 'mean.p.disp.inc'

#Join mean values into one table:
mean.tot.p2.p <- join_all(list(mean.p.fac.tot, mean.p.nat.tot, mean.p.disp.tot),
                          by = 'year')

#remove unnecessary tables
rm(mean.p.fac.tot, mean.p.nat.tot, mean.p.disp.tot)

#die Kommastellen abfeilen
mean.tot.p2.p <- round(mean.tot.p2.p, digits = 0)

# als excel speichern um eine Tabelle zu haben
write.csv(mean.tot.p2.p, file = "reports/GBR/tables/mean.tot.p2.p.csv",row.names=FALSE)

#--------------------------------------------
# Median Income fuer alle Jahre - Individuen
#--------------------------------------------

# Median Income für alle Jahre
med.p.fac.tot <- svyby(~fac.inc, ~year, silc.pd.svy,
                       svyquantile, ~fac.inc, quantiles = c(0.5), keep.var = FALSE)
med.p.nat.tot <- svyby(~nat.inc, ~year, silc.pd.svy,
                       svyquantile, ~nat.inc, quantiles = c(0.5), keep.var = FALSE)
med.p.disp.tot <- svyby(~disp.inc, ~year, silc.pd.svy,
                        svyquantile, ~disp.inc, quantiles = c(0.5), keep.var = FALSE)

#change column names:
names(med.p.fac.tot)[names(med.p.fac.tot) == 'statistic'] <- 'med.p.fac.inc'
names(med.p.nat.tot)[names(med.p.nat.tot) == 'statistic'] <- 'med.p.nat.inc'
names(med.p.disp.tot)[names(med.p.disp.tot) == 'statistic'] <- 'med.p.disp.inc'

#Join median values into one table:
med.tot.p2.p <- join_all(list(med.p.fac.tot, med.p.nat.tot, med.p.disp.tot ),
                         by = 'year')

#remove unnecessary tables
rm(med.p.fac.tot, med.p.nat.tot, med.p.disp.tot)

#die Kommastellen abfeilen
med.tot.p2.p <- round(med.tot.p2.p, digits = 0)

# als excel speichern um eine Tabelle zu haben
write.csv(med.tot.p2.p, file = "reports/GBR/tables/med.tot.p2.p.csv",row.names=FALSE)

#--------------------------------------------
# Gini für alle Jahre - Individuen
#--------------------------------------------

gini.p.fac.tot <- svyby(~fac.inc, ~year, silc.pd.svy, svygini, keep.var = FALSE)
gini.p.nat.tot <- svyby(~nat.inc, ~year, silc.pd.svy, svygini, keep.var = FALSE)
gini.p.disp.tot <- svyby(~disp.inc, ~year, silc.pd.svy, svygini, keep.var = FALSE)

#change column names:
names(gini.p.fac.tot)[names(gini.p.fac.tot) == 'statistic'] <- 'gini.p.fac.inc'
names(gini.p.nat.tot)[names(gini.p.nat.tot) == 'statistic'] <- 'gini.p.nat.inc'
names(gini.p.disp.tot)[names(gini.p.disp.tot) == 'statistic'] <- 'gini.p.disp.inc'

#Join gini values into one table:
gini.tot.p2.p <- join_all(list(gini.p.fac.tot, gini.p.nat.tot, gini.p.disp.tot ),
                          by = 'year')

#remove unnecessary tables
rm(gini.p.fac.tot, gini.p.nat.tot, gini.p.disp.tot)

# auf 2 Dezimalstellen runden
gini.tot.p2.p <- round(gini.tot.p2.p, digits = 2)

# als excel speichern um eine Tabelle zu haben
write.csv(gini.tot.p2.p, file = "reports/GBR/tables/gini.tot.p2.p.csv",row.names=FALSE)

#--------------------------------------------
# P80/20 Individuen
#--------------------------------------------

#P80/20 for individuals
quant.p.fac <- svyby(~fac.inc, ~year, silc.pd.svy, svyqsr, keep.var = FALSE)
quant.p.nat <- svyby(~nat.inc, ~year, silc.pd.svy, svyqsr, keep.var = FALSE)
quant.p.disp <- svyby(~disp.inc, ~year, silc.pd.svy, svyqsr, keep.var = FALSE)

#Change column names:
names(quant.p.fac)[names(quant.p.fac) == 'statistic'] <- 'quant.p.fac'
names(quant.p.nat)[names(quant.p.nat) == 'statistic'] <- 'quant.p.nat'
names(quant.p.disp)[names(quant.p.disp) == 'statistic'] <- 'quant.p.disp'

#Join mean values into one table:
quant.tot.p2.p <- join_all(list(quant.p.nat, quant.p.disp),
                           by = 'year')

#remove unnecessary tables
rm(quant.p.fac, quant.p.nat, quant.p.disp)

#Kommastellen auf 0
quant.tot.p2.p <- round(quant.tot.p2.p, digits = 0)

# als excel speichern um eine Tabelle zu haben
write.csv(quant.tot.p2.p, file = "reports/GBR/tables/quant.tot.p2.p.csv",row.names=FALSE)

#--------------------------------------------------------
#Ending Individuals Indicators
#--------------------------------------------------------

######## Ende indicators base script ########
###############################################################################

