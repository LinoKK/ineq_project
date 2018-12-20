###############################################################################
#### Indicators base script ####

#
# Indicators R-Script
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-08
#
# -------------------------------------------------------------------------

library(survey)
library(convey)
library(plyr)
library(dplyr)


# Source the Setup scripts to provide merged household and personal data
# braucht es nicht wenn wir alles in einem script machen
source("R/_connection.R")
source("R/_setup.R")

# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income 
silc.p2 <- silc.p2 %>% filter(hy010 > 0)


# Creating Survey Objects -------------------------------------------------
## müsste man dann noch auf die oben kreierten silc.pd.inc umschreiben

# personal cross section weighting. braucht es nicht da die Einkommen auf HH sind?
# silc.pd.svy <- svydesign(ids =  ~ id_h,
                         #strata = ~db020,
                         #weights = ~pb040,
                         #data = silc.p1.full) %>% convey_prep()

# household cross section weighting
silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p2) %>% convey_prep()


# Indicators --------------------------------------------------------------

# Basisset der Indikatoren: Median, Mean, Gini, 80/20, Top 10 von fac.inc, 
# nat.inc & disp.inc für P1 & P2 über den Zeitraum 2005-2016

# P1

# Mean Income für alle Jahre pb010 für p2 rb010 für p1
#
mean.fac.tot <- svyby(~fac.inc, ~rb010, silc.hd.svy, svymean)
mean.nat.tot <- svyby(~nat.inc, ~rb010, silc.hd.svy, svymean)
mean.disp.tot <- svyby(~disp.inc, ~rb010, silc.hd.svy, svymean)

mean.tot <- join_all(list(mean.fac.tot, mean.nat.tot, mean.disp.tot ),
                     by = 'rb010')

rm(mean.fac.tot, mean.nat.tot, mean.disp.tot)

# Median Income für alle Jahre
# jeweils noch die column benennen

med.fac.tot <- svyby(~fac.inc, ~rb010, silc.hd.svy,
                     svyquantile, ~fac.inc, quantiles = c(0.5), keep.var = FALSE)
names(med.fac.tot)[names(med.fac.tot) == 'statistic'] <- 'med.fac.inc'

med.nat.tot <- svyby(~nat.inc, ~rb010, silc.hd.svy,
                     svyquantile, ~nat.inc, quantiles = c(0.5), keep.var = FALSE)
names(med.nat.tot)[names(med.nat.tot) == 'statistic'] <- 'med.nat.inc'

med.disp.tot <- svyby(~disp.inc, ~rb010, silc.hd.svy,
                      svyquantile, ~disp.inc, quantiles = c(0.5), keep.var = FALSE)
names(med.disp.tot)[names(med.disp.tot) == 'statistic'] <- 'med.disp.inc'

med.tot <- join_all(list(med.fac.tot, med.nat.tot, med.disp.tot ),
                     by = 'rb010')

rm(med.fac.tot, med.nat.tot, med.disp.tot)

# Gini für alle Jahre, gini ist viel zu hoch

gini.fac.tot <- svyby(~fac.inc, ~pb010, silc.hd.svy, svygini)
gini.nat.tot <- svyby(~nat.inc, ~pb010, silc.hd.svy, svygini)
gini.disp.tot <- svyby(~disp.inc, ~pb010, silc.hd.svy, svygini)

gini.tot <- join_all(list(gini.fac.tot, gini.nat.tot, gini.disp.tot ),
                     by = 'pb010')

rm(gini.fac.tot, gini.nat.tot, gini.disp.tot)

# decile points


decile.fac <- svyby(~fac.inc, ~pb010, silc.hd.svy,
                     svyquantile, ~fac.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Quantile Share Ratio. funktioniert noch nicht.
#
quant.fac <- svyby(~fac.inc, ~pb010, silc.hd.svy, svyqsr, ~fac.inc,  0.2, 0.8,
                   keep.var = FALSE)
svyqsr(~nat.inc, silc.hd.svy, 0.2, 0.8)
svyqsr(~disp.inc, silc.hd.svy, 0.2, 0.8)

# For comparing regions?

mean16 <- svyby(~disp.inc, ~as.factor(db040.y), silc.hd.svy, svymean)


# Entwicklung des Gini in den Regionen #
## vor 2009 hat es keine info über die region

# damit man nachher mergen kann als data frame
gini15 <- as.data.frame(svyby(~disp.inc, ~as.factor(db040.y), silc.hd.svy, svygini))

# Zeitreihe, run for every year
gini10 <- gini10 %>% mutate(region = `as.factor(db040)`, disp.inc.gini.10 = 
                              disp.inc)

# Tabellen verkleinern und nur Region und Gini nehmen
gini16 <- select(gini16, region, disp.inc.gini.16)

# Tabellen zusammenfügen
gini <- (merge(gini, gini16, by = 'region', select=c(disp.inc)))

# Alternativ, R gibt aber eine Warnung wegen plyr, keine Ahnung ob da was passiert
library(plyr)
gini1 <- join_all(list(gini10, gini11, gini12, gini13, gini14, gini15, gini16),
                  by = 'region')



# Quantile Share Ratio
#
svyqsr(~fac.inc, silc.hd.svy, 0.2, 0.8)
svyqsr(~nat.inc, silc.hd.svy, 0.2, 0.8)
svyqsr(~disp.inc, silc.hd.svy, 0.2, 0.8)


# Top 10% Income Share
#

svytotal(~fac.inc, subset(silc.hd.svy, fac.inc >= 
                            as.numeric(svyquantile(~fac.inc, silc.hd.svy, quantile = 0.9)))) /
  svytotal(~fac.inc,silc.hd.svy)

svytotal(~nat.inc, subset(silc.hd.svy, nat.inc >= 
                            as.numeric(svyquantile(~nat.inc, silc.hd.svy, quantile = 0.9)))) /
  svytotal(~nat.inc,silc.hd.svy)

svytotal(~disp.inc, subset(silc.hd.svy, disp.inc >= 
                             as.numeric(svyquantile(~disp.inc, silc.hd.svy, quantile = 0.9)))) /
  svytotal(~disp.inc,silc.hd.svy)

# Gini Coefficient
#

svygini(~fac.inc,  silc.hd.svy)
svygini(~nat.inc, silc.hd.svy)
svygini(~disp.inc, silc.hd.svy)

# For comparing countries

gini15 <- svyby(~disp.inc, ~as.factor(db040.y), silc.hd.svy, svygini)

# Theil Index
#
svygei(~total.inc, silc.pd.svy, epsilon = 1)
svygei(~hy010, silc.hd.svy, epsilon = 1)

# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
#      svygei, epsilon = 1)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy,
#      svygei, epsilon = 1)


######## Ende indicators base script ########
###############################################################################

