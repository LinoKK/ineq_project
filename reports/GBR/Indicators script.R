###############################################################################
#### Indicators base script ####

#
# Indicators R-Script
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-08
#
# -------------------------------------------------------------------------

library(dplyr)
library(survey)
library(convey)
library(dplyr)
country <- "UK"
year <- 2013

# Source the Setup scripts to provide merged household and personal data
# braucht es nicht wenn wir alles in einem script machen
source("R/_connection.R")
source("R/_setup.R")

# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income
silc.p1.full.inc <- silc.p1.full %>% filter(disp.inc > 0)


# Creating Survey Objects -------------------------------------------------
## müsste man dann noch auf die oben kreierten silc.pd.inc umschreiben

# personal cross section weighting. braucht es nicht da die Einkommen auf HH sind?
silc.pd.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.p1.full) %>% convey_prep()

# household cross section weighting
silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p1.full) %>% convey_prep()


# Indicators --------------------------------------------------------------

# Basisset der Indikatoren: Median, Mean, Gini, 80/20, Top 10 von fac.inc, 
# nat.inc & disp.inc für P1 & P2 über den Zeitraum 2005-2016

# P1

# Mean Income für alle Jahre
#
mean.fac.tot <- svyby(~fac.inc, ~rb010, silc.hd.svy, svymean)
mean.nat.tot <- svyby(~nat.inc, ~rb010, silc.hd.svy, svymean)
mean.disp.tot <- svyby(~disp.inc, ~rb010, silc.hd.svy, svymean)


# Median Income
#

svyquantile(~fac.inc, silc.hd.svy, quantiles = c(0.5))
svyquantile(~nat.inc, silc.hd.svy, quantiles = c(0.5))
svyquantile(~disp.inc, silc.hd.svy, quantiles = c(0.5))

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


# Median Income
#

svyquantile(~fac.inc, silc.hd.svy, quantiles = c(0.5))
svyquantile(~nat.inc, silc.hd.svy, quantiles = c(0.5))
svyquantile(~disp.inc, silc.hd.svy, quantiles = c(0.5))

# Decile Points

svyquantile(~fac.inc, silc.hd.svy, quantiles = seq(0, 1, 0.1))
svyquantile(~nat.inc, silc.hd.svy, quantiles = seq(0, 1, 0.1))
svyquantile(~disp.inc, silc.hd.svy, quantiles = seq(0, 1, 0.1))

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

