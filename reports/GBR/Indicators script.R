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

country <- "UK"
year <- 2013

# Source the Setup scripts to provide merged household and personal data
# braucht es nicht wenn wir alles in einem script machen
source("R/_connection.R")
source("R/_setup.R")


# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income
silc.pd.inc <- silc.pd %>% filter(py010g > 0)
silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

# Creating Survey Objects -------------------------------------------------
## müsste man dann noch auf die oben kreierten silc.pd.inc umschreiben

# personal cross section weighting. braucht es nicht da die Einkommen auf HH sind?
silc.pd.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.p1.13) %>% convey_prep()

# household cross section weighting
silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.p1.13) %>% convey_prep()


# Indicators --------------------------------------------------------------

# Mean Income
#

svymean(~fac.inc, silc.hd.svy)
svymean(~nat.inc, silc.hd.svy)
svymean(~disp.inc, silc.hd.svy)

# For comparing regions?

svyby(~hy010, ~as.factor(db020), silc.hd.svy, svymean)

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

svygini(~fac.inc, silc.hd.svy)
svygini(~nat.inc, silc.hd.svy)
svygini(~disp.inc, silc.hd.svy)

# For comparing countries

svyby(~fac.inc, ~as.factor(db040), silc.hd.svy, svygini)

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


###############################################################################
###### Indicators laeken script #####

# ------------------------------------------------------------------------
#
# Laeken Indicators
# Autoren: Engelen & Kuschnig
# Datum: 2018-11-10
#
# -------------------------------------------------------------------------

library(laeken)
library(dplyr)

country <- "CZ"
year <- 2013

# Source the Setup scripts to provide merged household and personal data
source("R/_connection.R")
source("R/_setup.R")


# Subsetting --------------------------------------------------------------

# To get useful results we may want to subset to only positive income
silc.pd.inc <- silc.pd %>% filter(py010g > 0)
silc.hd.inc <- silc.hd %>% filter(hy010 > 0)

# For hourly wages we replace NAs in working hours with 0
silc.pd.wage <- silc.pd
silc.pd.wage$pl060[is.na(silc.pd.wage$pl060)] <- 0
silc.pd.wage$pl100[is.na(silc.pd.wage$pl100)] <- 0
# Filter out observations with no income and/or hours worked
silc.pd.wage <- silc.pd.wage %>% 
  filter(py010g > 0 & pl060 > 0 & (pl073 + pl074) > 0)


# Indicators --------------------------------------------------------------

# Share of population at risk of poverty
#
arpr(inc = silc.pd.inc$py010g, weights = silc.pd.inc$pb040, 
     breakdown = silc.pd.inc$pb020)
arpr(inc = silc.hd.inc$hy010, silc.hd.inc$db090)

# Gender Pay Gap
#
silc.pd.wage <- silc.pd.wage %>% 
  mutate(hwages = py010g / ((pl060 + pl100) * (pl073 + pl074) * 52 / 12),
         gender = factor(pb150, labels = c("Male", "Female")))

silc.pd.wage <- silc.pd.wage %>% # Make sure female is the first level
  mutate(gender = relevel(gender, "Female"))
gpg(silc.pd.wage$hwages, gender = silc.pd.wage$gender)


###### Ende Indicators laeken script #####

