# r code für die Berechnungen

#Davids update

##aus dem conncetion script#####
# Connect to the PostgreSQL database --------------------------------------

library(dplyr)

if(!exists("pg")) {
  
  if(!exists("password")) {password <- readline("Password: ")}
  pg <- src_postgres(dbname = "datacube", host = "ineq.wu.ac.at",
                     user = "lvineq", 
                     password = password, 
                     options = "-c search_path=silc")
} else {
  message("Connection pg already exists.")
}

##### Ende connection script ######


##### Aus dem Setup Script ######

# Setup -------------------------------------------------------------------

## exists befehl funktioniert nicht. manuell Year und country eingeben.
library(dplyr)
if(!exists(c("country", "year"))) {
  stop('UK' & '2013')
}


# Prepare Data ------------------------------------------------------------

# Download data

# enthält alle notwendigen Einkommenskomponenten entsprechend Mail vom 26.11.

#nur c13p entsprechend der mail für py021g (ansonsten heißt die tabelle pp). auf GitHub hat es ein neues file mit code für diese speziellen Tabellen 

silc.p <- tbl(pg, "c13p") %>%
  filter(pb020=='UK' & pb010==2013) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "hh") %>%
  filter(hb020=='UK' & hb010==2013) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050) %>%
  collect(n = Inf)

# beinhaltet region und cross section household weight
silc.d <- tbl(pg, "dd") %>%
  filter(db020=='UK' & db010==2013) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

# beinhaltet personal cross sectional personal weight rb050, personal id rb030 sollte pb030 entsprechen
silc.r <- tbl(pg, "rr") %>% 
  filter(rb020=='UK' & rb010==2013) %>%
  select(rb010, rb020, rb030, rb050) %>%
  collect(n = Inf)


# Create unique IDs for merging
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

silc.r <- silc.r %>% mutate(id_h = paste0(rb020, rb030))

# Merge the datasets
silc.pd <- left_join(silc.p, silc.d %>% select(id_h, db020, db090))

silc.hd <- left_join(silc.h, silc.d)

# drop NAs, für welche kategorien? bei fragezeichen dann zb hy050g eingeben
# silc.p <- tidyr::replace_na(silc.p, list(?=0))


############## Unterteilung gemäß der Mail vom 26.11. ########################

## Aufteilung P1 ##

# Aufteilung der jeweiligen Haushaltsgrößen nach Äquivalenzfaktor
silc.h1 <- silc.h %>% 
  mutate(eq.hy110g = hy110g/hx050,
         eq.h.cap.inc = (hy040g + hy090g)/hx050,
         eq.h.trans.inc = (hy050g + hy060g + hy070g + hy080g)/hx050,
         eq.h.expenses = (hy120g + hy130g + hy140g)/hx050) 

## match to r

silc.rh <- left_join(silc.r, silc.h1)
silc.rhpd <- left_join(silc.rh, silc.pd)

# replace NAs
silc.rhpd <- silc.rhpd %>% replace(is.na(.), 0)

# 1.1 Einkommen aus Arbeit: py010g+py021g+py050g+hy110g
silc.rhpd <- silc.rhpd %>% 
  mutate(work.inc = py010g + py021g + py050g + eq.hy110g)

# 1.2 Capital income

silc.rhpd <- silc.rhpd %>% 
  mutate(cap.inc = eq.h.cap.inc + py080g)

# factor income

silc.rhpd <- silc.rhpd %>% 
  mutate(fac.inc = cap.inc + work.inc)

# Pre-tax national income

silc.rhpd <- silc.rhpd %>% 
  mutate(nat.inc = fac.inc + py100g + py090g)

# post-tax disposable income

silc.rhpd <- silc.rhpd %>% 
  mutate(disp.inc = nat.inc + eq.h.trans.inc - eq.h.expenses + 
           py110g + py120g + py130g + py140g)

### Summing up, this led to the following variables to calculate the inequality
### indicators with dataset silc.rhpd:
# 1. Arbeitseinkommen: work.inc
# 2. Vermögenseinkommen: cap.inc
# 3. Pre-tax factor income: fac.inc
# 4. Pre-tax national income: nat.inc
# 5. Post-tax disposable income: disp.inc




## Aufteilung gemäß P2, nur über 20 jährige
#calculate age
silc.p <- silc.p %>% mutate(age = pb010 - pb140 )
silc.p2 <- subset(silc.p, age>19)

# haushaltsgröße

silc.p2 <- silc.p2 %>% mutate(count = 1)
silc.p21 <- group_by(silc.p2, id_h) %>% 
  summarize(h.size = sum(count)) 
silc.h <- left_join(silc.h, silc.p21)

# divide by HH size
silc.h2 <- silc.h %>% 
  mutate(eq.hy110g = hy110g/h.size,
         eq.h.cap.inc = (hy040g + hy090g)/h.size,
         eq.h.trans.inc = (hy050g + hy060g + hy070g + hy080g)/h.size,
         eq.h.expenses = (hy120g + hy130g + hy140g)/h.size) 

#match            
silc.p2d <- left_join(silc.p2, silc.d %>% select(id_h, db020, db090))
silc.pdh2 <- left_join(silc.p2d, silc.h2)

# replace NAs
silc.pdh2 <- silc.pdh2 %>% replace(is.na(.), 0)

# 1.1 Einkommen aus Arbeit: py010g+py021g+py050g+hy110g
silc.pdh2 <- silc.pdh2 %>% 
  mutate(work.inc = py010g + py021g + py050g + eq.hy110g)

# 1.2 Capital income

silc.pdh2 <- silc.pdh2 %>% 
  mutate(cap.inc = eq.h.cap.inc + py080g)

# factor income

silc.pdh2 <- silc.pdh2 %>% 
  mutate(fac.inc = cap.inc + work.inc)

# Pre-tax national income

silc.pdh2 <- silc.pdh2 %>% 
  mutate(nat.inc = fac.inc + py100g + py090g)

# post-tax disposable income

silc.pdh2 <- silc.pdh2 %>% 
  mutate(disp.inc = nat.inc + eq.h.trans.inc - eq.h.expenses + 
           py110g + py120g + py130g + py140g)

### Summing up, this led to the following variables to calculate the inequality
### indicators with dataset silc.pdh2:
# 1. Arbeitseinkommen: work.inc
# 2. Vermögenseinkommen: cap.inc
# 3. Pre-tax factor income: fac.inc
# 4. Pre-tax national income: nat.inc
# 5. Post-tax disposable income: disp.inc

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

silc.pd.svy <- svydesign(ids =  ~ id_h,
                         strata = ~db020,
                         weights = ~pb040,
                         data = silc.pd) %>% convey_prep()

silc.hd.svy <- svydesign(ids = ~id_h,
                         strata = ~db020,
                         weights = ~db090,
                         data = silc.hd) %>% convey_prep()


# Indicators --------------------------------------------------------------

# Mean Income
#
svymean(~total.inc, silc.pd.svy)
svymean(~hy010, silc.hd.svy)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svymean)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svymean)

# Median Income
#
svyquantile(~total.inc, silc.pd.svy, quantiles = c(0.5))
svyquantile(~hy010, silc.hd.svy, quantiles = c(0.5))

# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy,
#       svyquantile, ~total.inc, quantiles = c(0.5), keep.var = FALSE)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy,
#       svyquantile, ~hy010, quantiles = c(0.5), keep.var = FALSE)

# Decile Points
#
svyquantile(~total.inc, silc.pd.svy, quantiles = seq(0, 1, 0.1))
svyquantile(~hy010, silc.hd.svy, quantiles = seq(0, 1, 0.1))
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, 
#       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)
# svyby(~hy010, ~as.factor(hb020), silc.pd.svy, 
#       svyquantile, ~total.inc, quantiles = seq(0, 1, 0.1), keep.var = FALSE)

# Quantile Share Ratio
#
svyqsr(~total.inc, silc.pd.svy, 0.2, 0.8)
svyqsr(~hy010, silc.hd.svy, 0.2, 0.8)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svyqsr, 0.2, 0.8)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svyqsr, 0.2, 0.8)

# Top 10% Income Share
#
svytotal(~total.inc, subset(silc.pd.svy, pb020 == country & total.inc >= 
                              as.numeric(svyquantile(~total.inc, silc.pd.svy, quantile = 0.9)))) / 
  svytotal(~total.inc, subset(silc.pd.svy, pb020 == country))
svytotal(~hy010, subset(silc.hd.svy, db020 == country & hy010 >= 
                          as.numeric(svyquantile(~hy010, silc.hd.svy, quantile = 0.9)))) /
  svytotal(~hy010,subset(silc.hd.svy, db020 == country))

# Gini Coefficient
#
svygini(~total.inc, silc.pd.svy)
svygini(~hy010, silc.hd.svy)
# For comparing countries
# svyby(~total.inc, ~as.factor(db020), silc.pd.svy, svygini)
# svyby(~hy010, ~as.factor(db020), silc.hd.svy, svygini)

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

