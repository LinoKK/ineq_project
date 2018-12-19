# r code für die Berechnungen

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

# nur c13p entsprechend der mail für py021g (ansonsten heißt die tabelle pp). auf
# GitHub hat es ein neues file mit code für diese speziellen Tabellen 

## Die Jahre der Tabellen ändern cxxp, cxxd, cxxh, cxxr
# bis einschliesslich 2006 die variable py020n statt py021g (Zeile 49, 124 & 198)
# für 2017 funktioniert die r tabelle nicht

year <- 2017

silc.p <- tbl(pg, "c17p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

silc.h <- tbl(pg, "c17h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050) %>%
  collect(n = Inf)

# beinhaltet region und cross section household weight
silc.d <- tbl(pg, "c17d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

# beinhaltet personal cross sectional personal weight rb050, personal id rb030 sollte pb030 entsprechen
silc.r <- tbl(pg, "c17r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)


# Create unique IDs for merging id_h is household, id_p is person
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))
silc.p <- silc.p %>% mutate(id_p = paste0(pb020, pb030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

silc.r <- silc.r %>% mutate(id_p = paste0(rb020, rb030))
silc.r <- silc.r %>% mutate(id_h = paste0(rb020, rx030))

# Merge the datasets
silc.pd <- left_join(silc.p, silc.d %>% select(id_h, db020, db090, db040))

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

## match to r ## schauen dass das matching funktioniert, es muss nach id_p 

silc.rh <- left_join(silc.r, silc.h1)
silc.rhpd <- left_join(silc.rh, silc.pd, by=c("id_p"))

# bei dem vorigen merge hat es zwei mal id_h die r dann umbenennt, daher:
names(silc.rhpd)[names(silc.rhpd) == 'id_h.x'] <- 'id_h'

# Region matching needs to be based on id_h
silc.rhpd <- left_join(silc.rhpd, silc.d, by=c("id_h"), select=c(db040))

names(silc.rhpd)[names(silc.rhpd) == 'db020.y'] <- 'db020'
names(silc.rhpd)[names(silc.rhpd) == 'db040.y.'] <- 'db040'
names(silc.rhpd)[names(silc.rhpd) == 'db090.y'] <- 'db090'


# replace NAs
silc.rhpd <- silc.rhpd %>% replace(is.na(.), 0)

# 1.1 Einkommen aus Arbeit: py010g+py021g+py050g+hy110g - bis 2006 py020
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

# für das spezielle Jahr speichern, nur die notwendigen variablen

silc.p1.17 <- subset(silc.rhpd, select=c(id_h, id_p, work.inc, cap.inc, fac.inc, 
                                         nat.inc, disp.inc, db020, rb010, pb040, db090,
                                         db040.y))


### Summing up, this led to the following variables to calculate the inequality
### indicators with dataset silc.p1.YY:
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

library(data.table)
silc.p2 <- data.table(silc.p2)
silc.p21 <- left_join(silc.p2, silc.p2[, list(h.size=sum(count)), by = id_h])

silc.h <- left_join(silc.h, silc.p21)

# divide by HH size
silc.h2 <- silc.h %>% 
  mutate(eq.hy110g = hy110g/h.size,
         eq.h.cap.inc = (hy040g + hy090g)/h.size,
         eq.h.trans.inc = (hy050g + hy060g + hy070g + hy080g)/h.size,
         eq.h.expenses = (hy120g + hy130g + hy140g)/h.size) 

#match            
silc.p2d <- left_join(silc.p2, silc.d %>% select(id_h, db020, db090, db040))
silc.pdh2 <- left_join(silc.p2d, silc.h2)

# replace NAs
silc.pdh2 <- silc.pdh2 %>% replace(is.na(.), 0)

# 1.1 Einkommen aus Arbeit: py010g+py021g+py050g+hy110g ## hier noch py021g zu 
# py020n ändern
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

# für das spezielle Jahr speichern

silc.p2.17 <- subset(silc.pdh2, select=c(id_h, work.inc, cap.inc, fac.inc, 
                                         nat.inc, disp.inc, db020, pb040, db090,
                                         db040))


### Summing up, this led to the following variables to calculate the inequality
### indicators with dataset silc.p2.YY:
# 1. Arbeitseinkommen: work.inc
# 2. Vermögenseinkommen: cap.inc
# 3. Pre-tax factor income: fac.inc
# 4. Pre-tax national income: nat.inc
# 5. Post-tax disposable income: disp.inc


# alle Jahre zusammenfügen

### 2011 ist die regionenaufteilung genauer, letzter Buchstabe muss weg damit die 
### Aufteilung mit den anderen Jahren übereinstimmt
## vor 2009 hat es keine info über die region

#P1
mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}
silc.p1.10 <- silc.p1.10 %>% mutate(db040.y = mid(db040.y, 1, nchar(db040.y) -1))
silc.p1.11 <- silc.p1.11 %>% mutate(db040.y = mid(db040.y, 1, nchar(db040.y) -1))

silc.p1.full <- bind_rows(silc.p1.05, silc.p1.06, silc.p1.07, silc.p1.08,
                          silc.p1.09, silc.p1.10, silc.p1.11, silc.p1.12, silc.p1.13,
                          silc.p1.14, silc.p1.15, silc.p1.16, silc.p1.17)
#P2

silc.p2.10 <- silc.p2.10 %>% mutate(db040.y = mid(db040.y, 1, nchar(db040.y) -1))
silc.p2.11 <- silc.p2.11 %>% mutate(db040.y = mid(db040.y, 1, nchar(db040.y) -1))

silc.p2.full <- bind_rows(silc.p2.05, silc.p2.06, silc.p2.07, silc.p2.08,
                          silc.p2.09, silc.p2.10, silc.p2.11, silc.p2.12, silc.p2.13,
                          silc.p2.14, silc.p2.15, silc.p2.16, silc.p2.17)
