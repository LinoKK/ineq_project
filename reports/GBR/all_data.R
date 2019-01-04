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

year <- 2016

c16p <- tbl(pg, "c16p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c16h <- tbl(pg, "c16h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

# beinhaltet region und cross section household weight
c16d <- tbl(pg, "c16d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

# beinhaltet personal cross sectional personal weight rb050, personal id rb030 sollte pb030 entsprechen
c16r <- tbl(pg, "c16r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2015

c15p <- tbl(pg, "c15p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c15h <- tbl(pg, "c15h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

# beinhaltet region und cross section household weight
c15d <- tbl(pg, "c15d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

# beinhaltet personal cross sectional personal weight rb050, personal id rb030 sollte pb030 entsprechen
c15r <- tbl(pg, "c15r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2014

c14p <- tbl(pg, "c14p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c14h <- tbl(pg, "c14h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

# beinhaltet region und cross section household weight
c14d <- tbl(pg, "c14d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

# beinhaltet personal cross sectional personal weight rb050, personal id rb030 sollte pb030 entsprechen
c14r <- tbl(pg, "c14r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2013

c13p <- tbl(pg, "c13p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c13h <- tbl(pg, "c13h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c13d <- tbl(pg, "c13d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c13r <- tbl(pg, "c13r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2012

c12p <- tbl(pg, "c12p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c12h <- tbl(pg, "c12h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c12d <- tbl(pg, "c12d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c12r <- tbl(pg, "c12r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2011

c11p <- tbl(pg, "c11p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c11h <- tbl(pg, "c11h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c11d <- tbl(pg, "c11d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c11r <- tbl(pg, "c11r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2010

c10p <- tbl(pg, "c10p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c10h <- tbl(pg, "c10h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c10d <- tbl(pg, "c10d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c10r <- tbl(pg, "c10r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2009

c09p <- tbl(pg, "c09p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c09h <- tbl(pg, "c09h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c09d <- tbl(pg, "c09d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c09r <- tbl(pg, "c09r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2008

c08p <- tbl(pg, "c08p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c08h <- tbl(pg, "c08h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c08d <- tbl(pg, "c08d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c08r <- tbl(pg, "c08r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2007

c07p <- tbl(pg, "c07p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py021g, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c07h <- tbl(pg, "c07h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c07d <- tbl(pg, "c07d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c07r <- tbl(pg, "c07r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

year <- 2006

c06p <- tbl(pg, "c06p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py020n, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c06h <- tbl(pg, "c06h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c06d <- tbl(pg, "c06d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c06r <- tbl(pg, "c06r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

names(c06p)[names(c06p) == 'py020n'] <- 'py021g'

year <- 2005

c05p <- tbl(pg, "c05p") %>%
  filter(pb020=='UK' & pb010==year) %>%
  select(pb020, pb010, pb030, pb040, pb140, pb150, py010g, py020n, py050g, py090g,py080g, 
         px010, py100g, py110g, py120g, py130g, py140g, px030) %>%
  collect(n = Inf)

c05h <- tbl(pg, "c05h") %>%
  filter(hb020=='UK' & hb010==year) %>%
  select(hb020, hb030, hy010, hy110g, hy040g, hy050g, hy060g, hy070g, hy080g,
         hy120g, hy130g, hy140g, hy090g, hx010, hx050, hx040, hb010) %>%
  collect(n = Inf)

c05d <- tbl(pg, "c05d") %>%
  filter(db020=='UK' & db010==year) %>%
  select(db010, db020, db030, db040, db090) %>%
  collect(n = Inf)

c05r <- tbl(pg, "c05r") %>% 
  filter(rb020=='UK' & rb010==year) %>%
  select(rb010, rb020, rb030, rb050, rx030) %>%
  collect(n = Inf)

names(c05p)[names(c05p) == 'py020n'] <- 'py021g'

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}
c10d <- c10d %>% mutate(db040 = mid(db040, 1, nchar(db040) -1))
c11d <- c11d %>% mutate(db040 = mid(db040, 1, nchar(db040) -1))

silc.p <- bind_rows(c05p,c06p,c07p,c08p,c09p,c10p,c11p,c12p,
                    c13p,c14p, c15p, c16p)
silc.h <- bind_rows(c05h,c06h,c07h,c08h,c09h,c10h,c11h,c12h,
                    c13h,c14h, c15h, c16h)
silc.d <- bind_rows(c05d,c06d,c07d,c08d,c09d,c10d,c11d,c12d,
                    c13d,c14d, c15d, c16d)
silc.r <- bind_rows(c05r,c06r,c07r,c08r,c09r,c10r,c11r,c12r,
                    c13r,c14r, c15r, c16r)

rm(c05p,c06p,c07p,c08p,c09p,c10p,c11p,c12p,
   c13p,c14p, c15p, c16p,c05h,c06h,c07h,c08h,c09h,c10h,c11h,c12h,
   c13h,c14h, c15h, c16h,c05d,c06d,c07d,c08d,c09d,c10d,c11d,c12d,
   c13d,c14d, c15d, c16d,c05r,c06r,c07r,c08r,c09r,c10r,c11r,c12r,
   c13r,c14r, c15r, c16r)

# Create unique IDs for merging id_h is household, id_p is person
silc.p <- silc.p %>% mutate(id_h = paste0(pb020, px030))
silc.p <- silc.p %>% mutate(id_p = paste0(pb020, pb030))

silc.h <- silc.h %>% mutate(id_h = paste0(hb020, hb030))

silc.d <- silc.d %>% mutate(id_h = paste0(db020, db030))

silc.r <- silc.r %>% mutate(id_p = paste0(rb020, rb030))
silc.r <- silc.r %>% mutate(id_h = paste0(rb020, rx030))

# Merge the datasets
silc.pd <- left_join(silc.p, silc.d, by = c('id_h' = 'id_h', 'pb010' = 'db010')) 

silc.rpd <- left_join(silc.r, silc.pd, by = c('id_p' = 'id_p', 'rb010' = 'pb010')) 

silc.hd <- left_join(silc.h, silc.d, by = c('id_h' = 'id_h', 'hb010' = 'db010'))

## Aufteilung P1 ##

# Version 1 nur Einkommen auf Haushaltsebene durch Äquivalenzfaktor geteilt
# stimmt die Berechnung oder alles inklusive der p-einkommen zusammenrechnen
# und dann durch den äquivalenzfaktor teilen?

# Aufteilung der jeweiligen Haushaltsgrößen nach Äquivalenzfaktor
silc.h1 <- silc.h %>% 
  mutate(eq.hy110g = hy110g/hx050,
         eq.h.cap.inc = (hy040g + hy090g)/hx050,
         eq.h.trans.inc = (hy050g + hy060g + hy070g + hy080g)/hx050,
         eq.h.expenses = (hy120g + hy130g + hy140g)/hx050) 

## match to r ## schauen dass das matching funktioniert, es muss nach id_p 

silc.rh <- left_join(silc.r, silc.h1, by = c('id_h' = 'id_h', 'rb010' = 'hb010')) 
silc.rhpd <- left_join(silc.rh, silc.pd, by=c('id_p' = 'id_p', 'rb010' = 'pb010'))

# bei dem vorigen merge hat es zwei mal id_h die r dann umbenennt, daher:
names(silc.rhpd)[names(silc.rhpd) == 'id_h.x'] <- 'id_h'

# Region matching needs to be based on id_h
silc.rhpd <- left_join(silc.rhpd, silc.d, by=c('id_h' = 'id_h', 'rb010' = 'db010')
                       , select=c(db040))

names(silc.rhpd)[names(silc.rhpd) == 'db020.y'] <- 'db020'
names(silc.rhpd)[names(silc.rhpd) == 'db040.y'] <- 'db040'
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

silc.p1 <- subset(silc.rhpd, select=c(id_h, id_p, work.inc, cap.inc, fac.inc, 
                                      nat.inc, disp.inc, db020, rb010, pb040, db090,
                                      db040, hy010))

# alles auf Haushaltsebene zusammenfassen neu

silc.p1 <- aggregate(cbind(work.inc, cap.inc, fac.inc, 
                           nat.inc,  disp.inc) ~ id_h + rb010 + db020 + db090, silc.p1, FUN=sum)




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
silc.p20 <- subset(silc.p, age>19)


# divide by HH size
silc.h <- silc.h %>% 
  mutate(eq.hy110g = hy110g/ hx040,
         eq.h.cap.inc = (hy040g + hy090g)/hx040,
         eq.h.trans.inc = (hy050g + hy060g + hy070g + hy080g)/hx040,
         eq.h.expenses = (hy120g + hy130g + hy140g)/hx040) 

#match            
silc.pd <- left_join(silc.p20, silc.d, by = c('id_h' = 'id_h', 'pb010' = 'db010')) 
silc.pdh <- left_join(silc.pd, silc.h, by = c('id_h' = 'id_h', 'pb010' = 'hb010')) 

# replace NAs
silc.pdh <- silc.pdh %>% replace(is.na(.), 0)

# 1.1 Einkommen aus Arbeit: py010g+py021g+py050g+hy110g ## hier noch py021g zu 
# py020n ändern
silc.pdh <- silc.pdh %>% 
  mutate(work.inc = py010g + py021g + py050g + eq.hy110g)

# 1.2 Capital income

silc.pdh <- silc.pdh %>% 
  mutate(cap.inc = eq.h.cap.inc + py080g)

# factor income

silc.pdh <- silc.pdh %>% 
  mutate(fac.inc = cap.inc + work.inc)

# Pre-tax national income

silc.pdh <- silc.pdh %>% 
  mutate(nat.inc = fac.inc + py100g + py090g)

# post-tax disposable income

silc.pdh <- silc.pdh %>% 
  mutate(disp.inc = nat.inc + eq.h.trans.inc - eq.h.expenses + 
           py110g + py120g + py130g + py140g)

# für das spezielle Jahr speichern

silc.p2 <- subset(silc.pdh, select=c(id_h, id_p, work.inc, cap.inc, fac.inc, 
                                      nat.inc, disp.inc, db020, pb040, db090,
                                      db040, pb010, hy010))

rm(silc.pdh, silc.pd, silc.p20, silc.rpd, silc.rh, 
   silc.hd, silc.h1)

# alles auf Haushaltsebene zusammenfassen 
silc.p2 <- aggregate(cbind(work.inc, cap.inc, fac.inc, 
                           nat.inc,  disp.inc) ~ id_h + pb010 + db020 + db090, silc.p2, FUN=sum)