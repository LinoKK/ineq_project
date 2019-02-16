#Code for a spatial analysis and corresponding graphical depiction

install.packages(c("maptools", "rgdal", "spdep", "gstat", "MASS",
                   "lattice", "nlme", 
                   "GeoXp", "spectral",
                   "dplyr", "GISTools", "maptools", "RColorBrewer",
                   "ggplot2",
                   "grid",
                   "varhandle","magrittr","geom_map","maps","viridis",
                   "ggthemes",
                   "ggalt",
                   "rgeos",
                   "graphics",
                   "grDevices","raster","gtable"))



packages<-list("maptools", "spdep", "rgdal","gstat", "MASS", 
               "lattice", "nlme", "GeoXp",  "spectralGP",
               "dplyr", "GISTools", "maptools", "RColorBrewer",
               "ggplot2",
               "grid",
               "varhandle","magrittr","geom_map","maps","viridis",
               "ggthemes",
               "ggalt",
               "rgeos", 
               "graphics", 
               "grDevices","raster","gtable")

lapply(packages, require, character.only)

library("spdep")
library("maptools")
library("dplyr")
library("readr")
library("rgdal")

#----------------------------

#Loading in the shapefile

#The Shapefile contains information about borders and names of regions and can be merged with data to allow regressions.
#It can also be used to depict certain things (i.e. income levels, gini, voting outcomes) on the map.

#shp<-readOGR("reports/GBR/shape_UK/SG_NUTSLevel2_2008.shp")
#shp<-readOGR("reports/GBR/shape_UK/NUTS_RG_01M_2013_3035_LEVL_1.shp")
#shp<-readOGR("reports/GBR/shape_UK/European_Electoral_Regions_December_2015_Full_Clipped_Boundaries_in_Great_Britain.shp")
shp<-readOGR("reports/GBR/shape_UK/NUTS_Level_2_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")

nuts <- read_csv("~/ineq_project/reports/GBR/tables/nuts.csv")
nuts2 <- read_csv("~/ineq_project/reports/GBR/tables/nuts2.csv")

#merge by region codes
shp1<-merge(shp, nuts, all.y=FALSE, all.x=FALSE, by.x="nuts218cd", by.y="as.factor(Region)")

#merge by region names
shp2<-merge(shp, nuts2, all.y=FALSE, all.x=FALSE, by.x="nuts218nm", by.y="Region Name")

#These mergers leave us without Scotland, London and the region Surry, East and West Sussex as there were NUTS2
#classification changes in the meantime, apparently.

#plot(shp, border="grey20")
plot(shp1, border="grey20")
#plot(shp2, border="grey20")
coords<-coordinates(shp1)

summary(shp1$nuts218nm)

#Defining neighbours

#For spatial regressions, we need to define which regions neighbour each other. There are different variants, but
#since Great Britain consists of Islands and therefore (possibly) not all regions have at least one direct neighbour
#(necessary so the weight matrix can be inverted), we define the 3 closest regions as neighbours.
#Regression results can vary depending on the chosen method!

#k-nearest; K=3
knear5_nb<-knn2nb(knearneigh(coords, k=3, longlat = FALSE), sym=F)
plot(knear5_nb, coords, add=TRUE, col="green", cex=0.5)

summary(shp)

#Spatial weights matrix
W.list<-nb2listw(neighbours = knear5_nb, style="W")

#Testing for spatial autocorellation - Moran's I

#In "Leave" Vote
moran.test(shp1@data$gini.reg, listw=W.list, alternative = "greater")
moran.plot(shp1@data$gini.reg, listw=W.list)

#"Leave" votes appear to have positive spatial autocorrelation. 
#For every percentage point of leave votes in neighbouring regions, leave votes are expected to increase by 0.067 in
#the region in question. Not statistically significant at P = 0.157.

#In Mean Income
moran.test(shp1@data$mean.reg, listw=W.list, alternative = "greater")
moran.plot(shp1@data$mean.reg, listw=W.list)

#As expected, mean income is strongly and significantly spatially correlated.
#For every increase of mean income by 1000 in neighbouring regions, mean income is expected to rise by 328 in the
#specified region. Significant at the 1% level (P = 0.002).

#### ----------- Spatial Error Model -----------------

spatial.gini.reg.nuts<-errorsarlm(Leave ~ gini.reg, 
                  listw=W.list, method="eigen", 
                  data=shp1@data)

summary(spatial.gini.reg.nuts)

#Coefficient for gini is significant at the 5% level (P = 0.012) and negative with -0.49.
#Therefore (due to the scaling we use) an increase of the Gini by 100 percentage points would reduce 
#the percentage of "Leave" votes by 49%. More reasonably, an increase of the Gini by 1 percentage point
#leads to a decrease in "Leave" votes by 0.49 percentage points.

spatial.gini.reg.nuts_nr2<-errorsarlm(Leave ~ gini.reg+mean.reg, 
                                  listw=W.list, method="eigen", 
                                  data=shp1@data)

summary(spatial.gini.reg.nuts_nr2)

#When mean regional income is added, gini is only significant anymore at just under the 10% level, but still negative
#and similar in size, at -0.39. The coefficient for mean income is not significant, negative and very small in size.

spatial.med.reg.nuts<-errorsarlm(Leave ~ median, 
                                  listw=W.list, method="eigen", 
                                  data=shp1@data)

summary(spatial.med.reg.nuts)

#not even close to being significant at P = 0.515.

spatial.mean.reg.nuts<-errorsarlm(Leave ~ mean, 
                                  listw=W.list, method="eigen", 
                                  data=shp1@data)    

summary(spatial.mean.reg.nuts)

#Significant at barely above 5% (P = 0.054). Negative coefficient of -8.3520e-06. 
#Therefore, an increase in income by 1000 leads to a decrease in "Leave votes" by 0.0083.
#This is a obviously a much larger value than in the regular OLS regression, being almost twice as large.
#Though not significant at 5%, it is very close.


#Spatial maps

library("GISTools")

shades<-auto.shading(shp1@data$mean, n=5, cols = brewer.pal(5,"Greens"))
choropleth(shp1, shp1@data$mean, shades)
choro.legend(2,57, sh=shades, title="Mean Income NUTS 2", cex=0.75)
#north.arrow(-4,60,0.2, col="red")

shades<-auto.shading(shp1@data$gini, n=5, cols = brewer.pal(5,"Blues"))
choropleth(shp1, shp1@data$gini, shades)
choro.legend(2,57, sh=shades, title="Gini NUTS 2", cex=0.75)
#north.arrow(-4,60,0.2, col="red")

shades<-auto.shading(shp1@data$Leave, n=5, cols = brewer.pal(5,"Reds"))
choropleth(shp1, shp1@data$Leave, shades)
choro.legend(2,57, sh=shades, title="Leave % NUTS 2", cex=0.75)
#north.arrow(-4,60,0.2, col="red")


#---------------------------------------------------------------------------------
#Code for straightforward regression analysis (by Lino) - Used as a reference
#---------------------------------------------------------------------------------

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

nuts2.11 <- nuts2.11 %>% 
  mutate(gini.reg = gini,
         log.leave = log (Leave),
         mean.reg = mean,
         median.reg = median,
         leave100 = Leave*100,
         log.gini = log(gini))

library(ggplot2)

qplot(x=Leave, y=gini.reg, data=nuts2.11, geom="point")
qplot(x=Leave, y=mean.reg, data=nuts2.11, geom="point")

nuts2.11.table <- select(nuts2.11, "Region Name", "gini", "mean", "median", "Remain", "Leave")

write.csv(nuts2.11.table, file = "reports/GBR/tables/nuts2.11.csv",row.names=FALSE)
write.csv(nuts2.11, file = "reports/GBR/tables/nuts.11.csv",row.names=FALSE)

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

