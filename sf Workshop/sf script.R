library(tidyverse)
# if sf doesn't load because you don't have the group_map function, 
# ***update dplyr!***
library(sf)
# we're only using rmapshaper once today -- but it's a very useful package! in
# our case, we only want it for the ms_simplify() function
library(rmapshaper)
# colormap has a very nice set of palettes; I use it here as a ggplot2 helper.
library(colormap)
# these are for generalized additive models and for generalized least squares.
library(mgcv)
library(nlme)
# this is just for me; it loads a couple helper functions, most importantly the
# theme_map() function that's just a modified version of the theme_map() from 
# the ggthemes package.
# source("~/Code/rr/R/diffr.R")

##########
# sf is a relatively new R package (accepted to CRAN in Oct. 2016, but still
# actively in development -- ) that makes spatial analysis in R 
# *much* more efficient, commonsensical, and syntactically coherent.

# my plan for today: we'll work with one dataset that contains spatial features,
# and in the process we can explore the different features of sf, see how sf 
# helps us solve common problems and accomplish common tasks, and then explore
# how we can use spatial features data for regression modeling.

########## loading data & rgdal comparison----
### first: let's look at the sf data format, and what these datasets look like.
# the usual way you'll obtain spatial data is in *shapefiles*. those will come
# in a collection of files with the same name but different extensions
# (usually shp, sbx, snn, prj, cpg, & dbf). loading the .shp file is enough
# to open all the information in the shapefile. we can read a shapefile in sf
# with st_read:
counties <- st_read(file.path('~/Documents/GitHub/PRISM-2018-19/sf Workshop',
                              'counties/counties.shp'),
                    stringsAsFactors = FALSE)

# let's look at the data:
counties

# an aside: what does this dataset look like when opened with rgdal, another
# R package for working with spatial data? let's try:
counties_2 <- rgdal::readOGR(file.path('~/Documents/GitHub/PRISM-2018-19',
                                       'sf Workshop/counties'))

# let's print the data to the console again:
counties_2

# ...oh man. okay. that sucks.

# but the objects contain the exact same data, right? 
# are they even the same size?
paste0("The sf version of 'counties' is ", 
       format(object.size(counties), "Mb"), ".")
paste0("The rgdal version of 'counties' is ", 
       format(object.size(counties_2), "Mb"), ".")

# how awkward. so that's reason #1 why sf is the best. 
# let's not think about rgdal (or sp, etc.) anymore for today.
rm(counties_2)

########## how the data is stored----
# that last column, called "geometry", contains the *type* of the geometry
# and the coordinates for each observation. 
# usual geometry types are POINTs, LINESTRINGs, and POLYGONs.
# in this case, we have a MULTIPOLYGON -- which means that the objects are
# two-dimensional (like polygons), but they don't have to be contiguous like
# polygons do (think, for example, of the shapes of the states of Hawaii or
# Michigan). we can access the geometries of these multipolygons like we would
# any other column in a data frame:
counties$geometry[1]

# but each of these observations is a list, so we can obtain the full 
# coordinates by further subsetting the list:
counties$geometry[1][[1]]

# or, in matrix form:
counties$geometry[1][[1]][1]

# we can also use st_geometry(), one of sf's helper functions:
st_geometry(counties)[1]

# ...or st_coordinates() -- harder to subset to the first observation, though:
st_coordinates(counties) %>% as.data.frame %>% subset(L3 == 1)

########## plotting and subsetting----
# obviously, looking at a list of coordinates is not a good way to see what our
# dataset looks like. we can plot the dataset with the default plot()
# function (we'll use ggplot in a second):
plot(counties, max.plot = 1)

# ...well that's stupid. turns out our shapefile includes not only the 
# contiguous US, Alaska, and Hawaii, but also: Puerto Rico, American Samoa,
# Guam, Marshall Islands, Palau, the U.S. Virgin Islands, 
# the U.S. Minor Outlying Islands, Northern Mariana Islands, and 
# the Federated States of Micronesia. but our dataset only includes 
# the 50 states and DC. let's subset the data with filter(), 
# based on the STATEFP variable:
counties <- filter(counties, as.numeric(STATEFP) < 60)

plot(counties, max.plot = 1)

# so, that still looks all messed up. turns out the Aleutians West Census Area
# in Alaska contains very high positive and very high negative longitudes, 
# so the plot needs to "circle back" (insert your own flat earth joke here), 
# which is why the plot includes all of that empty space.
# we can fix that by changing the projection of the map.
# changing projections is very simple in sf: unless the original file's 
# projection is in some super messed up format, all you need is a 
# Coordinate Reference System code: an integer. in this case, we're using EPSG
# 2163, the US National Atlas Equal Area projection. it's an azimuthal 
# projection -- in short, it projects the globe onto a disk, with every distance
# from the central point preserved.
# (http://spatialreference.org/ref/epsg/2163/)
counties <- st_transform(counties, crs = 2163)
plot(counties, max.plot = 1)

########## simplifying----
# so, let's explore our data further. first, three 'housekeeping' tasks.
# first: the analysis below only uses the lower 48 states.
# so, let's remove Alaska and Hawaii for now (sorry, Alaska and Hawaii!)
counties <- filter(counties, STATE != "Alaska" & STATE != "Hawaii")

# second: if we're planning on plotting this data a bunch of times, we should 
# *simplify* the geometry. sf, by way of GDAL, uses the Douglas-Peuker 
# algorithm for line simplification. the other common algorithm for this task is
# Visvalingam, which produces much smoother-looking plots.
# see (https://github.com/mbloch/mapshaper/wiki/Simplification-Tips)
# in order to use Visvalingam, we need the ms_simplify function from the 
# rmapshaper package.
ct_simplified <- ms_simplify(counties, keep = 0.01, keep_shapes = TRUE)

# with rapply ('recursive sapply', for nested lists), we can find how many
# different coordinates make up the map in the original counties data and in
# the simplified one. although we set keep = 0.01 above, ct_simplified has about
# 3% of the lines of counties. still, that's a huge difference, and it looks 
# just fine at the resolutions we'll use.
sum(rapply(st_geometry(counties), nrow))
sum(rapply(st_geometry(ct_simplified), nrow))

# so anyway -- given that we simplified the geometry down to 3% of the original 
# number of lines, we can now try using ggplot2, and our plots will build in a
# timely fashion! so, do you have to load the maps package? do you have to use
# the fortify() function to transform our map object into a gigantic data frame?
# do you have to finagle with some kind of custom coordinate syntax for the 
# axes? do you have to give up and use plot()? or worse, sell your soul to ESRI 
# for an ArcGIS license? well, no... just use geom_sf().
p1 <- ggplot(ct_simplified) + 
  geom_sf(aes(fill = STATE), size = 0.2, color = "#f5f5f2bb") +
  theme_map() + 
  scale_fill_colormap(guide = FALSE, colormap = "copper", discrete = TRUE)
p1

# and this brings us to the third task before we move on: if we're plotting
# counties, it's probably a good idea to overlay state boundaries, in slightly
# thicker lines. we can create a data frame of states *very* easily with sf 
# by using group_by and summarize, and then using st_boundary to turn the
# polygons into LINESTRINGs or MULTILINESTRINGs.
state_borders <- ct_simplified %>% group_by(STATE) %>% 
  summarize(state = first(STATE)) %>% select(-state) %>% st_boundary

# and we can use this object in ggplot.
p1 + geom_sf(data = state_borders, size = 0.5, color = "#f5f5f2")

########## merging----
# so, for no reason at all, I want to merge my spatial dataset with another 
# spatial dataset that has demographic information on U.S. towns and cities,
# which I took from the ArcGIS free data site.
# (https://www.arcgis.com/home/item.html?id=85d0ca4ea1ca4b9abf0c51b9bd34de2e)
# ESRI's files are all in a .gdb format, which is very easy to open with 
# st_read():
cities <- st_read(file.path('~/Documents/GitHub/PRISM-2018-19/sf Workshop',
                            'USA_Major_Cities/cities.gdb'),
                  layer = "cities")

# cities has 3886 observations, all POINTs. 
cities

# another aside: note how the CRS of cities is different from what we set 
# for 'counties' (EPSG 4326 vs. EPSG 2163):
st_crs(cities)

# this means that if we want to plot these points on top of ct_simplified with
# "add = TRUE", this happens:
plot(ct_simplified, max.plot = 1)
plot(cities, cex = 2, col = "black", add = TRUE)

# so... nothing. what happens with ggplot2?
p1 + geom_sf(data = cities, aes(color = as.factor(POP_CLASS), 
                                size = POP2010/1e+6), alpha = 0.8) +
  scale_color_colormap(colormap = "summer", guide = FALSE, discrete = TRUE) +
  guides(size = "none")

# I really, really love this feature -- geom_sf() silently runs st_transform() 
# on every sf object so that they match the CRS of the *first* sf object on your
# plot. 
# ggplot also allows you to change the projection of the plot on the fly, with
# coord_sf(), which also takes EPSG codes or proj4 strings. for example, here's
# a weird-looking azimuthal projection (Lambert azimuthal equal-area), in
# proj4 format:
p1 + coord_sf(crs = paste('+proj=laea +lat_0=-10 +lon_0=-70 +x_0=0 +y_0=0',
                          '+a=6370997 +b=6370997 +units=m +no_defs'))


# we'll go back to ggplotting this stuff in a second -- for now, let's think
# about ways in which we can merge these two datasets together. 
# here, we can't silently st_transform() stuff, so let's just transform cities:
cities <- st_transform(cities, crs = st_crs(ct_simplified))

# we have to remove Alaska and Hawaii, so let's use the opportunity to figure
# out another way to remove features from sf objects:
# we can find which points in cities are inside of ct_simplified with the 
# *st_within* function. with sparse = FALSE, st_within will give us a matrix of
# TRUEs and FALSEs; by taking the rowSums and then using the resulting vector 
# in the filter() function, we keep only the cities that are inside one of the 
# features of ct_simplified, aka, within the contiguous United States.
cities_within_matrix <- st_within(cities, ct_simplified, sparse = FALSE)
cities <- cities_within_matrix %>% rowSums %>% as.logical %>% 
  filter(cities, .)

# and now we can merge the data together. how?
# idea 1: how about merging the information from cities to counties based on
# whether a county polygon *contains* a city point? we can do that very
# simply, with st_join:
# (by the way: st_contains(a, b) is the exact same as st_within(b, a))
ct_plus_cities <- st_join(ct_simplified, cities, join = st_contains)

# this is sort of awkward, but it works. the problem is that each county may
# contain several cities:
as.data.frame(ct_plus_cities) %>% group_by(COUNTYNS) %>% 
  summarize(n = n()) %>% arrange(desc(n))

# it's an easy fix, though: if we want county-level data, 
# we can just group_by county and summarize the city-level variables:
ct_plus_cities <- ct_plus_cities %>% group_by(COUNTYNS) %>% 
  summarize_at(18:57, sum, na.rm = TRUE)

# the problem, of course, is that we lose the character variables; a proper 
# solution is certainly much more convoluted... especially because when we plot
# the data, we see just how many counties do not contain any towns/cities:
ggplot(ct_plus_cities) + 
  geom_sf(aes(fill = POP2010 > 0), size = 0.2, color = "#f5f5f2bb") +
  theme_map() +
  scale_fill_colormap(colormap = "viridis", discrete = TRUE)

# so we can do something else: how about instead of plotting the total 
# population of a county's towns & cities, we find the shortest distance between
# a county and a large-ish city? we can get a distance matrix with st_distance.
# let's subset our cities data first, to only places with 100,000+ population:
cities <- filter(cities, POP2010 >= 100000)
ct_distances <- st_distance(ct_simplified, cities)

# with the matrix of distances, we can get both 1) the distance itself; 2) the
# index of the closest city.
ct_simplified$min_distance <- apply(ct_distances, 1, min)
ct_simplified$closest_city <- apply(ct_distances, 1, which.min)

# we can then extract from cities and then plot, for example... the proportion 
# of Hispanic residents in the city closest to each county in the US?
ct_simplified$prop_hispanic <- map_dbl(ct_simplified$closest_city, 
                                       ~ cities$HISPANIC[.x]/cities$POP2010[.x])

# which we can plot...
ggplot(ct_simplified) + geom_sf(aes(fill = prop_hispanic), size = 0.2,
                                color = "#f5f5f288") +
  geom_sf(data = state_borders, color = "#f5f5f2", size = 0.5) +
  geom_sf(data = cities, color = "#93ff00", size = 1.6) +
  theme_map() + 
  scale_fill_colormap(colormap = "YIOrRd")

# now, this may be very important to you in the future -- when you calculate the 
# distance between a polygon and a point, what does that mean? 
# are we using the polygons' centroids? we can test that! st_centroid will
# give us the centroids of polygons (or lines)...
ct_centroids <- st_centroid(ct_simplified)

# and we can see if the distances between these centroids & our points are equal
# to the distance matrix we found previously:
all.equal(st_distance(ct_centroids, cities), ct_distances)

# so they're not the same. I found on (and stole from) StackExchange a really
# cool way of demonstrating how st_distance is calculated: we can use the 
# st_point() and st_polygon() functions to create a point and a square by hand:
point <- st_point(c(0,0))
point
polyg <- st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))))
polyg

# with st_sfc(), we can create a "geometry list column", aka the "geometry"
# column we've seen in our sf data frames:
st_sfc(point, polyg)

# and when calculating the distance between the point & the polygon, we'll see
# that st_distance() reports the distance between the point and
# the *closest* point (edges or vertices) in the polygon:
st_sfc(point, polyg) %>% st_distance

# by the way: since we have that dataset of centroids, there's one last thing I
# wanna show you before we move to using spatial data in your analyses. let's 
# create a dataset of centroid coordinates and add county names:
county_coords <- as_tibble(cbind(ct_simplified$NAME, 
                                 st_coordinates(ct_centroids))) %>% 
  mutate_at(2:3, as.numeric)

# this is not an sf object:
class(county_coords)

# but we can make it an sf object with st_as_sf(). it converts other object 
# types to sf, and you can pass the names or indices of columns containing 
# coordinates to it!
county_coords_sf <- st_as_sf(county_coords, coords = c("X", "Y"))

# and we can plot the new data just to make sure everything is in place:
ggplot(county_coords_sf) + geom_sf() +
  theme_map()

########## an example of modeling spatially autocorrelated data----
# first -- we load a dataset with county-level variables from the 2010 Census, 
# which I assembled from various Census datasets.
cens <- readRDS(file.path('~/Documents/GitHub/PRISM-2018-19',
                          'sf Workshop/census2010.Rds'))
cens

# then, a dataset with county-level presidential election results
res2012 <- readRDS(file.path('~/Documents/GitHub/PRISM-2018-19',
                             'sf Workshop/results2012.Rds'))
res2012

# the counties's FIPS codes can be used to merge these two datasets to the sf 
# object:
ct_simplified <- ct_simplified %>% 
  mutate(STATEFP = as.numeric(STATEFP), 
         fips = as.numeric(paste0(STATEFP, COUNTYFP))) %>% 
  left_join(., cens, by = "fips") %>% 
  left_join(., res2012, by = c("fips" = "combined_fips"))

# here's the outcome variable: percentage of vote for Obama in 2012
ggplot(ct_simplified) + 
  geom_sf(aes(fill = per_dem_2012), size = 0, color = "#f5f5f200") +
  geom_sf(data = state_borders, size = 0.5, color = "#f5f5f2") +
  theme_map() + 
  scale_fill_colormap(colormap = "bluered", reverse = TRUE, discrete = FALSE)

# and a simple model -- built with gam() but with exactly equal results to 
# an lm(). the predictors are: total population in 1000s; 
# % of county population that is female; % of county population that is white;
# % of county population that is Hispanic/Latino; 
# % living below poverty rate; 
# % without health insurance(that 'UI' is for uninsured);
# a dummy for whether the county was predominantly urban 
# (it says 2013 but the data is from 2010); unemployment rate in 2010.
m0 <- gam(per_dem_2012 ~ I(TOT_POP/1e+3) + TOT_FEMALE + WA_ + H_ +
            POV_RATE + PCT_UI + Metro_2013 + Unemployment_rate_2010,
          data = ct_simplified)
summary(m0)
gam.check(m0)

# first thing: how do we even know if there's spatial autocorrelation here?
# the general formula is: find neighbors for each county, spatially lag your
# variable of interst, and test for spatial correlation with Moran's I, which
# can be done by hand pretty easily with sf data.
# 1) to create a list of counties that 'neighbor' each of the
# counties in our data, we can use st_touches() between our 
# dataset and itself. in this case, just to be safe, I'll use the 
# non-simplified data:
nbrs <- st_touches(counties, counties)

# (this takes kind of a while, so I saved the object & load it here)
nbrs <- readRDS("~/Desktop/nbrs.Rds")

# we can use nbrs (which, as a sparse matrix, is a list of integers that
# correspond to each county's neighbors) to create a big list containing one
# data frame for county, with all its neighbors:
nbrs_df <- map(nbrs, ~ slice(ct_simplified, .x))

# here, we use these data frames to create a spatially lagged per_dem_2012.
# in this case, we're taking the *mean* of all the neighbors' values, which is
# equivalent to saying that our spatially lagged variable gives the same weight
# to each neighbor.
moran_df <- map_dfr(nbrs_df,
                    ~ as.data.frame(.x) %>%
                      summarize(per_nbrs = mean(per_dem_2012, 
                                                na.rm = TRUE))) %>% 
  bind_cols(per_dem = ct_simplified$per_dem_2012)

# so, Moran's I statistic is the coefficient of the linear regression model of
# per_dem on per_nbrs:
moranistatistic <- coef(lm(per_nbrs ~ per_dem, data = moran_df))[2]

# and we can create a simulation-based significance test by shuffling the
# values of per_dem, and then creating a spatial lagged variable with these
# random values of per_dem, but the actual neighbors list. if we do this 
# a bajillion times, we should have a distribution of values of Moran's I under
# the null hypothesis of no spatial autocorrelation:
morani <- vector(length = 500, mode = "double")
for(i in 1:500){
  per_dem <- sample(ct_simplified$per_dem_2012, replace = FALSE)
  per_nbrs <- map_dbl(1:length(per_dem), ~ mean(per_dem[nbrs[[.x]]]))
  morani[i] <- coef(lm(per_nbrs ~ per_dem))[2]
}

qplot(morani) + geom_vline(xintercept = moranistatistic, color = "red")

# so in this case -- totally unsurprisingly -- the variable is spatially 
# autocorrelated. there are two common fixes for this. the first is to use a
# smoothing spline of latitude and longitude in a GAM. this is not directly
# controlling for spatial autocorrelation (that's fix #2), but it does get rid
# of autocorrelation. for the spline, let's extract the coordinates of the
# centroids, in latitude and longitude form:
ct_simplified <- st_transform(ct_centroids, 4326) %>%
{mutate(ct_simplified, lat = st_coordinates(.)[,1],
        long = st_coordinates(.)[,2])}

# we can recreate the model from before, but with a thin plate spline for lat
# and long:
m1 <- gam(per_dem_2012 ~ I(TOT_POP/1e+3) + TOT_FEMALE + WA_ + H_ +
            POV_RATE + PCT_UI +
            Metro_2013 + Unemployment_rate_2010 +
            s(lat, long, bs = "tp", k = 100),
          data = ct_simplified)
summary(m1)
gam.check(m1)

# GAMs with splines generate different kinds of really cool plots, which allow
# you to visualize the spatial effects, but also the wiggliness" of the smoother
plot(m1, scheme = 0)
plot(m1, scheme = 1)
plot(m1, scheme = 2)
plot(m1, scheme = 3)

# as promised two seconds ago -- the second fix for spatial autocorrelation is
# to use a generalized least squares model, in which we can directly model 
# the autocorrelation structure of the residuals. with the gls() function of the
# nlme package, we can explicitly tell the model the autocorrelation structure:
m2 <- gls(per_dem_2012 ~ I(TOT_POP/1e+3) + TOT_FEMALE + WA_ + H_ +
            POV_RATE + PCT_UI +
            Metro_2013 + Unemployment_rate_2010,
          data = ct_simplified,
          correlation = corGaus(form = ~ lat + long),
          na.action = na.omit)

# this also takes a really long time to estimate, so...
m2 <- readRDS("~/Desktop/m2.Rds")
summary(m2)

########## using neighbors to directly model an effect of interest----
# so, we have nbrs_df -- a list of data frames with the neighboring counties
# of each of our observations. we can use that for more than just correcting
# spatial nuisances. for instance, what if we want to know whether the 
# difference between the Hispanic populations of a county and its neighbors
# affect a county's vote for Obama? we can find spatial lags of H_ the same way
# we did for per_dem, and use that in a model:
ct_simplified <- map_dfr(nbrs_df,
                         ~ slice(.x, -1) %>% 
                           summarize(H_NBRS = mean(H_, na.rm = TRUE)) %>% 
                           as.data.frame %>% select(1)) %>% 
  bind_cols(ct_simplified, .)

m3 <- gam(per_dem_2012 ~ I(TOT_POP/1e+3) + TOT_FEMALE + WA_ + H_ +
            POV_RATE + PCT_UI + I(H_NBRS - H_) +
            Metro_2013 + Unemployment_rate_2010,
          data = ct_simplified)
summary(m3)

########## one last thing: using DE9IM codes in sf----
# the dimensionally extended 9-intersection model (DE9IM) framework is awesome,
# and it allows you to extend the predicate versions we've seen here 
# (like st_contains or st_touches). you can specify almost any kind of spatial
# relationship between two shapes with a 9-character code, and the overarching
# idea behind DE9IM is that every spatial object has an exterior, an interior, 
# and a boundary -- and by specifying the relationships between these three
# elements, we can figure out how two shapes relate. for a more in-depth 
# explanation, I recommend this article:
# http://postgis.net/workshops/postgis-intro/de9im.html

# anyway, sf allows us to extract DE9IM codes with st_relate:
st_relate(cities[1,], ct_simplified[9,])

# and, using DE9IM codes, we can specify our own predicate. for example, here
# I want a point inside a polygon:
cities_matrix2 <- st_relate(cities, ct_simplified, pattern = "0*****212", 
                            sparse = FALSE)
all.equal(cities_matrix2, st_within(cities, ct_simplified, sparse = FALSE))

# here, I want "queen" neighbors:
queens <- st_relate(counties, counties, pattern = "F***T****")
all.equal(queens, nbrs)

# and "rook" neighbors (the difference is that T includes points AND lines,
# while 1 only counts lines)...
rooks <- st_relate(counties, counties, pattern = "F***1****")

# an example of the difference:
plot(counties[rooks[[32]],], max.plot = 1)
plot(counties[queens[[32]],], max.plot = 1)
