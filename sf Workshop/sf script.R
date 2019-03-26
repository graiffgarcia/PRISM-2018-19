library(tidyverse)
library(sf)
library(rmapshaper)
library(colormap)

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

# how awkward. luckily, we have sf. so let's not think about rgdal (or sp, etc.)
# anymore for today.
rm(counties_2)

########## how the data is stored----
# that last column, called "geometry", contains the *type* of the geometry
# and the coordinates for each observation. 
# usual geometry types are POINTs, LINESTRINGs, and POLYGONs.
# in this case, we have a MULTIPOLYGON -- which means that each observation
# is a set of polygons. we can look at the coordinates for the first of these
# multipolygons:
counties$geometry[1]
counties$geometry[1][[1]]

# or, in matrix form:
counties$geometry[1][[1]][1]

# we can also use st_geometry(), one of sf's helper functions:
st_geometry(counties)[1]

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
ct_contig <- filter(counties, as.numeric(STATEFP) < 60)

plot(ct_contig, max.plot = 1)

# so, that still looks all messed up. turns out the Aleutians West Census Area
# in Alaska contains very positive and very negative longitudes, so the plot
# needs to "circle back" (insert your own flat earth joke here), which is why
# the plot includes all of that empty space.
# we can fix that by changing the projection of the map.
# changing projections is very simple in sf: unless the original file's 
# projection is in some super messed up format, all you need is a 
# Coordinate Reference System code: an integer. in this case, we're using EPSG
# 2163, the US National Atlas Equal Area projection 
# (http://spatialreference.org/ref/epsg/2163/)
ct_contig <- st_transform(ct_contig, crs = 2163)
plot(ct_contig, max.plot = 1)

########## simplifying----
# so, let's explore our data further. first, three 'housekeeping' tasks: 1) if 
# we're planning on plotting this data a bunch of times, we should *simplify*
# the geometry. sf, by way of GDAL, uses the Douglas-Peuker algorithm for 
# line simplification. the other common algorithm for this task is Visvalingam, 
# which produces much smoother-looking plots.
# see (https://github.com/mbloch/mapshaper/wiki/Simplification-Tips)
# in order to use Visvalingam, we need the ms_simplify function from the 
# rmapshaper package.
ct_simplified <- ms_simplify(ct_contig, keep = 0.01, keep_shapes = TRUE)

# with rapply ('recursive sapply', for nested lists), we can find how many
# different coordinates make up the map in the original counties data and in
# the simplified one. although we set keep = 0.01 above, ct_simplified has about
# 3.1% of the lines of ct_contig. still, that's a huge difference, and it looks 
# just fine at the resolutions we'll use.
sum(rapply(st_geometry(ct_contig), nrow))
sum(rapply(st_geometry(ct_simplified), nrow))

# second "housekeeping" task: the analysis below only uses the lower 48 states.
# so, let's remove Alaska and Hawaii for now (sorry, Alaska and Hawaii!)
ct_simplified <- filter(ct_simplified, STATE != "Alaska" & STATE != "Hawaii")

# so anyway -- given that we simplified the geometry down to 3% of the original 
# number of lines, we can now try using ggplot2! 
# so, do you have to load the maps package? do you have to finagle with
# some kind of custom coordinate syntax for the axes? do you have to give up and
# either use plot() or worse, sell your soul to ESRI for an ArcGIS license? 
# well, no... just use geom_sf().
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
# by the way: note how easy it is to open .gdb files with st_read()!
cities <- st_read(file.path('~/Documents/GitHub/PRISM-2018-19/sf Workshop',
                            'USA_Major_Cities/v106/cities.gdb'),
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
# a weird-looking azymuthal projection:
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
# 0s and 1s; by taking the rowSums and then using the resulting vector to filter
# cities, we keep only the cities that are inside one of the features of
# ct_simplified, aka, within the contiguous United States.
cities <- {st_within(cities, ct_simplified, sparse = FALSE) %>% rowSums > 0} %>% 
  filter(cities, .)

# and now we can merge the data together. how?
# idea 1: how about merging the information from cities to counties based on
# whether a county polygon *contains* a city point? we can do that like this:
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
# a county and a large-ish city? 
# we can get a distance matrix with st_distance.
# let's subset our cities data first, to only places with 100,000+ population:
large_cities <- filter(cities, POP2010 >= 100000)
ct_distances <- st_distance(ct_simplified, large_cities)

# with the matrix of distances, we can get both 1) the distance itself; 2) the
# index of the closest city.
ct_simplified$min_distance <- apply(ct_distances, 1, min)
ct_simplified$closest_city <- apply(ct_distances, 1, which.min)

# we can then extract from large_cities and then plot, for example... the
# proportion of Hispanic residents in the city closest to each county in the US?
ct_simplified$prop_hispanic <- map_dbl(ct_simplified$closest_city, 
                          ~ cities$HISPANIC[.x]/cities$POP2010[.x])

# which we can plot...
ggplot(ct_simplified) + geom_sf(aes(fill = prop_hispanic), size = 0.2,
                                color = "#f5f5f288") +
  geom_sf(data = state_borders, color = "#f5f5f2", size = 0.5) +
  geom_sf(data = large_cities, color = "#93ff00", size = 1.6) +
  theme_map() + 
  scale_fill_colormap(colormap = "magma")

# now, this may be very important to you in the future -- when you calculate the 
# distance between a polygon and a point, what does that mean? 
# are we using the polygons' centroids? we can test that! st_centroid will
# give us the centroids of polygons (or lines)...
ct_centroids <- st_centroid(ct_simplified)

# and we can see if the distances between these centroids & our points are equal
# to the distance matrix we found previously:
all.equal(st_distance(ct_centroids, large_cities), ct_distances)

# so they're not the same. I found on StackExchange a really clever way of
# demonstrating how st_distance is calculated: we can use the st_point() and
# st_polygon() functions to create a point and a square:
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

# since we have that dataset of centroids, there's one last thing I wanna show
# you before we move to using spatial data in your analyses. let's create a
# dataset of centroid coordinates and add county names:
county_coords <- as_tibble(cbind(ct_simplified$NAME, 
                                 st_coordinates(ct_centroids))) %>% 
  mutate_at(2:3, as.numeric)

# st_as_sf() converts other object types to sf, and you can pass the names
# or indices of columns containing coordinates in numeric form to it!
county_coords_sf <- st_as_sf(county_coords, coords = c("X", "Y"))

# and we can plot the new data just to make sure everything is in place:
ggplot(county_coords_sf) + geom_sf() +
  theme_map()

#### executions by county----
cens <- readRDS(file.path('~/Documents/GitHub/PRISM-2018-19',
                                  'sf Workshop/census2010.Rds'))
cens

res2012 <- readRDS(file.path('~/Documents/GitHub/PRISM-2018-19',
                             'sf Workshop/results2012.Rds'))
res2012

ct_simplified <- ct_simplified %>% 
  mutate(STATEFP = as.numeric(STATEFP), 
         fips = as.numeric(paste0(STATEFP, COUNTYFP))) %>% 
  left_join(., cens, by = "fips") %>% 
  left_join(., res2012, by = c("fips" = "combined_fips"))

ggplot(ct_simplified) + 
  geom_sf(aes(fill = per_dem_2012), size = 0, color = "#f5f5f200") +
  geom_sf(data = state_borders, size = 0.5, color = "#f5f5f2") +
  theme_map() + 
  scale_fill_colormap(colormap = "bluered", reverse = TRUE, discrete = FALSE)



summary(glm(per_dem_2012 ~ TOT_POP + I(TOT_FEMALE/TOT_POP) + 
              I(BA_MALE/TOT_POP),
    data = ct_simplified))


#### neighbors----
nbrs <- st_intersects(ct_contig, ct_contig)
nbrs_fips <- map(nbrs, ~ slice(ct_contig, .x))


ggplot(ct_simplified) + 
  geom_sf(fill = "grey80", size = 0.1, color = "#f5f5f2bb") +
  geom_sf(data = ct_simplified[nbrs[[50]],], aes(fill = NAME))


