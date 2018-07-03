# Filename: startup_locations.R (2018-06-16)
#
# TO DO: Visualize temporal development of Israelian start-ups
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. VISUALIZATION
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("mapview")
library("tmap")
library("readxl")
library("dplyr")
library("raster")
library("sf")
library("lattice")
library("latticeExtra")
library("RPostgreSQL")

# attach data
conn = dbConnect(drv = PostgreSQL(), 
                 user = "jannes",
                 dbname = "qual_gis", 
                 port = 5432,
                 password = "incommunicado")
RPostgreSQL::dbListTables(conn)
# startup locations
su = st_read(conn, query = "select * from israel.startups")
# Israel/Palestine admin polygons
isr = st_read(conn, query = "select * from israel.israel")
dbDisconnect(conn)

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# tree start-up phases
# 1: -1989; 2: 1990-2007; 3: 2008-2018
su$phase = cut(su$founded, breaks = c(0, 1989, 2007, 2018), 
               labels = c(1989, 2007, 2018))
# check
filter(su, founded == 1989) %>% dplyr::select(founded, phase)  # looks good

# aggregate points into a raster of resolution 10 km
b_box = st_bbox(su)
r = raster(xmn = floor(b_box$xmin), xmx = ceiling(b_box$xmax),
           ymn = floor(b_box$ymin), ymx = ceiling(b_box$ymax), res = 10000,
           crs = st_crs(su)$proj4string)
r_1 = rasterize(filter(su, phase == 1989), r, field = "id", fun = "count")
# r_1[is.na(r_1)] = 0
r_2 = rasterize(filter(su, phase == 2007), r, field = "id", fun = "count")
# r_2[is.na(r_2)] = 0
r_3 = rasterize(filter(su, phase == 2018), r, field = "id", fun = "count")
# r_3[is.na(r_3)] = 0
# put everything into a raster stack and trim outer NAs
s = trim(stack(list(r_1, r_2, r_3)))

#**********************************************************
# 3 VISUALIZATION------------------------------------------
#**********************************************************

# define fisher-jenking class intervals
c_7 = classInt::classIntervals(values(r_3), n = 7, style = "fisher")
cuts = c_7$brks
pal = RColorBrewer::brewer.pal(7, "YlOrRd")
#plot(trim(r_1), col = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"))
#dev.off()

# names(s) = c(expression("1901-1997"), "1998-2010", "2011-2018")
# does not works as wanted -> transformed to X1901..1997
p_1 = spplot(s, col.regions = pal, between = list(x = 0.5),
       colorkey = list(space = "right",
                       # labels specifies what to plot
                       # at specifies where to plot the labels
                       labels = list(labels = c(0, cuts[-1]),
                                     at = cuts, cex = 0.7),
                       # at is needed again, to indicate where the 
                       # colors change (must be of length 1 more than 
                       # the col vector!!!)
                       at = cuts,
                       # width and heigt are relative to the plot
                       width = 1, height = 1,
                       # draw a box and ticks around the legend
                       axis.line = list(col = "black")),
       # at COMMAND AGAIN NECESSARY AS WE PLOT A CONTINOUS VARIABLE!!!
       at = cuts, pretty = TRUE,
       strip = strip.custom(factor.levels = 
                              c("<=1989", "1990-2007", "2008-2018")),
       sp.layout = list(
         list("sp.polygons", as(isr, "Spatial"), col = "lightgrey",
              first = FALSE))) 
png(filename = "figures/comp.png", width = 17, height = 17, units = "cm",
    res = 300)
print(p_1)
dev.off()

# does not make really sense
# r_1 = rasterToPolygons(r_1)
# r_1$year = 1997
# r_2 = rasterToPolygons(r_2)
# r_2$year = 2010
# r_3 = rasterToPolygons(r_3)
# r_3$year = 2018
# r_all = rbind(r_1, r_2, r_3)
# yrs = c(1997, 2010, 2018)
# time = as.POSIXct(paste(yrs, "-01-01", sep = ""), tz = "GMT")
# 
# r_all = STFDF(as(r_1, "SpatialPolygons"), time, as.data.frame(r_all))
# library("RColorBrewer")
# stplot(r_all[, , "layer"], yrs)


# Tel Aviv hineinzoomen + Animation Plot der Investoren, multi-nationale U
# (mnc), accelerator, start-ups bis 11.07.2018 ideas: 1. variogram start-ups for
# different phases -> does autocorrelation change? + interpolation? 2. use
# distance to mnc, accelerator, investors as predictors for spatially predicting
# start-ups