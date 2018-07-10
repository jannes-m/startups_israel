# Filename: 02_viz.R (2018-06-16)
#
# TO DO: Visualize temporal development of Israelian start-ups and sponsors
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. VARIOGRAM
# 4. SU VISUALIZATION
# 5. SU "CAPITAL" ANIMATION
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
                 password = "jannes")
RPostgreSQL::dbListTables(conn)
# startup locations
su = st_read(conn, query = "select * from israel.startups")
# sponsors (acc/inv/mcn)
spon = st_read(conn, query = "select * from israel.sponsors")
# Israel/Palestine admin polygons
isr = st_read(conn, query = "select * from israel.israel")
# census tracts
cs = st_read(conn, query = "select * from israel.census")
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
# 3 VARIOGRAM----------------------------------------------
#**********************************************************

library("gstat")
r = raster(xmn = floor(b_box$xmin), xmx = ceiling(b_box$xmax),
           ymn = floor(b_box$ymin), ymx = ceiling(b_box$ymax), res = 100,
           crs = st_crs(su)$proj4string)
r_1 = rasterize(filter(su, phase == 1989), r, field = "id", fun = "count")
p_1 = rasterToPoints(r_1) %>%
  as.data.frame
coordinates(p_1) =~ x + y
vg_1 = variogram(layer ~ 1, data = p_1, width = 300, cutoff = 2500)
plot(vg_1, plot.numbers = TRUE)

r_2 = rasterize(filter(su, phase == 2007), r, field = "id", fun = "count")
p_2 = rasterToPoints(r_2) %>%
  as.data.frame
coordinates(p_2) =~ x + y
vg_2 = variogram(layer ~ 1, data = p_2, width = 300, cutoff = 2500)
plot(vg_2, plot.numbers = TRUE)

r_3 = rasterize(filter(su, phase == 2018), r, field = "id", fun = "count")
p_3 = rasterToPoints(r_3) %>%
  as.data.frame
coordinates(p_3) =~ x + y
vg_3 = variogram(layer ~ 1, data = p_3, width = 300, cutoff = 2500)
plot(vg_3, plot.numbers = TRUE)

vg_1$phase = 1
vg_2$phase = 2
vg_3$phase = 3
vg = rbind(vg_1, vg_2, vg_3)
xyplot(gamma ~ dist | phase, data = vg, as.table = TRUE,
       scales = list(y = "free"), layout = c(1, 3))


#**********************************************************
# 4 SU VISUALIZATION---------------------------------------
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
# png(filename = "figures/comp.png", width = 17, height = 17, units = "cm",
#     res = 300)
print(p_1)
# dev.off()

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

#**********************************************************
# 5 SU CAPITAL ANIMATION-----------------------------------
#**********************************************************

# polygonize startup capitals
s_2 = sum(s)
plot(s_2)
plot(s_2 > 500)
plot(s_2 > 200)
plot(s_2 > 100)
plot(s_2 > 75)
plot(s_2 > 60)
s_2[s_2 > 60]

polys = s_2 > 60
polys = polys %>% 
  clump %>%
  rasterToPolygons %>%
  st_as_sf
plot(isr$geometry, col = NA)
plot(polys, add = TRUE)

cap = polys %>%
  group_by(clumps) %>%
  summarize
# Tel Aviv
filter(cap, clumps == 3) %>% dplyr::select(geometry) %>% plot
cs = st_transform(cs, crs = st_crs(cap))

tv = filter(cap, clumps == 3)
plot(tv$geometry)
plot(cs$geometry, add = TRUE)
plot(su$geometry, col = "red", pch = 16, add = TRUE)

# 2.2 Tel Aviv animation example===========================
#**********************************************************
# just have a look at TV
plot(s_2)
# aggregate to a coarser resolution
s_3 = aggregate(s_2, fact = 1.5, fun = sum)
cellStats(s_3, max)
# just use
tv = s_3 == cellStats(s_3, max)
tv[tv == 0] = NA
tv = trim(tv)
plot(tv)
plot(cs$geometry, add = TRUE)
plot(raster::extend(tv, 1))
plot(cs$geometry, add = TRUE)
tv = rasterToPolygons(clump(tv)) %>% st_as_sf
plot(tv$geometry)
plot(cs$geometry, add = TRUE)
# ok, move 5km to the west
tv = tv$geometry + c(-5000, 0)
plot(tv, add = TRUE, border = "blue")
tv = st_set_crs(tv, st_crs(cs))
cs_tv = cs[tv, op = st_within]
plot(cs_tv$geometry)
plot(tv, add = TRUE, border = "blue")
# add su information per year
cs_tv$id = 1:nrow(cs_tv)
# count points per polygon
pp = st_join(dplyr::select(cs_tv, id), dplyr::select(su, founded))
# just consider startups founded after 1969
pp = filter(pp, is.na(founded) | founded >= 1970)
filter(pp, is.na(founded))[1, ] %>% st_geometry %>% plot
plot(su$geometry, add = TRUE, col = "red", pch = 16)  
# ok, there are polygons without any startups
pp = mutate(pp, n = ifelse(is.na(founded), 0, 1))
# prior to aggregation, refactor
labs = seq(1980, 2020, by = 10)
pp$phase = cut(pp$founded, breaks = c(1900, labs - 1), 
               labels = paste0(labs - 10, "-", labs - 1),
               include.lowest = FALSE)
# think about if this is what you want!
filter(pp, founded == 1989)
filter(pp, founded == 1990)
filter(pp, founded == 1991)
# aggregate
pp_agg = group_by(pp, id, phase) %>%
  summarize(n = sum(n, na.rm = TRUE))
# check
filter(pp_agg, is.na(phase))  # cs without any startups, all 0, excellent

# now repeat the geometry for each year
sort(unique(pp_agg$phase))

# ok, NA phase is not very meaningful, so just assign NA phase to the first phase
# and then all phases will be repeated whereby each of the tracts will receive 0
# startups for each phase
filter(pp_agg, is.na(phase))
pp_agg = mutate(pp_agg, 
                phase = as.character(phase),
                phase = ifelse(is.na(phase), "1970-1979", phase))

groups = ungroup(pp_agg) %>% 
  dplyr::select(phase, id) %>% 
  st_set_geometry(NULL) %>% 
  filter(!duplicated(paste(phase, id)))
groups = expand.grid(unique(groups$phase), unique(groups$id))
names(groups) = c("phase", "id")
# founded and id = unique?
filter(pp_agg, duplicated(phase) & duplicated(id))  # yes, very good
# full join (explode geometries)
pp_agg_expl = full_join(pp_agg, groups, by = c("id", "phase"))
length(unique(groups$phase))  # 49
table(pp_agg_expl$id)  # all 49, perfect
length(unique(groups$id))  # 528
table(pp_agg_expl$phase)  # all 528, perfect
pp_agg_expl$empty = st_dimension(pp_agg_expl)
pp_agg_expl = mutate(pp_agg_expl, empty= ifelse(is.na(empty), 0, 1))
# repeat geometries for all empty geometries
arrange(pp_agg_expl, id, desc(empty))  
plot(pp_agg_expl$geometry, col = NA)

# the following code does not work, because sometimes we have more than 1 geom 
# per group
# pp_agg_expl = group_by(pp_agg_expl, id) %>%
#   arrange(id, desc(empty)) %>%
#   mutate(geometry = geometry[1])

geom = group_by(pp_agg_expl, id) %>% 
  filter(empty == 1) %>%
  # sometimes we already have more than one geometry per group,
  # just keep the first
  slice(1)
# repeat each geometry five times
geom = rep(st_geometry(geom), each = 5)
pp_agg_expl = arrange(pp_agg_expl, id)
pp_agg_expl$geometry = geom
plot(pp_agg_expl[, "n"])
# you have to cast, otherwise tmap will complain
pp_agg_expl = st_cast(pp_agg_expl, "MULTIPOLYGON")  
# multi-panel plot
# find out how to put legend into several columns 
tm_shape(pp_agg_expl) + 
  tm_fill("n", style = "fisher", n = 9) +
  tm_facets(by = "phase") + 
  tm_legend(legend.outside.position = "bottom")
# not really convincing
# have a look at the outlier
ol = filter(pp_agg_expl, n > 500)
units::set_units(st_area(ol), "km^2")
mapview::mapview(ol)
plot(ol$geometry)
plot(su$geometry, add = TRUE) 
# apparently, the same coordinate is plotted over and over again
su[ol, ]  # -> Tel Aviv-Yafo

# Animation
# find out how to use three or more legend columns
my_ani = tm_shape(pp_agg_expl) +
  tm_fill("n", title = "Number of startups", style = "fisher", 
          n = 9) +
  tm_facets(along = "phase") + 
  tm_legend(# legend.outside = TRUE, 
            # legend.outside.position = "bottom",
            legend.stack = "horizontal",
            legend.title.size = 0.5,
            legend.text.size = 0.4
            #legend.format = list(digits = 0)
            )
# system("magick")  # does work
# system("magick convert -version")
tmap_animation(my_ani, 
               filename = file.path(tempdir(), "test.gif"),
               width = 1000, height = 900, delay = 150)

#**********************************************************
# ACC, INV, MNC--------------------------------------------
#**********************************************************

table(spon$type)
plot(isr$geometry, col = NA)
plot(spon$geometry, add = TRUE, pch = 16, col = as.factor(spon$type), cex = 0.4)
legend("right", legend = levels(as.factor(spon$type)), pch = 16,
       col = 1:3)
spon$type = as.factor(spon$type)
l_1 = list("sp.polygons", as(isr, "Spatial"))
spplot(as(spon, "Spatial"), "type",
       sp.layout = l_1)

coords = st_coordinates(spon) %>%
  as.data.frame
poly = rasterToPolygons(sum(s))
q_7 = classInt::classIntervals(poly$layer, style = "fisher", n = 7)
pal = RColorBrewer::brewer.pal(7, "YlOrRd")
poly$col = classInt::findColours(q_7, pal)

xyplot(Y ~ X | spon$type, data = coords, aspect = "iso",
       xlab = "", ylab = "",
       scales = list(
         alternating = c(1, 0),
         tck = c(1, 0)
       ),
       panel = function(...) {
         sp.polygons(as(isr, "Spatial"), col = gray(0.5))
         sp.polygons(poly, fill = poly$col, col = "lightgray")
         panel.points(..., pch = 16, cex = 0.5, col = "black")
        })
# also possible to additionally group panels by founding phase/year

# Indicator variogram
su_2 = dplyr::select(su, founded)
su_2$type = as.factor("su")
spon_2 = dplyr::select(spon, founded)
# also possible:
# spon_2 = dplyr::select(spon, founded, type)
spon_2$type = as.factor("spon")
indi = rbind(su_2, spon_2)
indi = as(indi, "Spatial")
vi = variogram(type ~ 1, location = indi, cutoff = 2000, width = 100)
plot(vi, plot.numbers = TRUE)
plot(vi)
# this means there is a spatial clustering of sus without sponsors (well, not
# really surprising since there are so many more sus than sponsores)
# but keep in mind that we are here looking at the aggregate:
# - founded years
# - sponsors can also be subdivided into acc, inv, mnc

# but it might also be that the 66073 observations are stemming mainly from
# one or two places...
plot(ol$geometry)
plot(su$geometry, add = TRUE) 
plot(spon$geometry, add = TRUE, col = "blue", pch = 16)
# ok, here are at least two sponsors
spon[ol, "type"]  # in fact, a few sponsors are sharing the same coordinate

#**********************************************************
# IDEAS----------------------------------------------------
#**********************************************************

# Tel Aviv hineinzoomen + Animation Plot der Investoren, multi-nationale U
# (mnc), accelerator, start-ups bis 11.07.2018 

# ideas: 
# 1. variogram start-ups for different phases -> does autocorrelation change? +
# interpolation?
# 2. Indicator variogram of sus and sponsors
# 3. use distance to mnc, accelerator, investors as predictors for spatially
# predicting start-ups
# 4. or compute distance to closest mnc, acc, inv and use dist as variogram variable
