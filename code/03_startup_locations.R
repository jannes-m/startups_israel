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

# attach data
d = read_excel("data/cleaned_xy.xlsx")

# Israel and Palestina  
cs = st_read("data/census_shp_2012/israel_demog2012.shp") %>%
  # using Israel's CRS
  # https://en.wikipedia.org/wiki/Israeli_Transverse_Mercator
  st_transform(2039)

isr = getData("GADM", country = "ISR", level = 1, path = "data") %>%
  st_as_sf %>%
  st_transform(2039)
ccodes()[grep("Palest", ccodes()$NAME), ]
pa = getData("GADM", country = "PSE", level = 0, path = "data") %>%
  st_as_sf %>%
  st_transform(2039)
# join the two countries 
isr = st_union(isr, pa)
plot(st_geometry(cs))
plot(st_geometry(isr), col = NA, add = TRUE, border = "red")
# ok, more or less overlapping (Gaza strip is missing from cs, and Palestina as
# well)

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

colSums(is.na(d))
filter(d, is.na(founded)) %>%
  dplyr::select(founded, created_date, update_date)
# ok, get rid off them (-> ask Susann, if ok)
d = filter(d, !is.na(founded))
table(d$founded)  # 1, not very sensible
d = filter(d, founded != 1)

# have a look at the coordinates
dplyr::filter(d, is.na(lat) | is.na(lon)) %>%
  dplyr::select(lat, lon)
# ok, remove
d = filter(d, !(is.na(lat) | is.na(lon)))
# ok, there are some wrong coordinates
summary(d$lat)
summary(d$lon)
# convert to sf
d = st_as_sf(d, coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(2039)
plot(st_geometry(d))
plot(st_geometry(isr), add = TRUE, col = "red")
# have a look at the points outside of Israel
out = d[st_union(isr), op = sf::st_disjoint]
plot(st_geometry(isr), col = NA)  # mmh, they should be inside isr
plot(st_geometry(out), add = TRUE, pch = 16, col = "red") 
# ok, neglect the two points for the moment 
d = d[st_union(isr), ]

# tree start-up phases
# 1: -1997; 2: 1998-2010; 3: 2011-2018
d$phase = cut(d$founded, breaks = c(0, 1997, 2010, 2018), 
              labels = c(1997, 2010, 2018))
# check
filter(d, founded == 1997) %>% dplyr::select(founded, phase)  # looks good

# aggregate points into a raster of resolution 10 km
b_box = st_bbox(d)
r = raster(xmn = floor(b_box$xmin), xmx = ceiling(b_box$xmax),
           ymn = floor(b_box$ymin), ymx = ceiling(b_box$ymax), res = 10000,
           crs = st_crs(d)$proj4string)
r_1 = rasterize(filter(d, phase == 1997), r, field = "X__1", fun = "count")
# r_1[is.na(r_1)] = 0
r_2 = rasterize(filter(d, phase == 2010), r, field = "X__1", fun = "count")
# r_2[is.na(r_2)] = 0
r_3 = rasterize(filter(d, phase == 2018), r, field = "X__1", fun = "count")
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
# now in absolute numbers
s = trim(stack(list(su_1, su_2, su_3)))
c_7 = classInt::classIntervals(values(s), n = 7, style = "fisher")
cuts = c_7$brks
pal = RColorBrewer::brewer.pal(7, "YlOrRd")
nms = c("Pre-emergence", 
        "Emergence",
        "Growth")
coords = tibble::tribble(~x, ~y, ~name,
                         34.782777, 32.077890, "Tel Aviv",
                         34.990511, 32.800339, "Haifa",
                         35.212674, 31.766307, "Jerusalem"
)
s_2 = projectRaster(s, crs = st_crs(4326)$proj4string)
coords = st_as_sf(coords, coords = c("x", "y"), crs = 4326) %>%
  st_transform(crs = st_crs(s2))


p_2 = spplot(s_2, col.regions = pal, between = list(x = 0.5),
             colorkey = list(space = "right",
                             # labels specifies what to plot
                             # at specifies where to plot the labels
                             #labels = list(labels = c("", round(cuts[-1])),
                             #             at = cuts, cex = 0.7),
                             # percentage display
                             labels = list(labels =
                                             c("", round(cuts[2]),
                                               round(cuts[-c(1:2)])),
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
             scales = list(draw = TRUE, alternating = 1, tck = c(1, 0)),
             # ylab.right = "%",
             # par.settings = list(layout.widths = list(axis.key.padding = 0,
             #                                          ylab.right = 2)),
             strip = strip.custom(factor.levels = nms,
                                  bg = "white"),
             sp.layout = list(
               list("sp.polygons", as(st_transform(isr, 4326), "Spatial"), 
                    col = "lightgrey", first = FALSE),
               list("sp.points", as(st_geometry(coords), "Spatial"), col = "black",
                    cex = 1, pch = 16, first = FALSE),
               list("sp.text", st_coordinates(coords), txt = coords$name, 
                    cex = 1, font = 3,  pos = 1, first = FALSE)
             )) 

png(filename = "figures/su_abs.png", width = 17, height = 17, units = "cm",
    res = 300)
print(p_2)
dev.off()