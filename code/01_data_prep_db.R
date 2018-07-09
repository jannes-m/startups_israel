# Filename: data_prep_db.R (2018-07-03)
#
# TO DO: Prepare data and put it into a PostgreSQL/PostGIS db
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. DATA PREPARATION
# 3. DB STUFF
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RPostgreSQL")
library("readxl")
library("dplyr")
library("sf")
library("raster")

#**********************************************************
# 2 DATA PREPARATION---------------------------------------
#**********************************************************

# 2.1 startups=============================================
#**********************************************************

# read in start-ups (su)
su = read_excel("data/20180328 Datensatz Masterarbeit_cleaned_geocode.xlsx")
colSums(is.na(su))
filter(su, is.na(founded)) %>%
  dplyr::select(founded, created_date, update_date)
# ok, get rid off them (-> ask Susann, if ok)
su = filter(su, !is.na(founded))
table(su$founded)  # 1, not very sensible
su = filter(su, founded != 1)

# have a look at the coordinates
dplyr::filter(su, is.na(lat) | is.na(lon)) %>%
  dplyr::select(lat, lon)
# ok, remove
su = filter(su, !(is.na(lat) | is.na(lon)))
# ok, there are some wrong coordinates
summary(su[, c("lat", "lon")])
# convert to sf
su = st_as_sf(su, coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  st_transform(2039)
plot(st_geometry(su))
plot(st_geometry(isr), add = TRUE, col = "red")
# have a look at the points outside of Israel
out = su[st_union(isr), op = sf::st_disjoint]
dim(su)  # nrow 6665
dim(out)  # nrow 9
plot(st_geometry(isr), col = NA)  # mmh, they should be inside isr
plot(st_geometry(out), add = TRUE, pch = 16, col = "red") 
# ok, include the two border case points 
# dim(d[isr, op = st_is_within_distance, dist = 10000])
# 6664, does not work as expected (guess this is because crs transformation
# resulted in empty geometries), then do it yourself
buf = st_buffer(st_union(isr), dist = 10000)
plot(buf, add = TRUE)
su = su[buf, ]
dim(su)  # 6658, ok, perfect

# 2.2 Israel/Palestine polygons============================
#**********************************************************
# israel polygons
isr = getData("GADM", country = "ISR", level = 1, path = "data") %>%
  st_as_sf %>%
  st_transform(2039)
ccodes()[grep("Palest", ccodes()$NAME), ]
pa = getData("GADM", country = "PSE", level = 0, path = "data") %>%
  st_as_sf %>%
  st_transform(2039)
# join the two countries 
isr = st_union(isr, pa)
plot(st_geometry(isr), col = NA, border = "red")

# 2.3 ACC/INV/MNC==========================================
#**********************************************************

# accelerators/investors/multi-national companies (?) = sponsors
spon = readxl::read_excel("data/raw_geocode_acc_inv_mnc.xlsx")
colSums(is.na(spon))
spon = filter(d, !(is.na(lon) | is.na(lat)))
dim(spon)  # 624
spon = st_as_sf(spon, coords = c("lon", "lat"))
spon = st_set_crs(spon, 4326)
spon = st_transform(spon, 2039)
plot(spon$geometry)  # ok, there is one wrong coordinate
plot(isr$geometry, col = NA)
plot(spon$geometry, add = TRUE)
plot(spon[st_union(isr), ]$geometry, col = "blue", pch = 16, add = TRUE)
spon = spon[st_buffer(st_union(isr), 5000), ]
dim(spon)  # 623

#**********************************************************
# 3 DB STUFF-----------------------------------------------
#**********************************************************

# create qual_gis database
pg = dbDriver("PostgreSQL")
# not sure why I cannot login as jannes... (I don't want connect to a db but
# just to the server, maybe you have to use the postgres user for this...)
# dbConnect(pg, user = "jannes", host = "localhost", port = 5432, password = "jannes")
conn = dbConnect(pg, 
                 user = "postgres",
                 host = "localhost",
                 port = 5432, 
                 password = "postgres")
# create a new database
dbSendQuery(conn, "CREATE DATABASE qual_gis;")
dbDisconnect(conn)

# connect to the newly created database, this time you can use user jannes
conn = dbConnect(drv = PostgreSQL(), 
                 user = "jannes",
                 dbname = "qual_gis", 
                 port = 5432,
                 password = "jannes")
# enable PostGIS extension
dbSendQuery(conn, "CREATE EXTENSION postgis;")
# create a new schema
dbSendQuery(conn, "CREATE SCHEMA israel;")

# write su into the schema israel named "startups"
st_write(su, conn, c("israel", "startups"))
# or using RPostgreSQL
# RPostgreSQL::dbWriteTable(conn = conn, name c("israel", "startups"), 
#                           value = su)
# check if you can load it back into R
# st_read(conn, query = "select * from israel.startups")  # ok, excellent

# specify a primary key column
dbSendQuery(conn, "ALTER TABLE israel.startups ADD PRIMARY KEY (id);")

# add Israel/Palestina to the db
st_write(isr, conn, c("israel", "israel"))
dbSendQuery(conn, "ALTER TABLE israel.israel ADD PRIMARY KEY (OBJECTID);")
# does not work, seems to be an encoding problem...

# add sponsors
st_write(spon, conn, c("israel", "sponsors"))
dbSendQuery(conn, "ALTER TABLE israel.sponsors ADD PRIMARY KEY (id);")
