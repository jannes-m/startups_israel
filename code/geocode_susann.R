# Filename: geocode_startups.R (2017-11-27)
#
# TO DO: Geocode startups
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. GEOCODING
# 3. RESULT
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("dplyr")
library("readxl")
library("ggmap")
library("data.table")
library("xlsx")

# define directories
dir_main = "D:/uni/science/projects/economic/israel"
dir_data = file.path(dir_main, "data")
dir_ima = file.path(dir_main, "images")

# attach data
d = read_excel(file.path(dir_data, "20171108 Israeli IT companies.xlsx"), 
               sheet = 1)

#**********************************************************
# 2 GEOCODING----------------------------------------------
#**********************************************************

# add ID column
d = mutate(d, id = 1:nrow(d))
(load(file = file.path(dir_ima, "geocode.Rdata")))
geocodeQueryCheck()
start = out$id[nrow(out)] + 1
end = nrow(d)
# ok, then we just build a slow loop, not that important here
out_2 = list()
for (i in start:end) {
  print(d$id[i])
  print(d$address[i])
  if (is.na(d$address[i])) {
   add = data.frame(lon = NA, lat = NA)
  } else {
    Sys.sleep(sample(seq(7.4, 14.5, 0.1), 1))
    try({add = geocode(d$address[i], output = "more")})
    x = 2
    while (is.na(add$lon) & x > 0) {
      print(d$id[i])
      Sys.sleep( sample(seq(7.4, 14.5, 0.1), 1))
      try({add = geocode(d$address[i], output = "more")})
      # just try three times
      x = x - 1
    }
  }
  # return your output
  print(add)
  out_2 = data.table::rbindlist(list(out_2, add), fill = TRUE)
  }

#out[id := 1:nrow(out)]
out_2$id = start:(start + nrow(out_2) - 1)
# just check
out_2[nrow(out_2), ]
d[start + nrow(out_2) - 1, c("address", "id")]
# out$id = 1:nrow(out)
out = rbindlist(list(out, out_2), fill = TRUE)
# how many NAs, do we have here
nrow(out[is.na(lon)])
# goodness of the geocoding (approximate -> e.g., Tel Aviv-Yafo; Israel)
# rooftop: perfect fit
table(out$loctype)
save(out, file = file.path(dir_ima, "geocode.Rdata"))

#**********************************************************
# 3 OUTPUT-------------------------------------------------
#**********************************************************

# load(file = file.path(dir_ima, "geocode.Rdata"))

out = select(out, id, lon, lat, type, loctype, route, street_number, locality,
             country)
res = inner_join(d, out, by = "id")
write.xlsx2(res, file = "C:/Users/pi37pat/Desktop/geocode.xlsx")
