# Filename: geocode.R (2017-11-27)
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
# 3. GEOCODING ACC, INV, MNC
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

#**********************************************************
# 2 GEOCODING STARTUPS-------------------------------------
#**********************************************************

# attach data
su = read_excel("data/20171108 Israeli IT companies.xlsx", sheet = 1)
# add ID column
su = mutate(d, id = 1:nrow(d))
(load(file = file.path(dir_ima, "geocode.Rdata")))
geocodeQueryCheck()
start = out$id[nrow(out)] + 1
end = nrow(su)
# ok, then we just build a slow loop, not that important here
out_2 = list()
for (i in start:end) {
  print(su$id[i])
  print(su$address[i])
  if (is.na(su$address[i])) {
    add = data.frame(lon = NA, lat = NA)
  } else {
    Sys.sleep(sample(seq(7.4, 14.5, 0.1), 1))
    try({add = geocode(su$address[i], output = "more")})
    x = 4
    while (is.na(su$lon) & x > 0) {
      print(su$id[i])
      Sys.sleep(sample(seq(7.4, 14.5, 0.1), 1))
      try({add = geocode(su$address[i], output = "more")})
      # try five times
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
su[start + nrow(out_2) - 1, c("address", "id")]
# out$id = 1:nrow(out)
out = rbindlist(list(out, out_2), fill = TRUE)
# how many NAs, do we have here
nrow(out[is.na(lon)])
# goodness of the geocoding (approximate -> e.g., Tel Aviv-Yafo; Israel)
# rooftop: perfect fit
table(out$loctype)
# save(out, file = "images/geocode.Rdata")

# save the result
# load(file = "images/geocode.Rdata")
out = dplyr::select(out, id, lon, lat, type, loctype, route, street_number,
                    locality, country)
res = inner_join(d, out, by = "id")
write.xlsx2(res, file = "data/raw_geocode.xlsx")

#**********************************************************
# 3 GEOCODING ACC, INV, MNC--------------------------------
#**********************************************************

# attach data
acc = read_excel("data/Datenblatt_Acceleratoren.xlsx")
inv = read_excel("data/Datenblatt_Investoren.xlsx")
mnc = read_excel("data/Datenblatt_MNC.xlsx")

identical(names(acc), names(inv))  # TRUE
identical(names(inv), names(mnc))  # TRUE

acc$type = "acc"
inv$type = "inv"
mnc$type = "mnc"
d = rbind(acc, inv, mnc)
sum(is.na(d$address))  # 177 without address
group_by(d, type) %>%
  summarize(missing_address = sum(is.na(address)))

start = 1
end = nrow(d)
out = list()
for (i in start:end) {
  print(su$id[i])
  print(su$address[i])
  if (is.na(su$address[i])) {
    add = data.frame(lon = NA, lat = NA)
  } else {
    Sys.sleep(sample(seq(7.4, 14.5, 0.1), 1))
    try({add = geocode(su$address[i], output = "more")})
    x = 4
    while (is.na(su$lon) & x > 0) {
      print(su$id[i])
      Sys.sleep(sample(seq(7.4, 14.5, 0.1), 1))
      try({add = geocode(su$address[i], output = "more")})
      # try five times
      x = x - 1
    }
  }
  # return your output
  print(add)
  out = data.table::rbindlist(list(out, add), fill = TRUE)
}

