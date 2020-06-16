# placename standardization script for openarch death certificates

setwd("~/02. Werk/Clariah")
rm(list = ls())

library("data.table")
library("stringi")
library("stringr")

setDTthreads(threads = 8)

openarch = fread("openarch/openarch_death_nonduplids_basefile.csv.gz") 
setDT(openarch)

openarch[EVENT_PLACE == "", EVENT_PLACE := NA]
openarch[SOURCE_PLACE == "", SOURCE_PLACE := NA]

# compare event_pl with source_pl

places <- openarch[!is.na(EVENT_PLACE) & !is.na(SOURCE_PLACE), .N, list(EVENT_PLACE, SOURCE_PLACE)][order(-N)]
places[EVENT_PLACE != SOURCE_PLACE, sum(N, na.rm = T),] #1,970,831, but in most cases just spelling differences or more detail
places[EVENT_PLACE == SOURCE_PLACE, sum(N, na.rm = T),] # 13,183,002
rm(places)

# decision: go with EVENT_PLACE and replace NA's with SOURCE_PLACE 
openarch[is.na(EVENT_PLACE), EVENT_PLACE := SOURCE_PLACE]

#generate event place standardized: lcase, nospace and not solely digits, no accents 
openarch[,EVENT_PLACE_ST := tolower(EVENT_PLACE)]
openarch[,EVENT_PLACE_ST := gsub('[[:punct:] ]+',' ',EVENT_PLACE_ST)]
openarch[,EVENT_PLACE_ST := gsub(" ", "",EVENT_PLACE_ST, fixed = TRUE)]
openarch[,EVENT_PLACE_ST := gsub("'", "",EVENT_PLACE_ST, fixed = TRUE)]
openarch[,EVENT_PLACE_ST := trimws(EVENT_PLACE_ST, which = c("both"))]
openarch[!grepl("[a-zA-Z]+", EVENT_PLACE_ST), EVENT_PLACE_ST := NA] 
openarch[, EVENT_PLACE_ST := stri_trans_general(EVENT_PLACE_ST, "Latin-ASCII")]

# make list of event_place_st plus min/max year

openarch[EVENT_YEAR > 1968, EVENT_YEAR := NA] 
openarch[EVENT_YEAR < 1811, EVENT_YEAR := NA] 

openarch[!is.na(EVENT_PLACE_ST),  min_place_year := min(EVENT_YEAR, na.rm=T), by = EVENT_PLACE_ST]
openarch[!is.na(EVENT_PLACE_ST),  max_place_year := max(EVENT_YEAR, na.rm=T), by = EVENT_PLACE_ST]

# take out placenames 

places <- openarch[!is.na(EVENT_PLACE_ST), .N, list(EVENT_PLACE, EVENT_PLACE_ST, min_place_year, max_place_year)][order(-N)]

fwrite(places, "openarch/openarch_death_placenames.csv", sep = ";", row.names = F)


### Toponymen file IISG (https://iisg.amsterdam/en/hsn/data/place-names)

top <- read.csv(url("https://iisg.amsterdam/files/2019-04/DutchToponyms1812-2012Spatio-Temporal.txt"), sep = "|", header = TRUE, 
                fileEncoding= "ASCII", skipNul = T)
setDT(top)

# nicer colnames
setnames(top, tolower(names(top)))
setnames(top, "þÿtoponym..city.town.village.hamlet.", "toponym")
setnames(top, "lattitude..3.dec.degrees.", "lat")
setnames(top, "longitude..3.dec.degrees." , "long")
setnames(top, "amsterdam.code" , "amco")
setnames(top, "part.of.municipality.from.jjjjmmdd", "startdate_munc")
setnames(top, "till.jjjjmmdd", "enddate_munc")

# set year ranges 
top[, first_year := substr(startdate_munc, 1, 4)]
top[, last_year := substr(enddate_munc, 1, 4)][, last_year := as.integer(last_year) - 1] # to include last year, as new munc is (often) from 01-01 next year

# keep relevant vars

top <- top[,-c(8:11,13)]

# standardize toponym and munc

top[,top_st:= tolower(toponym)]
top[,top_st := gsub('[[:punct:] ]+',' ',top_st)]
top[,top_st := gsub(" ", "",top_st, fixed = TRUE)]
top[,top_st := gsub("'", "",top_st, fixed = TRUE)]
top[,top_st := trimws(top_st, which = c("both"))]
top[, top_st := stri_trans_general(top_st, "Latin-ASCII")]

top[,munic_st := tolower(municipality)]
top[,munic_st := gsub('[[:punct:] ]+',' ',munic_st)]
top[,munic_st := gsub(" ", "",munic_st, fixed = TRUE)]
top[,munic_st := gsub("'", "",munic_st, fixed = TRUE)]
top[,munic_st := trimws(munic_st, which = c("both"))]
top[, munic_st := stri_trans_general(munic_st, "Latin-ASCII")]

# find event_place_st within top and munc (to check if standardize on top or munc or both)

#openarch[!is.na(EVENT_PLACE_ST), .N, by = EVENT_PLACE_ST] # 25000 unique placenames 
#openarch[!is.na(EVENT_PLACE_ST), .N] # 16027394 obs

#openarch[EVENT_PLACE_ST %in% top$munic_st, .N, by = EVENT_PLACE_ST] # 1443 unique pn matched (6.4% of unique pn)
#openarch[EVENT_PLACE_ST %in% top$munic_st, .N, ] # 14344883 matched (89% of all placesobs)
#openarch[EVENT_PLACE_ST %in% top$top_st, .N, by = EVENT_PLACE_ST] # 2427 (10.7%)
#openarch[EVENT_PLACE_ST %in% top$top_st, .N, ]  # 14535683 (90.6%)

# preferred strategy is first match on top+mun (because of multiple top with same name) 
# only does slightly worse than toponym but less false positives (Zwolle, GL vs. Zwolle, Ov, etc.)
# remaining placenames on toponym 
# also: Vulsma 1988 states that recorded place of death should be municipality 

### 1: match double event_place_st string with top_st+munic_st 
#(to make sure the pn is also actually the munc: top is always munc but not vice versa)

places[, placeplace := paste0(EVENT_PLACE_ST,EVENT_PLACE_ST) ]
top[, topmun := paste0(top_st,munic_st)]

places[placeplace %in% top$topmun, match := 1, ]

# merge places with top for match1

m1 <- places[match == 1, merge(places[,c("EVENT_PLACE", "EVENT_PLACE_ST", "placeplace", "N", "min_place_year", "max_place_year", "match")], 
                               top[,c("toponym", "municipality", "topmun", "amco")], 
                               by.x = "placeplace", by.y = "topmun", all.x = F, all.y = F)]

m1 <- m1[!duplicated(m1, fromLast=T),]
m1[, id := paste0(EVENT_PLACE,amco)]
m1 <- m1[!duplicated(id, fromLast=T),]
m1[, id := NULL]

# check if all matches are still there

'%ni%' <- Negate('%in%')
places[match == 1 & placeplace %ni% m1$placeplace,]

# take out all remaining double placenames/amco for manual resolving and keep matches in m1

dup_placenames_m1 <- m1[duplicated(EVENT_PLACE, fromLast = F) | duplicated(EVENT_PLACE, fromLast = T),]
dup_placenames_m1[, placeplace := NULL]

m1 <- m1[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)] # 1803 pn, 1434488 obs matched
m1[, placeplace := NULL]
places[, placeplace := NULL]

### 2: match event_place with top_st+munic_st (often pn is already combination of top+mun in openarch)

top[, topmun := paste0(top_st,munic_st)]

places[EVENT_PLACE_ST %in% top$topmun & is.na(match), match := 2,]

m2 <- places[match == 2, merge(places[,c("EVENT_PLACE", "EVENT_PLACE_ST", "N", "min_place_year", "max_place_year", "match")], 
                                top[,c("toponym", "municipality", "topmun", "amco")], 
                                by.x = "EVENT_PLACE_ST", by.y = "topmun", all.x = F, all.y = F)]


m2 <- m2[!duplicated(m2, fromLast=T),]
m2[, id := paste0(EVENT_PLACE,amco)]
m2<- m2[!duplicated(id, fromLast=T),]
m2[, id := NULL]

# check if all matches are still there
places[match == 2 & EVENT_PLACE %ni% m2$EVENT_PLACE,]

# take out all remaining double placenames/amco for manual resolving and keep matches in m2

dup_placenames_m2 <- m2[duplicated(EVENT_PLACE, fromLast = F) | duplicated(EVENT_PLACE, fromLast = T),]

m2 <- m2[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)] # 2020 pn, 722386 obs matched


### 3: match event_place_st with top_st

places[EVENT_PLACE_ST %in% top$top_st & is.na(match) , match := 3,] # 1065 pn, 190654 obs

m3 <- places[match == 3, merge(places[,c("EVENT_PLACE", "EVENT_PLACE_ST", "N", "min_place_year", "max_place_year", "match")], 
                               top[,c("toponym", "municipality", "top_st", "amco")], 
                               by.x = "EVENT_PLACE_ST", by.y = "top_st", all.x = F, all.y = F)]


m3 <- m3[match == 3,]
m3 <- m3[!duplicated(m3, fromLast=T),]
m3[, id := paste0(EVENT_PLACE,amco)]
m3<- m3[!duplicated(id, fromLast=T),]
m3[, id := NULL]

# check if all matches are still there
places[match == 3 & EVENT_PLACE %ni% m3$EVENT_PLACE,]

# take out all remaining double placenames/amco for manual resolving and keep matches in m3

dup_placenames_m3 <- m3[duplicated(EVENT_PLACE, fromLast = F) | duplicated(EVENT_PLACE, fromLast = T),]

m3 <- m3[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)] # 412 pn, 722386 obs matched


### 4: match string after "gem" (short for municipality) with topmun:

places[, gem:= as.character(lapply(strsplit(as.character(EVENT_PLACE_ST), split="gem"),tail, n=1))]
places[, gem := paste0(gem,gem)]

top[, topmun := paste0(top_st,munic_st)]

places[gem %in% top$topmun & is.na(match), match := 4,] # 1270 pn, 363917 obs

m4 <- places[match == 4,]


m4 <- merge(m4[,c("EVENT_PLACE", "EVENT_PLACE_ST", "gem", "N", "min_place_year", "max_place_year", "match")], 
                               top[,c("toponym", "municipality", "topmun", "amco")], 
                               by.x = "gem", by.y = "topmun", all.x = F, all.y = F)


m4 <- m4[!duplicated(m4, fromLast=T),]
m4[, id := paste0(EVENT_PLACE,amco)]
m4 <- m4[!duplicated(id, fromLast=T),]
m4[, id := NULL]

# check if all matches are still there

places[match == 4 & EVENT_PLACE %ni% m4$EVENT_PLACE,] # 1 remaining...

# take out all remaining double placenames/amco for manual resolving and keep matches in m4

dup_placenames_m4 <- m4[duplicated(EVENT_PLACE, fromLast = F) | duplicated(EVENT_PLACE, fromLast = T),]

m4 <- m4[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)]
m4[, gem := NULL]
places[, gem:= NULL]
dup_placenames_m4[, gem:= NULL]

#### bind matches, conflicts, and to do's

openarch_places_matched <- rbind(m1,m2,m3,m4)

openarch_places_conflicts <- rbind(dup_placenames_m1,dup_placenames_m2,dup_placenames_m3,dup_placenames_m4)

openarch_places_todo <- places[EVENT_PLACE %ni% openarch_places_matched$EVENT_PLACE 
                               & EVENT_PLACE %ni% openarch_places_conflicts$EVENT_PLACE,]


### say we code remaining unmatched/conflicts manually only when N >= 100, that gets the obs coded up to well over 99%

openarch_places_todo[N >= 100, ]
openarch_places_todo[N >= 100, sum(N)]

openarch_places_conflicts[!duplicated(EVENT_PLACE, fromLast = T) & N >=100,]
openarch_places_conflicts[!duplicated(EVENT_PLACE, fromLast = T) & N >=100, sum(N)]

### remove discarded

rm(m1,m2,m3,m4)
rm(dup_placenames_m1,dup_placenames_m2,dup_placenames_m3,dup_placenames_m4)


### write to csv

fwrite(openarch_places_matched, "openarch/openarch_death_places_matched.csv", sep = ";", row.names = F)

fwrite(openarch_places_conflicts, "openarch/openarch_death_places_conflicts.csv", sep = ";", row.names = F)

fwrite(openarch_places_todo, "openarch/openarch_death_places_todo.csv", sep = ";", row.names = F)



