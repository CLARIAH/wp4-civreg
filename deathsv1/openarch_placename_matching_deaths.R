library("data.table")
library("stringi")

setDTthreads(threads = 3)

openarch = fread("gunzip -c openarch_death.csv.gz") 

# EVENT_PLACE != SOURCE_PLACE #1,970,831, but in most cases just spelling differences or more detail
# EVENT_PLACE == SOURCE_PLACE # 13,183,002
openarch[EVENT_PLACE == "", EVENT_PLACE := NA]
openarch[SOURCE_PLACE == "", SOURCE_PLACE := NA]
openarch[is.na(EVENT_PLACE), EVENT_PLACE := SOURCE_PLACE]

#generate event place standardized: lcase, nospace and not solely digits, no accents 
openarch[,EVENT_PLACE_ST := tolower(EVENT_PLACE)]
openarch[,EVENT_PLACE_ST := gsub('[[:punct:] ]+', ' ', EVENT_PLACE_ST)]
openarch[,EVENT_PLACE_ST := gsub(" ", "",EVENT_PLACE_ST, fixed = TRUE)]
openarch[,EVENT_PLACE_ST := gsub("'", "",EVENT_PLACE_ST, fixed = TRUE)]
openarch[,EVENT_PLACE_ST := trimws(EVENT_PLACE_ST, which = c("both"))]
openarch[!grepl("[a-zA-Z]+", EVENT_PLACE_ST), EVENT_PLACE_ST := NA] 
openarch[, EVENT_PLACE_ST := stri_trans_general(EVENT_PLACE_ST, "Latin-ASCII")]

# make list of event_place_st plus min/max year

openarch[EVENT_YEAR > 1968, EVENT_YEAR := NA] 
openarch[EVENT_YEAR < 1811, EVENT_YEAR := NA] 

openarch[!is.na(EVENT_PLACE_ST), min_place_year := min(EVENT_YEAR, na.rm=T), by = EVENT_PLACE_ST]
openarch[!is.na(EVENT_PLACE_ST), max_place_year := max(EVENT_YEAR, na.rm=T), by = EVENT_PLACE_ST]

# export placenames 

places <- openarch[!is.na(EVENT_PLACE_ST), .N, list(EVENT_PLACE, EVENT_PLACE_ST, min_place_year, max_place_year)][order(-N)]

fwrite(places, "openarch_death_placenames.csv", sep = ";", row.names = F)

### Toponymen file IISG (https://iisg.amsterdam/en/hsn/data/place-names)
# local version, but still with awful encoding misery
top <- fread("DutchToponyms1812-2012Spatio-Temporal.txt")

# nicer colnames
setnames(top, tolower(names(top)))
setnames(top, "toponym (city,town,village,hamlet)", "toponym")
setnames(top, "lattitude (3 dec.degrees)", "lat")
setnames(top, "longitude (3 dec.degrees)" , "long")
setnames(top, "amsterdam code" , "amco")
setnames(top, "part of municipality from jjjjmmdd", "startdate_munc")
setnames(top, "till jjjjmmdd", "enddate_munc")

# set year ranges 
top[, first_year := substr(startdate_munc, 1, 4)]
top[, last_year := substr(enddate_munc, 1, 4)]
top[, last_year := as.integer(last_year) - 1] # to include last year, as new munc is (often) from 01-01 next year

# keep relevant vars
top <- top[,-c("start postal coderange cccc",
               "end postalcoderange cccc",
               "tel.area code (0)",
               "municipality?",
               "remarks include favored spelling nl,fr")]

# standardize toponym and munc
top[, top_st := tolower(toponym)]
top[, top_st := gsub('[[:punct:] ]+',' ',top_st)]
top[, top_st := gsub(" ", "",top_st, fixed = TRUE)]
top[, top_st := gsub("'", "",top_st, fixed = TRUE)]
top[, top_st := trimws(top_st, which = c("both"))]
top[, top_st := stri_trans_general(top_st, "Latin-ASCII")]

top[, munic_st := tolower(municipality)]
top[, munic_st := gsub('[[:punct:] ]+',' ',munic_st)]
top[, munic_st := gsub(" ", "",munic_st, fixed = TRUE)]
top[, munic_st := gsub("'", "",munic_st, fixed = TRUE)]
top[, munic_st := trimws(munic_st, which = c("both"))]
top[, munic_st := stri_trans_general(munic_st, "Latin-ASCII")]

# find event_place_st within top and munc (to check if standardize on top or munc or both)

# openarch[!is.na(EVENT_PLACE_ST), .N, by = EVENT_PLACE_ST] # 25000 unique placenames 
# openarch[!is.na(EVENT_PLACE_ST), .N] # 16027394 obs

# openarch[EVENT_PLACE_ST %in% top$munic_st, .N, by = EVENT_PLACE_ST] # 1443 unique pn matched (6.4% of unique pn)
# openarch[EVENT_PLACE_ST %in% top$munic_st, .N, ] # 14344883 matched (89% of all placesobs)
# openarch[EVENT_PLACE_ST %in% top$top_st, .N, by = EVENT_PLACE_ST] # 2427 (10.7%)
# openarch[EVENT_PLACE_ST %in% top$top_st, .N, ]  # 14535683 (90.6%)

# preferred strategy is first match on top+mun (because of multiple top with same name) 
# only does slightly worse than toponym but less false positives (Zwolle, GL vs. Zwolle, Ov, etc.)
# remaining placenames on toponym 
# also: Vulsma 1988 states that recorded place of death should be municipality 

### 1: match double event_place_st string with top_st+munic_st 
# (to make sure the pn is also actually the munc: top is always munc but not vice versa)

places[, placeplace := paste0(EVENT_PLACE_ST, EVENT_PLACE_ST)]
top[, topmun := paste0(top_st, munic_st)]

places[EVENT_PLACE_ST == "oploostanthonisenledeacker", 
       placeplace := "oploooploostanthonisenledeacker",] # only municipality whose name is not among corresponding toponyms

places[placeplace %in% top$topmun, match := 1]

# merge places with top for match1
m1 <- merge(
    places[match == 1, c("EVENT_PLACE", "EVENT_PLACE_ST", "placeplace", "N", "min_place_year", "max_place_year", "match")],
    top[, c("toponym", "municipality", "topmun", "amco")],
    by.x = "placeplace",
    by.y = "topmun",
    all.x = FALSE,
    all.y = FALSE)

m1 <- m1[!duplicated(m1, fromLast=TRUE),]
m1[, id := paste0(EVENT_PLACE, amco)]
m1 <- m1[!duplicated(id, fromLast=TRUE),]
m1[, id := NULL]

# check if all matches are still there
places[match == 1 & !(placeplace %in% m1$placeplace),]

# take out all remaining double placenames/amco for manual resolving and keep matches in m1
dup_placenames_m1 <- m1[duplicated(EVENT_PLACE, fromLast = F) | duplicated(EVENT_PLACE, fromLast = T),]
dup_placenames_m1[, placeplace := NULL]

m1 <- m1[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)] # 1803 pn, 1434488 obs matched
m1[, placeplace := NULL]
places[, placeplace := NULL]

### 2: match event_place with top_st+munic_st (often pn is already combination of top+mun in openarch)

top[, topmun := paste0(top_st,munic_st)]

places[EVENT_PLACE_ST %in% top$topmun & is.na(match), match := 2]

m2 <- merge(
    places[match == 2,c("EVENT_PLACE", "EVENT_PLACE_ST", "N", "min_place_year", "max_place_year", "match")], 
    top[, c("toponym", "municipality", "topmun", "amco")], 
    by.x = "EVENT_PLACE_ST", 
    by.y = "topmun", 
    all.x = FALSE, 
    all.y = FALSE)

m2 <- m2[!duplicated(m2, fromLast=TRUE), ]
m2[, id := paste0(EVENT_PLACE,amco)]
m2<- m2[!duplicated(id, fromLast=TRUE), ]
m2[, id := NULL]

# check if all matches are still there
places[match == 2 & !(EVENT_PLACE %in% m2$EVENT_PLACE), ]

# take out all remaining double placenames/amco for manual resolving and keep matches in m2

dup_placenames_m2 <- m2[duplicated(EVENT_PLACE, fromLast = FALSE) | duplicated(EVENT_PLACE, fromLast = TRUE), ]

m2 <- m2[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)] # 2020 pn, 722386 obs matched


### 3: match event_place_st with top_st

places[EVENT_PLACE_ST %in% top$top_st & is.na(match), match := 3,] # 1065 pn, 190654 obs

m3 <- merge(
    places[match == 3, c("EVENT_PLACE", "EVENT_PLACE_ST", "N", "min_place_year", "max_place_year", "match")], 
    top[,c("toponym", "municipality", "top_st", "amco")], 
    by.x = "EVENT_PLACE_ST", 
    by.y = "top_st", 
    all.x = FALSE, 
    all.y = FALSE)

m3 <- m3[match == 3,]
m3 <- m3[!duplicated(m3, fromLast=TRUE),]
m3[, id := paste0(EVENT_PLACE,amco)]
m3<- m3[!duplicated(id, fromLast=TRUE), ]
m3[, id := NULL]

# check if all matches are still there
places[match == 3 & !(EVENT_PLACE %in% m3$EVENT_PLACE), ]

# take out all remaining double placenames/amco for manual resolving and keep matches in m3

dup_placenames_m3 <- m3[duplicated(EVENT_PLACE, fromLast = FALSE) | duplicated(EVENT_PLACE, fromLast = TRUE),]

m3 <- m3[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)] # 412 pn, 722386 obs matched

### 4: match string after "gem" (short for municipality) with topmun:

places[, gem := as.character(lapply(strsplit(as.character(EVENT_PLACE_ST), split="gem"),tail, n=1))]
places[, gem := paste0(gem,gem)]

top[, topmun := paste0(top_st,munic_st)]

places[gem %in% top$topmun & is.na(match), match := 4,] # 1270 pn, 363917 obs

m4 <- places[match == 4,]
m4 <- merge(
    m4[, c("EVENT_PLACE", "EVENT_PLACE_ST", "gem", "N", "min_place_year", "max_place_year", "match")], 
    top[, c("toponym", "municipality", "topmun", "amco")], 
    by.x = "gem", 
    by.y = "topmun", 
    all.x = FALSE, 
    all.y = FALSE)

m4 <- m4[!duplicated(m4, fromLast=T),]
m4[, id := paste0(EVENT_PLACE,amco)]
m4 <- m4[!duplicated(id, fromLast=T),]
m4[, id := NULL]

# check if all matches are still there

places[match == 4 & !(EVENT_PLACE %in% m4$EVENT_PLACE), ] 

# take out all remaining double placenames/amco for manual resolving and keep matches in m4

dup_placenames_m4 <- m4[duplicated(EVENT_PLACE, fromLast = F) | duplicated(EVENT_PLACE, fromLast = T),]

m4 <- m4[!(duplicated(EVENT_PLACE) | duplicated(EVENT_PLACE, fromLast = TRUE)), ][order(-N)]
m4[, gem := NULL]
places[, gem := NULL]
dup_placenames_m4[, gem := NULL]

#### bind matches, conflicts, and to do's

openarch_places_matched <- rbindlist(list(m1,m2,m3,m4), use.names = TRUE)

openarch_places_conflicts <- rbindlist(list(dup_placenames_m1,dup_placenames_m2,dup_placenames_m3,dup_placenames_m4), use.names = TRUE)

openarch_places_todo <- places[!(EVENT_PLACE %in% openarch_places_matched$EVENT_PLACE) 
                             & !(EVENT_PLACE %in% openarch_places_conflicts$EVENT_PLACE), ]

### say we code remaining unmatched/conflicts manually only when N >= 100, that gets the obs coded up to well over 99%

openarch_places_todo[N >= 100, ]
openarch_places_todo[N >= 100, sum(N)]

openarch_places_conflicts[!duplicated(EVENT_PLACE, fromLast = T) & N >=100,]
openarch_places_conflicts[!duplicated(EVENT_PLACE, fromLast = T) & N >=100, sum(N)]

### remove discarded

rm(m1,m2,m3,m4)
rm(dup_placenames_m1,dup_placenames_m2,dup_placenames_m3,dup_placenames_m4)

# fix duplicate "Heugem (Gronsveld)"

openarch_places_matched[EVENT_PLACE == "Heugem (Gronsveld)" & toponym == "Gronsveld", amco := NA]

openarch_places_matched <- openarch_places_matched[!is.na(amco)]

### write to csv

fwrite(openarch_places_matched, "openarch_death_places_matched.csv", sep = ";")
fwrite(openarch_places_conflicts, "openarch_death_places_conflicts.csv", sep = ";")
fwrite(openarch_places_todo, "openarch_death_places_todo.csv", sep = ";")

### use matched file to get amco's for death certificates

openarch_places_matched <- fread("openarch_death_places_matched.csv")

keep = !duplicated(colnames(openarch))
openarch <- openarch[, ..keep, with = FALSE] # remove duplicated cols before merge (EVENT_TYPE & SOURCE_PLACE)

openarch_amco <- merge(
    openarch, 
    openarch_places_matched[, c("EVENT_PLACE","toponym", "municipality","amco", "match")], 
    by = "EVENT_PLACE", 
    all.x = TRUE, 
    all.y = FALSE)

openarch_amco[!is.na(amco), .N] #check, should be 1,52million

rm(openarch) # clear some mem

# seperate amco cert from non amco cert

openarch_noamco <- openarch_amco[is.na(amco),]
openarch_noamco[, amco := NULL]
openarch_noamco[, municipality := NULL]
openarch_noamco[, toponym := NULL]
openarch_noamco[, match := NULL] 

openarch_amco <- openarch_amco[!is.na(amco),] 

### use provinces to get amco for unmatched

setDT(openarch_noamco)
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "AlleFriezen", province := "Friesland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "AlleGroningers", province := "Groningen"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Archief Delft", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Archief Eemland", province := "Utrecht"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Brabants Historisch Informatie Centrum", province := "Noord-Brabant"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Erfgoed Leiden", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Erfgoed Leiden", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Erfgoed Leiden en omstreken", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gelders Archief", province := "Gelderland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeente Venray, gemeentearchief", province := "Limburg"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Borsele", province := "Zeeland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Ede", province := "Gelderland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Hengelo", province := "Overijssel"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Kerkrade", province := "Limburg"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Schiedam", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Schouwen-Duiveland", province := "Zeeland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Venlo", province := "Limburg"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Wassenaar", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Gemeentearchief Zaanstad", province := "Noord-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Haags Gemeentearchief", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Het Utrechts Archief", province := "Utrecht"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Historisch Centrum Leeuwarden", province := "Friesland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Historisch Centrum Overijssel", province := "Overijssel"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Nationaal Archief Rijksarchief Zuid-Holland", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Nieuw Land Erfgoedcentrum", province := "Flevoland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Noord-Hollands Archief", province := "Noord-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal Archief Alkmaar", province := "Noord-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal Archief Dordrecht", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal Archief Rivierenland", province := "Gelderland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal archief Tilburg", province := "Noord-Brabant"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal Archief Tilburg", province := "Noord-Brabant"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal Historisch Centrum Eindhoven", province := "Noord-Brabant"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal Historisch Centrum Limburg", province := "Limburg"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Regionaal Historisch Centrum Vecht en Venen", province := "Utrecht"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "RHC Rijnstreek en Lopikerwaard", province := "Utrecht"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Rijckheyt, centrum voor regionale geschiedenis", province := "Limburg"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Stadsarchief Breda", province := "Noord-Brabant"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Stadsarchief Enschede", province := "Overijssel"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Stadsarchief Rotterdam", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Streekarchief Voorne-Putten", province := "Zuid-Holland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "West Brabants Archief", province := "Noord-Brabant"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Zeeuws Archief", province := "Zeeland"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Drents Archief", province := "Drenthe"]
openarch_noamco[ SOURCEREFERENCE_INSTITUTIONNAME == "Streekarchief Langstraat Heusden Altena", province := "Noord-Brabant"]

# 5: match placeprov with munprov

top[, munprov := paste0(munic_st, province)]

openarch_noamco[, placeprov := paste0(EVENT_PLACE_ST,province)]

m5 <- openarch_noamco[placeprov %in% top$munprov, list(EVENT_PLACE, EVENT_PLACE_ST, placeprov)]

m5 <- m5[!duplicated(m5, fromLast = TRUE), ]

m5 <- merge(m5, 
    top[,c("amco", "municipality", "toponym", "munprov")], 
    by.x = "placeprov",
    by.y = "munprov", 
    all.x = TRUE, 
    all.y = FALSE)

m5 <- m5[!duplicated(m5, fromLast = TRUE), ]
m5[, id := paste0(placeprov,amco)]
m5 <- m5[!duplicated(id, fromLast = TRUE), ]

# remove double placeplaceprov, because of double amco (manual resolve)
m5 <- m5[!duplicated(placeprov, fromLast = FALSE) & !duplicated(placeprov, fromLast = TRUE)]

# merge m5 with openarch_noamco on placeprov
openarch_noamco <- merge(
    m5[,c("amco", "toponym", "municipality", "placeprov")], 
    openarch_noamco, 
    by = "placeprov", 
    all.y = TRUE, 
    all.x = TRUE)

openarch_noamco[!is.na(placeprov) & !is.na(amco), match := 5]
                
openarch_noamco[, placeprov := NULL]
openarch_noamco[, placeplace := NULL]


# 6: match placeprov with topprov

top[, topprov := paste0(top_st, province)]
openarch_noamco[, placeprov := paste0(EVENT_PLACE_ST, province)]

m6 <- openarch_noamco[placeprov %in% top$topprov & is.na(match), ]

m6[,  min_place_year := min(EVENT_YEAR, na.rm=T), by = EVENT_PLACE_ST]
m6[,  max_place_year := max(EVENT_YEAR, na.rm=T), by = EVENT_PLACE_ST]

m6 <- m6[, list(EVENT_PLACE, EVENT_PLACE_ST, placeprov, min_place_year, max_place_year) ]
m6 <- m6[!duplicated(m6, fromLast = TRUE), ]

m6 <- merge(m6, 
    top[,c("amco", "municipality", "toponym", "topprov", "first_year", "last_year")], 
    by.x = "placeprov",
    by.y = "topprov", 
    all.x = TRUE, 
    all.y = FALSE)

m6 <- m6[!duplicated(m6, fromLast = TRUE), ]

m6 <- m6[min_place_year >= first_year 
       & max_place_year <= last_year, ] # here we need to work with min max year, because of multiple top/prov combinations

# remove remaining double placeprov , because of double amco (manual resolve)
m6 <- m6[!duplicated(placeprov, fromLast = FALSE) & !duplicated(placeprov, fromLast = TRUE)]

# merge m6 with noamco
openarch_noamco <- merge(
    m6[,c("amco", "toponym", "municipality", "placeprov")], 
    openarch_noamco, 
    by = "placeprov", 
    all.y = TRUE, 
    all.x = TRUE)

openarch_noamco[is.na(amco.y), amco.y := amco.x]
openarch_noamco[is.na(municipality.y), municipality.y := municipality.x]
openarch_noamco[is.na(toponym.y), toponym.y := toponym.x]

setnames(openarch_noamco, "amco.y", "amco")
setnames(openarch_noamco, "municipality.y", "municipality")
setnames(openarch_noamco, "toponym.y", "toponym")

openarch_noamco[!is.na(placeprov) & !is.na(amco) & is.na(match), match := 6]

todo <- openarch_noamco[is.na(amco), .N, list(EVENT_PLACE, EVENT_PLACE_ST,province)][order(-N)]

fwrite(todo, "openarch_death_todo_placenames_afterm6.csv", sep = ";")

openarch_noamco[, placeprov := NULL]
openarch_noamco[, municipality.x := NULL]
openarch_noamco[, amco.x := NULL]
openarch_noamco[, toponym.x := NULL]
openarch_noamco[, province := NULL]

### other combinations do not add much

# check remaining todo's > seem to be combination of multiple amco's for same pn+province combination (Hoogeveen,ZH/ Hoogeveen,Dr)
# easily coded manually

### bind noamco with openarch_amco

openarch_matched <- rbindlist(list(openarch_amco, openarch_noamco), use.names = TRUE)

rm(openarch_amco, openarch_noamco)

### 7: code placenames with N >= 5000 manually

# if multiple amco's for same choice (top/mun/prov or mun/prov or top/prov in descending order of priority): 
# we go for the amco falling within the year range of the pn (mostly the older ones before 1960-90s merges)

openarch_matched[EVENT_PLACE == "Kollumerland c.a.", amco := 10984]
openarch_matched[EVENT_PLACE == "Kollumerland c.a.", toponym := "Kollum"]
openarch_matched[EVENT_PLACE == "Kollumerland c.a.", municipality := "Kollumerland en Nieuwkruisland"]

openarch_matched[EVENT_PLACE == "Roosendaal - Nispen", amco := 10407]
openarch_matched[EVENT_PLACE == "Roosendaal - Nispen", toponym := "Roosendaal en Nispen"]
openarch_matched[EVENT_PLACE == "Roosendaal - Nispen", municipality := "Roosendaal en Nispen"]

openarch_matched[EVENT_PLACE == "Rijswijk" & SOURCEREFERENCE_INSTITUTIONNAME == "Archief Delft", amco := 11133]
openarch_matched[EVENT_PLACE == "Rijswijk" & SOURCEREFERENCE_INSTITUTIONNAME == "Archief Delft", toponym := "Rijswijk"]
openarch_matched[EVENT_PLACE == "Rijswijk" & SOURCEREFERENCE_INSTITUTIONNAME == "Archief Delft", municipality := "Rijswijk"]

openarch_matched[EVENT_PLACE == "Rijswijk" & SOURCEREFERENCE_INSTITUTIONNAME == "Brabants Historisch Informatie Centrum", amco := 11016]
openarch_matched[EVENT_PLACE == "Rijswijk" & SOURCEREFERENCE_INSTITUTIONNAME == "Brabants Historisch Informatie Centrum", toponym := "Rijswijk"]
openarch_matched[EVENT_PLACE == "Rijswijk" & SOURCEREFERENCE_INSTITUTIONNAME == "Brabants Historisch Informatie Centrum", municipality := "Rijswijk"]

openarch_matched[EVENT_PLACE == "Almelo, Stad", amco := 11053]
openarch_matched[EVENT_PLACE == "Almelo, Stad", toponym := "Almelo"]
openarch_matched[EVENT_PLACE == "Almelo, Stad", municipality := "Almelo"]

openarch_matched[EVENT_PLACE == "Almelo, Ambt" & EVENT_YEAR <= 1913, amco := 11065]
openarch_matched[EVENT_PLACE == "Almelo, Ambt" & EVENT_YEAR <= 1913, toponym := "Ambt Almelo"]
openarch_matched[EVENT_PLACE == "Almelo, Ambt" & EVENT_YEAR <= 1913, municipality := "Ambt Almelo"]

openarch_matched[EVENT_PLACE == "Almelo, Ambt" & EVENT_YEAR >= 1914, amco := 11053]
openarch_matched[EVENT_PLACE == "Almelo, Ambt" & EVENT_YEAR >= 1914, toponym := "Ambt Almelo"]
openarch_matched[EVENT_PLACE == "Almelo, Ambt" & EVENT_YEAR >= 1914, municipality := "Ambt Almelo"]

openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "Drents Archief", amco := 10839]
openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "Drents Archief", toponym := "Hoogeveen"]
openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "Drents Archief", municipality := "Hoogeveen"]

openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "Historisch Centrum Overijssel", amco := 10839]
openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "Historisch Centrum Overijssel", toponym := "Hoogeveen"]
openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "Historisch Centrum Overijssel", municipality := "Hoogeveen"]

openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "AlleGroningers", amco := 10839]
openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "AlleGroningers", toponym := "Hoogeveen"]
openarch_matched[EVENT_PLACE == "Hoogeveen" & SOURCEREFERENCE_INSTITUTIONNAME == "AlleGroningers", municipality := "Hoogeveen"]

openarch_matched[EVENT_PLACE == "Dinteloord", amco := 11193]
openarch_matched[EVENT_PLACE == "Dinteloord", toponym := "Dinteloord"]
openarch_matched[EVENT_PLACE == "Dinteloord", municipality := "Dinteloord en Prinsenland"]

openarch_matched[EVENT_PLACE == "Serooskerke (Walcheren)", amco := 10369]
openarch_matched[EVENT_PLACE == "Serooskerke (Walcheren)", toponym := "Serooskerke"]
openarch_matched[EVENT_PLACE == "Serooskerke (Walcheren)", municipality := "Veere"]

openarch_matched[EVENT_PLACE == "Blerick", amco := 10477]
openarch_matched[EVENT_PLACE == "Blerick", toponym := "Hout-Blerick"]
openarch_matched[EVENT_PLACE == "Blerick", municipality := "Venlo"]

openarch_matched[EVENT_PLACE == "Vessem", amco := 11414 ]
openarch_matched[EVENT_PLACE == "Vessem", toponym := "Vessem"]
openarch_matched[EVENT_PLACE == "Vessem", municipality := "Vessem, Wintelre en Knegsel"]

openarch_matched[EVENT_PLACE == "Stad Doetinchem (Doetinchem)" & EVENT_YEAR <= 1919, amco := 10603 ]
openarch_matched[EVENT_PLACE == "Stad Doetinchem (Doetinchem)" & EVENT_YEAR <= 1919, toponym := "Doetinchem"]
openarch_matched[EVENT_PLACE == "Stad Doetinchem (Doetinchem)" & EVENT_YEAR <= 1919, municipality := "Stad Doetinchem"]

openarch_matched[EVENT_PLACE == "Stad Doetinchem (Doetinchem)" & EVENT_YEAR >= 1920, amco := 10396 ]
openarch_matched[EVENT_PLACE == "Stad Doetinchem (Doetinchem)" & EVENT_YEAR >= 1920, toponym := "Doetinchem"]
openarch_matched[EVENT_PLACE == "Stad Doetinchem (Doetinchem)" & EVENT_YEAR >= 1920, municipality := "Doetinchem"]

openarch_matched[EVENT_PLACE == "Ommerschans (Ommen, Stad)", amco := 11069 ]
openarch_matched[EVENT_PLACE == "Ommerschans (Ommen, Stad)", toponym := "Ommerschans"]
openarch_matched[EVENT_PLACE == "Ommerschans (Ommen, Stad)", municipality := "Ommen"]

openarch_matched[EVENT_PLACE == "Delden, Stad", amco := 10913 ]
openarch_matched[EVENT_PLACE == "Delden, Stad", toponym := "Delden"]
openarch_matched[EVENT_PLACE == "Delden, Stad", municipality := "Stad Delden"]

openarch_matched[EVENT_PLACE == "Gestel" & EVENT_YEAR <= 1919, amco := 11269 ]
openarch_matched[EVENT_PLACE == "Gestel" & EVENT_YEAR <= 1919, toponym := "Gestel"]
openarch_matched[EVENT_PLACE == "Gestel" & EVENT_YEAR <= 1919, municipality := "Gestel en Blaarthem"]

openarch_matched[EVENT_PLACE == "Gestel" & EVENT_YEAR >= 1920, amco := 11298 ]
openarch_matched[EVENT_PLACE == "Gestel" & EVENT_YEAR >= 1920, toponym := "Gestel"]
openarch_matched[EVENT_PLACE == "Gestel" & EVENT_YEAR >= 1920, municipality := "Eindhoven"]

openarch_matched[EVENT_PLACE == "Herwen en Aerdt (Rijnwaarden)", amco := 10819 ]
openarch_matched[EVENT_PLACE == "Herwen en Aerdt (Rijnwaarden)", toponym := "Herwen en Aerdt"]
openarch_matched[EVENT_PLACE == "Herwen en Aerdt (Rijnwaarden)", municipality := "Herwen en Aerdt"]

openarch_matched[EVENT_PLACE == "Arcen", amco := 10202 ]
openarch_matched[EVENT_PLACE == "Arcen", toponym := "Arcen"]
openarch_matched[EVENT_PLACE == "Arcen", municipality := "Arcen en Velden"]

openarch_matched[grepl("deldenstad", EVENT_PLACE_ST), amco := 10913 ]
openarch_matched[grepl("deldenstad", EVENT_PLACE_ST), toponym := "Delden"]
openarch_matched[grepl("deldenstad", EVENT_PLACE_ST), municipality := "Stad Delden"]
openarch_matched[grepl("deldenstad", EVENT_PLACE_ST), match := 7]

openarch_matched[grepl("deldenambt", EVENT_PLACE_ST), amco := 11400 ]
openarch_matched[grepl("deldenambt", EVENT_PLACE_ST), toponym := "Ambt Delden"]
openarch_matched[grepl("deldenambt", EVENT_PLACE_ST), municipality := "Ambt Delden"]
openarch_matched[grepl("deldenambt", EVENT_PLACE_ST), match := 7]

openarch_matched[grepl("beusichemburen", EVENT_PLACE_ST), amco := 11182 ]
openarch_matched[grepl("beusichemburen", EVENT_PLACE_ST), toponym := "Beusichem"]
openarch_matched[grepl("beusichemburen", EVENT_PLACE_ST), municipality := "Beusischem"]
openarch_matched[grepl("beusichemburen", EVENT_PLACE_ST), match := 7]

# generate match # for these manual matches > 7
openarch_matched[!is.na(amco) & is.na(match), match := 7]

# write csv 

fwrite(
    x = openarch_matched, 
    file = "openarch_deaths_amco.csv.gz", 
    sep = ";", 
    compress = "gzip")
