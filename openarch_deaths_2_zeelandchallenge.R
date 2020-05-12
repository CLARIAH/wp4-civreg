# reshape openarch data into zeeland challenge format

rm(list = ls())

library("data.table")
library("stringi")

setDTthreads(threads = 8)

openarch = fread("/data/auke/civreg/openarch/openarch_deaths_dedup.csv")

# replace "" by NA for character variables
for (j in 1:ncol(openarch)){
    if (class(openarch[[j]]) != "character") next
    set(x = openarch, i = which(openarch[[j]] == ""), j = j, NA)
}

openarch[, id_registration := .I]

# as.Date fastest
# NA propagates in as.Date, though maybe something is to be said for making something out of year/month
openarch[, death_date := as.Date(
    stri_join(EVENT_YEAR, "-", EVENT_MONTH, "-", EVENT_DAY),
    format = "%Y-%m-%d")]

openarch[, registration_date := as.Date(
    stri_join(SOURCE_DATE_YEAR, "-", SOURCE_DATE_MONTH, "-", SOURCE_DATE_DAY),
    format = "%Y-%m-%d")]

# proxy marriage from registration date and flag
openarch[, death_date_flag := ifelse(is.na(death_date), 0, 1)]
openarch[is.na(death_date), death_date := registration_date]
openarch[death_date_flag == 0 & !is.na(death_date), death_date_flag := 2]
openarch[, .N, by = death_date_flag]

# set relevant variables to cleaned ones to preserve patterns
openarch[, PR_AGE := PR_AGE_year]
openarch[, PR_GENDER := PR_GENDER_2]
openarch[, EVENT_PLACE := AMCO_EVENT_PLACE]

setkey(openarch, V1)

# this takes about 10-15m
openarch_deduplicated = openarch[duplicate == TRUE, 
    lapply(.SD, function(x) x[!is.na(x)][1]), 
    by = V1,
    .SDcols = patterns("^id_registration$|^death_date$|^EVENT_YEAR$|^EVENT_PLACE$|_NAME_GN$|_NAME_SPRE$|_NAME_SURN$|_AGE$|_BIR_YEAR$|_BIR_MONTH$|_BIR_DAY$|_BIR_PLACE$|_GENDER$")]

# minute or two
openarch = rbindlist(
    list(openarch[duplicate == FALSE],
         openarch_deduplicated),
    fill = TRUE)

# source_place is in there twice, identical though
x = melt(openarch, 
    id.vars = c("id_registration", "V1", "death_date", "EVENT_PLACE", "EVENT_YEAR"),
    measure.vars = patterns(firstname = "_NAME_GN$", 
                            prefix = "_NAME_SPRE$",
                            familyname = "_NAME_SURN$",
                            # occupation
                            age_year = "_AGE$",
                            bir_year = "_BIR_YEAR", # maybe prebake this, only bride and groom
                            bir_month = "_BIR_MONTH",
                            bir_day = "_BIR_DAY",
                            birth_location = "_BIR_PLACE",
                            sex = "_GENDER"))

# rm("openarch") # some operations below are mem hungry

x[, id_person := .I]

# variable -> role
x[variable == 1, role := 10] # deceased
x[variable == 2, role := 3] # father
x[variable == 3, role := 2] # mother
# reporter never included?
x[, variable := NULL]

x[, birth_date := as.Date(
    stri_join(bir_year, "-", bir_month, "-", bir_day),
    format = "%Y-%m-%d")]
x[, bir_year := NULL]
x[, bir_month := NULL]
x[, bir_day := NULL]
# these are only removed because schema does not use them
# todo: see what info can be recovered

setnames(x, "EVENT_PLACE", "death_location")
setnames(x, "EVENT_YEAR", "death_year")

# civil_status -> empty
x[, civil_status := NA]

# birth_date_flag 0 if empty 1 if good 2/3 if proxied, supplement birth_date?
# keep NA, doesn't make sense to impute birth dates on a marriage certificate
x[, birth_date_flag := NA]

# recode sex + impute from role
# first four unnecessary now that it is pre-standardised
# x[sex == "Man", sex := "m"]
# x[sex == "Vrouw", sex := "f"]
# x[sex == "Onbekend", sex := "u"]
# x[sex == "", sex := NA]
x[is.na(sex) & role %in% c(2) & firstname != "" & familyname != "", 
    sex := "f"]
x[is.na(sex) & role %in% c(3) & firstname != "" & familyname != "", 
    sex := "m"]
# is.na(firstname)

# tolower
x[, firstname := stringi::stri_trans_tolower(firstname)]
x[, prefix := stringi::stri_trans_tolower(prefix)]
x[, familyname := stringi::stri_trans_tolower(familyname)]

# strip diacretics
# much time!
x[, firstname := stringi::stri_trans_general(firstname, "Latin-ASCII")]
x[, familyname := stringi::stri_trans_general(familyname, "Latin-ASCII")]
x[, prefix := stringi::stri_trans_general(prefix, "Latin-ASCII")]

# x[, fullname := paste0(firstname, prefix, familyname, sep = " ")]
# fwrite(
#     x[fullname != ""][stri_detect_regex(fullname, "[^A-z\\s\\-.'’]"), .(unique(fullname))],
#     "badnames.csv",
#     append = TRUE)
# x[, fullname := NULL]

# encoding check
x[, all(validUTF8(firstname))]
x[, all(validUTF8(familyname))]
x[, all(validUTF8(prefix))]
# x[, all(validUTF8(death_location))] now amco

# type of source
x[, registration_maintype := 3] # 2 marriage, 1 births, 3 deaths

x[, death := "n"] # always no/unknown
x[, stillbirth := NA]
x[, occupation := NA] # todo
# x[, death_year := NA]
# x[, death_date_flag := NA]
# x[, death_location := NA]

fwrite(x, "openarch_persons_deaths.csv.gz")
