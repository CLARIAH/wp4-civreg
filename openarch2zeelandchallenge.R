# reshape openarch data into zeeland challenge format

rm(list = ls())

library("data.table")
library("stringi")

target_vrbs = fread("LINKS_Zeeland_cleaned_2016_01_Persons.csv", nrow = 1, header = FALSE)
target = fread(cmd = "iconv -f latin1 -t utf8 LINKS_Zeeland_cleaned_2016_01_Persons.csv | grep -P '^\\d+;\\d+;2'")
openarch = fread(cmd = "gunzip -c openarch_marriage.csv.gz")

openarch[, id_registration := .I]

openarch[, mar_date := as.Date(
    stri_join(EVENT_YEAR, "-", EVENT_MONTH, "-", EVENT_DAY),
    format = "%Y-%m-%d")]

openarch[, registration_date := as.Date(
    stri_join(SOURCE_DATE_YEAR, "-", SOURCE_DATE_MONTH, "-", SOURCE_DATE_DAY),
    format = "%Y-%m-%d")]

# proxy marriage from registration date and flag
openarch[, mar_date_flag := ifelse(is.na(mar_date), 0, 1)]
openarch[is.na(mar_date), mar_date := registration_date]
openarch[mar_date_flag == 0 & !is.na(mar_date), mar_date_flag := 2]
openarch[, .N, by = mar_date_flag]

x = melt(openarch, 
    id.vars = c("id_registration", 
                "mar_date", 
                "EVENT_PLACE"),
    measure.vars = patterns(firstname = "_NAME_GN", 
                            prefix = "_NAME_SPRE",
                            familyname = "_NAME_SURN",
                            # occupation
                            age_year = "_AGE",
                            bir_year = "_BIR_YEAR", # maybe prebake this, only bride and groom
                            bir_month = "_BIR_MONTH",
                            bir_day = "_BIR_DAY",
                            birth_location = "_BIR_PLACE",
                            sex = "_GENDER"))

# rm("openarch") # some operations below are mem hungry

setnames(x, "EVENT_PLACE", "mar_location")

x[, birth_date := as.Date(
    stri_join(bir_year, "-", bir_month, "-", bir_day),
    format = "%Y-%m-%d")]
x[, bir_year := NULL]
x[, bir_month := NULL]
x[, bir_day := NULL]

# variable -> role
x[variable == 1, role := 7]
x[variable == 2, role := 4]
x[variable == 3, role := 9]
x[variable == 4, role := 8]
x[variable == 5, role := 6]
x[variable == 6, role := 5]
x[, variable := NULL]

# civil_status -> empty
x[, civil_status := NA]

# birth_date_flag 0 if empty 1 if good 2/3 if proxied, supplement birth_date?
# keep NA, doesn't make sense to impute birth dates on a marriage certificate
x[, birth_date_flag := NA]

# recode sex + impute from role
x[sex == "Man", sex := "m"]
x[sex == "Vrouw", sex := "f"]
x[sex == "Onbekend", sex := "u"]
x[sex == "", sex := NA]
x[is.na(sex) & role %in% c(2, 4, 5, 8) & firstname != "" & familyname != "", 
    sex := "f"]
x[is.na(sex) & role %in% c(3, 6, 7, 9) & firstname != "" & familyname != "", 
    sex := "m"]

# tolower
x[, firstname := stringi::stri_trans_tolower(firstname)]
x[, prefix := stringi::stri_trans_tolower(prefix)]
x[, familyname := stringi::stri_trans_tolower(familyname)]

# strip diacretics
# there are still some encoding issues, e.g. Ã\u009
x[, firstname := stringi::stri_trans_general(firstname, "Latin-ASCII")]
x[, familyname := stringi::stri_trans_general(familyname, "Latin-ASCII")]
x[, prefix := stringi::stri_trans_general(firstname, "Latin-ASCII")]

# list of messed up names
x[, fullname := paste0(firstname, prefix, familyname, sep = " ")]
fwrite(
    x[fullname != ""][stri_detect_regex(fullname, "[^A-z\\s\\-.'’]"), .(unique(fullname))],
    "badnames.csv")
x[, fullname := NULL]
# done here to get the full range, but ideally the encodings are fixed before thstri

# encoding check
x[, all(validUTF8(firstname))]
x[, all(validUTF8(familyname))]
x[, all(validUTF8(prefix))]
x[, all(validUTF8(mar_location))]

# type of source
x[, registration_maintype := 2] # 2 marriages, 1 births, 3 deaths

x[, death := NA]
x[, stillbirth := NA]
x[, occupation := NA] # todo
x[, death_year := NA]
x[, death_date_flag := NA]
x[, death_location := NA]

fwrite(x, "~/downloads/Zeeland_Challenge/openarch_persons_marriages.csv.gz")
