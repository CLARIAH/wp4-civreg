# reshape openarch data into zeeland challenge format

library("data.table")
library("stringi")

target = fread("~/Downloads/Zeeland_Challenge/LINKS_Zeeland_cleaned_2016_01_csv_files/LINKS_Zeeland_cleaned_2016_01_Persons.csv",
    nrows = 10)

openarch = fread("~/Downloads/Zeeland_Challenge/openarch_marriages.csv")

# like Ruben
# todo: make it something smarter and transportable
openarch[, id_registration := .I]

openarch[, mar_date := stri_join(EVENT_YEAR, "-", 
    stri_pad_left(EVENT_MONTH, width = 2, pad = "0"), "-", 
    stri_pad_left(EVENT_DAY, width = 2, pad = "0"))]

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
                            bir_place = "_BIR_PLACE",
                            sex = "_GENDER"))


x[id_registration == 5]
openarch[id_registration == 5]

rm("openarch") # some operations below are mem hungry

x[, birth_date := stri_join(bir_year, "-", 
    stri_pad_left(bir_month, width = 2, pad = "0"), "-", 
    stri_pad_left(bir_day, width = 2, pad = "0"))]
x[, bir_year := NULL]
x[, bir_month := NULL]
x[, bir_day := NULL]

setnames(x, "EVENT_PLACE", "mar_location")

# variable -> role
x[variable == 1, role := 7]
x[variable == 2, role := 4]
x[variable == 3, role := 9]
x[variable == 4, role := 8]
x[variable == 5, role := 5]
x[variable == 6, role := 6]
x[, variable := NULL]

# civil_status -> empty
x[, civil_status := NA]

# birth_date_flag 0 if empty 1 if good 2/3 if proxied, supplement birth_date?
# birth_date_flag 0 if empty 1 if good 2/3 if proxied
# birth_date_flag 0 if empty 1 if good 2/3 if proxied

# recode sex + impute from role
    # man vrouw onbekend leeg
    # male
    # female
    # unknown
    # NULL (also unknown)

# tolower
x[, firstname := stringi::stri_trans_tolower(firstname)]
x[, prefix := stringi::stri_trans_tolower(prefix)]
x[, familyname := stringi::stri_trans_tolower(familyname)]

# strip diacretics
x[, firstname := stringi::stri_trans_general(firstname, "Latin-ASCII")]
x[, familyname := stringi::stri_trans_general(familyname, "Latin-ASCII")]
x[, prefix := stringi::stri_trans_general(firstname, "Latin-ASCII")]

# encoding check

# type of source
x[, registration_maintype := 2] # 2 marriage, 1 births, 3 deaths


# # id person: make after melt
# firstname NAME_GN
# prefix NAME_SPRE
# familyname NAME_SURN
# sex _GENDER
# civil_status x
# death
# stillbirth
# occupation x 
# birth_date BIR_DAY BIR_MONTH BIR_YEAR
# birth_date_flag
# birth_location BIR_PLACE
# mar_date EVENT_DAY EVENT_MONTH EVEN_YEAR
# mar_date_flag
# mar_location EVENT_PLACE
# death_year
# death_date_flag
# registration_maintype
# death_location

# # firstname 
# # prefix 
# # familyname 
# # sex 
# # civil_status
# role
# occupation
# age_day
# age_week
# age_month
# age_year

# target[!is.na]
# birth_date
# birth_date_flag
# birth_day
# birth_month
# birth_year
# birth_location
# death
# stillbirth
# death_date_flag
# death_day
# death_month
# death_year
# death_location
# mar_date
# mar_date_flag
# mar_day
# mar_month
# mar_year
# mar_location
# > 