# reshape openarch data into zeeland challenge format

rm(list = ls())

library("data.table")
library("stringi")

target_vrbs = fread("LINKS_Zeeland_cleaned_2016_01_Persons.csv", nrow = 1, header = FALSE)
target = fread(cmd = "iconv -f latin1 -t utf8 LINKS_Zeeland_cleaned_2016_01_Persons.csv | grep -P '^\\d+;\\d+;2'")

openarch = fread("gunzip -c openarch_birth.csv.gz")
openarch[, id_registration := .I]

openarch[, birth_date := as.Date(
    stri_join(EVENT_YEAR, "-", EVENT_MONTH, "-", EVENT_DAY),
    format = "%Y-%m-%d")]
openarch[, registration_date := as.Date(
    stri_join(SOURCE_DATE_YEAR, "-", SOURCE_DATE_MONTH, "-", SOURCE_DATE_DAY),
    format = "%Y-%m-%d")]

# proxy birth from registration date and flag
openarch[, birth_date_flag := ifelse(is.na(birth_date), 0, 1)]
openarch[is.na(birth_date), birth_date := registration_date]
openarch[birth_date_flag == 0 & !is.na(birth_date), birth_date_flag := 2]
openarch[, .N, by = birth_date_flag]
# 0 no date
# 1 valid date
# 2 registriation date
# 3 broken birth date, registration date used

x = melt(openarch, 
    id.vars = c("id_registration", 
                "birth_date",
                "registration_date",
                "EVENT_PLACE"),
    measure.vars = patterns(firstname = "_NAME_GN", 
                            prefix = "_NAME_SPRE",
                            familyname = "_NAME_SURN",
                            # occupation
                            age_year = "_AGE",
                            sex = "_GENDER",
                            bir_year = "_BIR_YEAR", # maybe prebake this and below, only PR
                            bir_month = "_BIR_MONTH",
                            bir_day = "_BIR_DAY",
                            birth_location = "_BIR_PLACE"))

# rm("openarch") # some operations below are mem hungry

# variable -> role
# 1 -> 1
# 3 -> 2 # links mother = 2, openarch mother == 3
# 2  -> 3
x[variable == 1, role := 1]
x[variable == 2, role := 3] # father
x[variable == 3, role := 2] # mother
x[, variable := NULL]

# birth date from registration date only makes sense for newborn
x[role != 1, birth_date := NA]

openarch[, birth_date2 := as.Date(
    stri_join(bir_year, "-", bir_month, "-", bir_day),
    format = "%Y-%m-%d")]

# get from bir_* (61 cases only)
x[role == 1 & is.na(birth_date), birth_date := birth_date2]

# 700k date2 != date, resolve where possible

# fixes 6
x[role == 1 
    & birth_date != birth_date2
    & (abs(birth_date - birth_date2) > 5) 
    & (abs(birth_date2 - registration_date) <= 5), 
    birth_date := registration_date]
# can't resolve, NA
x[role == 1
    & birth_date != birth_date2 
    & (abs(birth_date - birth_date2) > 5)
    & (abs(birth_date2 - registration_date) > 5)
    & (abs(birth_date - registration_date) > 5),
    birth_date := NA]
# can't resolve, NA
x[role == 1
    & birth_date != birth_date2 
    & (abs(birth_date - birth_date2) > 5)
    & is.na(registration_date),
    birth_date := NA]

# x[, mindif := pmin(abs(birth_date - registration_date), abs(birth_date - birth_date2), na.rm = TRUE)]

x[, bir_year := NULL]
x[, bir_month := NULL]
x[, bir_day := NULL]

setnames(x, "EVENT_PLACE", "birth_location")

# civil_status -> empty
x[, civil_status := NA]

# birth_date_flag 0 if empty 1 if good 2/3 if proxied, supplement birth_date?
# keep NA, doesn't make sense to impute birth dates on a marriage certificate
# here reverse, marriage date empty
x[, mar_date_flag := NA]

# recode sex + impute from role
x[sex == "Man", sex := "m"]
x[sex == "Vrouw", sex := "f"]
x[sex == "Onbekend", sex := "u"]
x[sex == "", sex := NA]
x[is.na(sex) & role %in% c(2) & firstname != "" & familyname != "", 
    sex := "f"]
x[is.na(sex) & role %in% c(3) & firstname != "" & familyname != "", 
    sex := "m"]
# is.na(firstname)

# tolower
x[, firstname := stringi::stri_trans_tolower(firstname)]
x[, prefix := stringi::stri_trans_tolower(prefix)]
x[, familyname := stringi::stri_trans_tolower(familyname)]

# nb: Latin-ASCII still includes e.g. <
x[1:1e6][firstname != ""][stri_detect_regex(firstname, "[^[:alpha:] .-]"), .(firstname)]

# is
# josephus michaa\u0080°l
# present before trans?

# strip diacretics
x[, firstname := stringi::stri_trans_general(firstname, "Latin-ASCII")]
x[, familyname := stringi::stri_trans_general(familyname, "Latin-ASCII")]
x[, prefix := stringi::stri_trans_general(firstname, "Latin-ASCII")]

x[, fullname := paste0(firstname, prefix, familyname, sep = " ")]
fwrite(
    x[fullname != ""][stri_detect_regex(fullname, "[^A-z\\s\\-.'’]"), .(unique(fullname))],
    "badnames.csv",
    append = TRUE)
x[, fullname := NULL]

# encoding check
x[, all(validUTF8(firstname))]
x[, all(validUTF8(familyname))]
x[, all(validUTF8(prefix))]
x[, all(validUTF8(birth_location))]

# type of source
x[, registration_maintype := 1] # 2 marriage, 1 births, 3 deaths

x[, death := NA] # should be a or n
    # The value ‘a’ is deduced from role 1 (birth by definition alive), or role 4
    # (bride), 7 (bridegroom) or 10 (deceased) with an age in days, weeks, months
    # or years and/or having an occupational title.

x[, stillbirth := NA] # event remark? source remark?
    # y in the firstname one can find terms such as ‘levenloos’
    # y-r lifeless-reported was retrieved from the remarks in the registration
# x[role == 1 & firstname == "levenloos"]
# x[role == 1 & firstname == "doodgeboren"]
# etc, but Kees will have a table?

x[, occupation := NA] # todo
x[, death_year := NA]
x[, death_date_flag := NA]
x[, death_location := NA]

fwrite(x, "~/downloads/Zeeland_Challenge/openarch_persons_births.csv.gz")
