### GET DATA

# Download all bsg.csv.zip files from openarch using the url list at github/wp4civreg, 
# unzip and save as seperate csv's in working directory

library(data.table)

setwd("C:\\Users\\Ruben\\Desktop\\openarch\\birth")


urls <- readLines("https://raw.githubusercontent.com/CLARIAH/wp4-civreg/master/openarch_bsg_url_list_september2019.txt")


for (url in urls) {
  download.file(url, destfile = basename(url), method="curl")
  unzip(basename(url))
}

# remove .zip files from folder

unlink("*.zip", recursive = FALSE)

# combine all csv's into one data frame using selection of variables

file_list <- list.files()

datasets = list()

for (file in file_list){
  {
    datasets[[file]] <- fread(file, encoding="UTF-8")[, list(EVENT_DAY, EVENT_MONTH, EVENT_YEAR, EVENT_PLACE, 
                                                             SOURCE_DATE_DAY, SOURCE_DATE_MONTH, SOURCE_DATE_YEAR,
                                                             SOURCE_PLACE, PR_NAME_GN, PR_NAME_SPRE, PR_NAME_SURN, PR_NOTE,
                                                             PR_FTHR_NAME_GN, PR_FTHR_NAME_SPRE, PR_FTHR_NAME_SURN,
                                                             PR_FTHR_OCCUPATION,PR_FTHR_AGE, PR_MTHR_NAME_GN,	PR_MTHR_NAME_SPRE, 
                                                             PR_MTHR_NAME_SURN, PR_MTHR_OCCUPATION, PR_MTHR_AGE,
                                                             SOURCE_TYPE, SOURCEREFERENCE_PLACE,	
                                                             SOURCEREFERENCE_INSTITUTIONNAME, SOURCE_DIGITAL_ORIGINAL)]
  }}

# write to dataframe

openarch_birth <- rbindlist(datasets) # result should give same number of obs. as mentioned in openarch (currently 16,068,975)

# set data table

setDT(openarch_birth)

### DATES cleaning

# add “0” to days and months below 10 ("9" into "09")

openarch_birth[, EVENT_MONTH := ifelse(EVENT_MONTH < 10, paste0("0", EVENT_MONTH), as.character(EVENT_MONTH)), ]
openarch_birth[, EVENT_DAY := ifelse(EVENT_DAY < 10, paste0("0", EVENT_DAY), as.character(EVENT_DAY)), ]

openarch_birth[, SOURCE_DATE_MONTH := ifelse(SOURCE_DATE_MONTH < 10, paste0("0", SOURCE_DATE_MONTH), as.character(SOURCE_DATE_MONTH)), ]
openarch_birth[, SOURCE_DATE_DAY := ifelse(SOURCE_DATE_DAY < 10, paste0("0", SOURCE_DATE_DAY), as.character(SOURCE_DATE_DAY)), ]

# clean EVENT_YEAR

openarch_birth[, EVENT_YEAR := ifelse(is.na(EVENT_YEAR), SOURCE_DATE_YEAR, EVENT_YEAR), ]
openarch_birth[, EVENT_YEAR := ifelse((EVENT_YEAR < 1720 | EVENT_YEAR > 1930), SOURCE_DATE_YEAR, EVENT_YEAR), ]
openarch_birth[, EVENT_YEAR := ifelse((EVENT_YEAR < 1720 | EVENT_YEAR > 1930), NA, EVENT_YEAR), ]

# clean SOURCE_YEAR_YEAR

openarch_birth[, SOURCE_DATE_YEAR := ifelse((SOURCE_DATE_YEAR < 1720 | SOURCE_DATE_YEAR > 1930), NA, SOURCE_DATE_YEAR), ]

# clean days and months and replace remaining NA at EVENT_MONTH with SOURCE_DATE_MONTH 
# (same exercise for EVENT_DAY replaces nothing)

openarch_birth[, EVENT_MONTH := ifelse(EVENT_MONTH > 12, NA, EVENT_MONTH), ]
openarch_birth[, EVENT_DAY := ifelse(EVENT_DAY > 31, NA, EVENT_DAY), ]

openarch_birth[, SOURCE_DATE_MONTH := ifelse(SOURCE_DATE_MONTH > 12, NA, SOURCE_DATE_MONTH), ]
openarch_birth[, SOURCE_DATE_DAY := ifelse(SOURCE_DATE_DAY > 31, NA, SOURCE_DATE_DAY), ]

openarch_birth[, EVENT_MONTH := ifelse(is.na(EVENT_MONTH), SOURCE_DATE_MONTH, EVENT_MONTH), ]

# combine days, months, years into _DATE variables (YYYY-MM-DD)

openarch_birth[, EVENT_DATE := as.Date(paste(EVENT_DAY,EVENT_MONTH,EVENT_YEAR, sep = "-"), format = "%d-%m-%Y"),]
openarch_birth[, SOURCE_DATE_DATE := as.Date(paste(SOURCE_DATE_DAY,SOURCE_DATE_MONTH,SOURCE_DATE_YEAR, sep = "-"), format = "%d-%m-%Y"),]

# reset days into numericals removing 0's if < 10?

### AGES cleaning

# generate PR_FTHR_AGE_CLEAN

ref_age_2019_09_12_Rick_M <- read.delim("~/02. Werk/Clariah/ref_age_2019_09_12_Rick_M.txt")

openarch_birth$PR_FTHR_AGE_CLEAN <- with(ref_age_2019_09_12_Rick_M, standard_year[match(openarch_birth$PR_FTHR_AGE, cleaned)])
openarch_birth$PR_FTHR_AGE_CLEAN_CODE <- with(ref_age_2019_09_12_Rick_M, standard_code[match(openarch_birth$PR_FTHR_AGE, cleaned)])

openarch_birth[PR_FTHR_AGE_CLEAN_CODE == "n" | PR_FTHR_AGE_CLEAN_CODE == "u" , PR_FTHR_AGE_CLEAN := NA] 

# remove PR_FTHR_AGE_CLEAN_CODE variable

openarch_birth[, PR_FTHR_AGE_CLEAN_CODE := NULL] 

# generate PR_MTHR_AGE_CLEAN

ref_age_2019_09_12_Rick_M <- read.delim("~/02. Werk/Clariah/ref_age_2019_09_12_Rick_M.txt")

openarch_birth$PR_MTHR_AGE_CLEAN <- with(ref_age_2019_09_12_Rick_M, standard_year[match(openarch_birth$PR_MTHR_AGE, cleaned)])
openarch_birth$PR_MTHR_AGE_CLEAN_CODE <- with(ref_age_2019_09_12_Rick_M, standard_code[match(openarch_birth$PR_MTHR_AGE, cleaned)])

openarch_birth[PR_MTHR_AGE_CLEAN_CODE == "n" | PR_MTHR_AGE_CLEAN_CODE == "u" , PR_MTHR_AGE_CLEAN := NA] 

# remove PR_MTHR_AGE_CLEAN_CODE variable

openarch_birth[, PR_MTHR_AGE_CLEAN_CODE := NULL] 

# remove ages outside fertility range (set to 12?)

openarch_birth[PR_MTHR_AGE_CLEAN <= 14 | PR_MTHR_AGE_CLEAN >= 50 , PR_MTHR_AGE_CLEAN := NA] 
openarch_birth[PR_FTHR_AGE_CLEAN <= 14 | PR_FTHR_AGE_CLEAN >= 100 , PR_FTHR_AGE_CLEAN := NA] 

# to do: add remaining _AGE_CLEAN that are not yet in the lookup table ('29jaar' > 29)

### PLACE NAMES

# empty cells to NA

openarch_birth[EVENT_PLACE == "", EVENT_PLACE := NA, ]
openarch_birth[SOURCEREFERENCE_PLACE == "", SOURCEREFERENCE_PLACE := NA, ]




