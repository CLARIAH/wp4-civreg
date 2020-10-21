### This script 1) automatically downloads and unzips all openarch burgerlijke stand .zip files, 
#               2) generates csv file per type of certificate (birth, marriage, death), 
#                  while selecting the variables used by LINKS + a source url

library(data.table)
setDTthreads(threads = 8)

### BIRTH

setwd("C:\\Users\\Ruben\\Desktop\\openarch\\birth")

# download files

urls <- readLines("https://raw.githubusercontent.com/CLARIAH/wp4-civreg/master/openarch_bsg_url_list_october2020.txt")

for (url in urls) {
  download.file(url, destfile = basename(url), method="curl")
  unzip(basename(url))
}

# remove .zip files from folder

unlink("*.zip", recursive = FALSE)

# combine csv's into one data frame while selecting variables

file_list <- list.files()

datasets = list()

for (file in file_list){
  {
    datasets[[file]] <- fread(file, encoding="UTF-8")[, list(EVENT_TYPE, EVENT_DAY,	EVENT_MONTH, EVENT_YEAR, EVENT_PLACE, 
                                                             SOURCE_DATE_DAY, SOURCE_DATE_MONTH, SOURCE_DATE_YEAR, SOURCE_PLACE, SOURCE_TYPE,
                                                             PR_BIR_DAY, PR_BIR_MONTH, PR_BIR_YEAR, PR_NAME_GN, PR_NAME_SPRE, PR_NAME_SURN,
                                                             PR_OCCUPATION, PR_GENDER, PR_AGE, PR_BIR_PLACE, PR_FTHR_NAME_GN, PR_FTHR_NAME_SPRE, 
                                                             PR_FTHR_NAME_SURN, PR_FTHR_GENDER, PR_FTHR_OCCUPATION, PR_FTHR_RELATIONTYPE,
                                                             PR_FTHR_AGE, PR_MTHR_NAME_GN, PR_MTHR_NAME_SPRE, PR_MTHR_NAME_SURN, PR_MTHR_GENDER,
                                                             PR_MTHR_OCCUPATION, PR_MTHR_AGE, PR_MTHR_RELATIONTYPE, SOURCEREFERENCE_PLACE, 
                                                             SOURCEREFERENCE_DOCUMENTNUMBER, SOURCEREFERENCE_REGISTRYNUMBER, SOURCE_REMARK, EVENT_REMARK, 
                                                             SOURCEREFERENCE_INSTITUTIONNAME,SOURCE_DIGITAL_ORIGINAL,
                                                             SOURCE_RECORD_GUID)]  
  }}

openarch_birth <- rbindlist(datasets)
rm(datasets)

# remove duplicated rows

openarch_birth <- openarch_birth[!duplicated(openarch_birth), ]

# remove rows with duplicated SOURCE_DIGITAL ORIGINAL, keep first after ordered by EVENT_YEAR

openarch_birth <- openarch_birth[order(SOURCE_DIGITAL_ORIGINAL, EVENT_YEAR, decreasing = TRUE),]
openarch_birth <- openarch_birth[!duplicated(SOURCE_DIGITAL_ORIGINAL) ,]


# write to csv

fwrite(openarch_birth, "openarch_births_2.csv.gz", sep=";", row.names = FALSE, na = "")



### MARRIAGE

setwd("C:\\Users\\Ruben\\Desktop\\openarch\\marriage")

# download files

urls <- readLines("https://raw.githubusercontent.com/CLARIAH/wp4-civreg/master/openarch_bsh_url_list_januari2020.txt")

for (url in urls) {
  download.file(url, destfile = basename(url), method="curl")
  unzip(basename(url))
}

# remove .zip files from folder

unlink("*.zip", recursive = FALSE)

# combine csv's into one data frame while selecting variables


file_list <- list.files()

datasets = list()

for (file in file_list){
  {
    datasets[[file]] <- fread(file, encoding="UTF-8")[, list(EVENT_DAY,	EVENT_MONTH, EVENT_YEAR, EVENT_PLACE, 
                                                             SOURCE_DATE_DAY, SOURCE_DATE_MONTH, SOURCE_DATE_YEAR,SOURCE_PLACE, 
                                                             GROOM_NAME, GROOM_NAME_GN, GROOM_NAME_SPRE, GROOM_NAME_SURN, GROOM_GENDER,
                                                             GROOM_AGE,GROOM_OCCUPATION, GROOM_BIR_PLACE, GROOM_BIR_DAY, GROOM_BIR_MONTH, GROOM_BIR_YEAR,
                                                             BRIDE_NAME, BRIDE_NAME_GN, BRIDE_NAME_SPRE, BRIDE_NAME_SURN, BRIDE_GENDER,
                                                             BRIDE_AGE,BRIDE_OCCUPATION, BRIDE_BIR_PLACE, BRIDE_BIR_DAY, BRIDE_BIR_MONTH, BRIDE_BIR_YEAR,
                                                             GROOM_FTHR_NAME, GROOM_FTHR_NAME_GN, GROOM_FTHR_NAME_SPRE, GROOM_FTHR_NAME_SURN,
                                                             GROOM_FTHR_GENDER,GROOM_FTHR_OCCUPATION, GROOM_FTHR_AGE, GROOM_FTHR_RELATIONTYPE,
                                                             GROOM_MTHR_NAME, GROOM_MTHR_NAME_GN, GROOM_MTHR_NAME_SPRE, GROOM_MTHR_OCCUPATION,
                                                             GROOM_MTHR_NAME_SURN, GROOM_MTHR_GENDER,GROOM_MTHR_AGE, GROOM_MTHR_RELATIONTYPE,
                                                             GROOM_FTHR_BIR_PLACE, GROOM_MTHR_BIR_PLACE,
                                                             BRIDE_FTHR_NAME, BRIDE_FTHR_NAME_GN, BRIDE_FTHR_NAME_SPRE, BRIDE_FTHR_NAME_SURN,
                                                             BRIDE_FTHR_GENDER, BRIDE_FTHR_AGE, BRIDE_FTHR_RELATIONTYPE,BRIDE_FTHR_OCCUPATION,
                                                             BRIDE_MTHR_NAME, BRIDE_MTHR_NAME_GN, BRIDE_MTHR_NAME_SPRE, 
                                                             BRIDE_MTHR_NAME_SURN, BRIDE_MTHR_GENDER,BRIDE_MTHR_AGE,BRIDE_MTHR_RELATIONTYPE, 
                                                             BRIDE_MTHR_BIR_PLACE, BRIDE_FTHR_BIR_PLACE, BRIDE_MTHR_OCCUPATION,
                                                             EVENT_TYPE, SOURCE_TYPE, SOURCEREFERENCE_PLACE, SOURCE_REMARK,EVENT_REMARK,
                                                             SOURCEREFERENCE_INSTITUTIONNAME, SOURCEREFERENCE_DOCUMENTNUMBER, SOURCE_DIGITAL_ORIGINAL,
                                                             SOURCE_RECORD_GUID    )]
  }}

openarch_marriage <- rbindlist(datasets)
rm(datasets)


# remove duplicated rows

openarch_marriage <- openarch_marriage[!duplicated(openarch_birth), ]

# remove rows with duplicated SOURCE_DIGITAL ORIGINAL, keep first after ordered by EVENT_YEAR

openarch_marriage <- openarch_marriage[order(SOURCE_DIGITAL_ORIGINAL, EVENT_YEAR, decreasing = TRUE),]
openarch_marriage <- openarch_marriage[!duplicated(SOURCE_DIGITAL_ORIGINAL) ,]



fwrite(openarch_marriage, 
       "C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_marriage.csv.gz", 
       sep=";", row.names = FALSE, na = "")

### DEATH

setwd("C:\\Users\\Ruben\\Desktop\\openarch\\death")

# download files

urls <- readLines("https://raw.githubusercontent.com/CLARIAH/wp4-civreg/master/openarch_bso_url_list_januari2020.txt")

for (url in urls) {
  download.file(url, destfile = basename(url), method="curl")
  unzip(basename(url))
}

# remove .zip files from folder

unlink("*.zip", recursive = FALSE)

# combine csv's into one data frame while selecting variables


file_list <- list.files()

datasets = list()

for (file in file_list){
  {
    datasets[[file]] <- fread(file, encoding="UTF-8")[, list(EVENT_DAY,	EVENT_MONTH, EVENT_YEAR, EVENT_TYPE, EVENT_PLACE, SOURCE_PLACE,
                                                             SOURCE_DATE_DAY, SOURCE_DATE_MONTH, SOURCE_DATE_YEAR,
                                                             PR_NAME_GN, PR_NAME_SPRE, PR_NAME_SURN,PR_RES_PLACE, PR_BIR_PLACE, 
                                                             PR_AGE,PR_BIR_DAY, PR_BIR_MONTH, PR_BIR_YEAR, PR_OCCUPATION, PR_GENDER,
                                                             PR_FTHR_NAME_GN, PR_FTHR_NAME_SPRE, PR_FTHR_AGE, PR_FTHR_OCCUPATION, 
                                                             PR_FTHR_NAME_SURN, PR_MTHR_NAME_GN, PR_MTHR_NAME_SPRE, PR_MTHR_NAME_SURN, PR_MTHR_AGE,
                                                             PR_MTHR_OCCUPATION, EVENT_TYPE, SOURCE_TYPE, SOURCEREFERENCE_PLACE, SOURCEREFERENCE_REGISTRYNUMBER, 
                                                             SOURCEREFERENCE_DOCUMENTNUMBER, SOURCE_DIGITAL_ORIGINAL, SOURCE_REMARK,EVENT_REMARK,
                                                             SOURCEREFERENCE_INSTITUTIONNAME, SOURCE_RECORD_GUID)]
  }}

openarch_death <- rbindlist(datasets)
rm(datasets)


# remove duplicated rows

openarch_death <- openarch_death[!duplicated(openarch_birth), ]

# remove rows with duplicated SOURCE_DIGITAL ORIGINAL, keep first after ordered by EVENT_YEAR

openarch_death <- openarch_death[order(SOURCE_DIGITAL_ORIGINAL, EVENT_YEAR, decreasing = TRUE),]
openarch_death <- openarch_death[!duplicated(SOURCE_DIGITAL_ORIGINAL) ,]

fwrite(openarch_death, 
       "C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_death.csv.gz", 
       sep=";", row.names = FALSE, na = "")
