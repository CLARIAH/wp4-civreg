### This script 1) automatically downloads and unzips all openarch burgerlijke stand .zip files, 
#               2) generates csv file per type of certificate (birth, marriage, death), 
#                  while selecting the variables used by LINKS + a source url

library(data.table)
setDTthreads(threads = 8)

### DEATH

setwd("C:\\Users\\Ruben\\Desktop\\openarch\\death")

# download files

urls <- readLines("https://raw.githubusercontent.com/CLARIAH/wp4-civreg/master/deathsv1/openarch_bso_url_list_januari2020.txt")

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

# remove rows with duplicated SOURCE_DIGITAL ORIGINAL, keep first after ordered by EVENT_YEAR (not implemented in v1 yet)

#openarch_death <- openarch_death[order(SOURCE_DIGITAL_ORIGINAL, EVENT_YEAR, decreasing = TRUE),]
#openarch_death <- openarch_death[!duplicated(SOURCE_DIGITAL_ORIGINAL) ,]

fwrite(openarch_death, 
       "C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_death.csv.gz", 
       sep=";", row.names = FALSE, na = "")
