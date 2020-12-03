### This script 1) automatically downloads and unzips all openarch burgerlijke stand .zip files, 
#               2) generates csv file per type of certificate (birth, marriage, death), 
#                  while selecting the variables used by LINKS + a source url

library(data.table)
setDTthreads(threads = 3)

args = commandArgs(trailingOnly = TRUE)
urls = args[1]
cat(urls)
dataset = args[2]
cat(dataset)


urls <- readLines(urls)

datasets = list()

for (csv_url in urls) {
    # download and unzip files
    tmp = tempfile()
    download.file(csv_url, destfile = tmp, method="curl")
    tmp_unzipped = unzip(tmp, list = TRUE)$Name # get name of unzipped file
    unzip(zipfile = tmp, files = tmp_unzipped, exdir = dirname(tmp))

    datasets[[tmp_unzipped]] <- fread(
        input = file.path(dirname(tmp), tmp_unzipped), 
        encoding="UTF-8")[, list(EVENT_DAY,	EVENT_MONTH, EVENT_YEAR, EVENT_TYPE, EVENT_PLACE, SOURCE_PLACE,
                                 SOURCE_DATE_DAY, SOURCE_DATE_MONTH, SOURCE_DATE_YEAR,
                                 PR_NAME_GN, PR_NAME_SPRE, PR_NAME_SURN,PR_RES_PLACE, PR_BIR_PLACE, 
                                 PR_AGE,PR_BIR_DAY, PR_BIR_MONTH, PR_BIR_YEAR, PR_OCCUPATION, PR_GENDER,
                                 PR_FTHR_NAME_GN, PR_FTHR_NAME_SPRE, PR_FTHR_AGE, PR_FTHR_OCCUPATION, 
                                 PR_FTHR_NAME_SURN, PR_MTHR_NAME_GN, PR_MTHR_NAME_SPRE, PR_MTHR_NAME_SURN, PR_MTHR_AGE,
                                 PR_MTHR_OCCUPATION, EVENT_TYPE, SOURCE_TYPE, SOURCEREFERENCE_PLACE, SOURCEREFERENCE_REGISTRYNUMBER, 
                                 SOURCEREFERENCE_DOCUMENTNUMBER, SOURCE_DIGITAL_ORIGINAL, SOURCE_REMARK,EVENT_REMARK,
                                 SOURCEREFERENCE_INSTITUTIONNAME, SOURCE_RECORD_GUID)]
}

openarch_death <- rbindlist(datasets)
rm(datasets)

# remove duplicated rows
openarch_death <- openarch_death[!duplicated(openarch_death), ]

# remove rows with duplicated SOURCE_DIGITAL ORIGINAL, keep first after ordered by EVENT_YEAR (not implemented in v1 yet)
#openarch_death <- openarch_death[order(SOURCE_DIGITAL_ORIGINAL, EVENT_YEAR, decreasing = TRUE),]
#openarch_death <- openarch_death[!duplicated(SOURCE_DIGITAL_ORIGINAL) ,]

fwrite(openarch_death, 
       dataset,
       verbose = TRUE)
