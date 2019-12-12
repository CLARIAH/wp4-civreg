### GET FIELDS WITH PLACENAMES FROM BIRTH, MARRIAGE, AND DEATH CERT.

# BIRTH PLACENAMES (only for variables that contain (sometimes) placenames, others are checked and empty)

library(data.table)
setwd("C:\\Users\\Ruben\\Desktop\\openarch\\birth\\")

file_list_birth <- list.files(recursive = FALSE)

placenames_birth = list()

for (file in file_list_birth){
  {
    placenames_birth[[file]] <- fread(file, encoding="UTF-8")[, list(EVENT_PLACE, SOURCE_PLACE, SOURCEREFERENCE_PLACE, PR_BIR_PLACE)]
  }}

# write to dataframe 

openarch_placenames_birth <- rbindlist(placenames_birth)

# MARRIAGE PLACENAMES (only for variables that contain (sometimes) placenames, others are checked and empty)

library(data.table)
setwd("C:\\Users\\Ruben\\Desktop\\openarch\\marriage\\")

file_list_marriage <- list.files(recursive = FALSE)

placenames_marriage = list()

for (file in file_list_marriage){
  {
    placenames_marriage[[file]] <- fread(file, encoding="UTF-8")[, list(EVENT_PLACE, SOURCE_PLACE, SOURCEREFERENCE_PLACE, 
                                                                        GROOM_FTHR_BIR_PLACE, GROOM_MTHR_BIR_PLACE, BRIDE_BIR_PLACE, 
                                                                        GROOM_BIR_PLACE, BRIDE_MTHR_BIR_PLACE, BRIDE_FTHR_BIR_PLACE)]
  }}

# write to dataframe

openarch_placenames_marriage <- rbindlist(placenames_marriage)

# DEATH PLACENAMES (only for variables that contain (sometimes) placenames, others are checked and empty)

library(data.table)
setwd("C:\\Users\\Ruben\\Desktop\\openarch\\death\\")

file_list_death <- list.files(recursive = FALSE)

placenames_death = list()

for (file in file_list_death){
  {
    placenames_death[[file]] <- fread(file, encoding="UTF-8")[, list(EVENT_PLACE, SOURCE_PLACE, SOURCEREFERENCE_PLACE,
                                                                     PR_BIR_PLACE, PR_RES_PLACE)]
  }}

# write to dataframe

openarch_placenames_death <- rbindlist(placenames_death)

### Combine lists to dataframe

placenames.list <- list(openarch_placenames_birth, openarch_placenames_marriage, openarch_placenames_death)

openarch_all_placenames <- rbindlist(placenames.list, fill = TRUE)

### combine placenames 

openarch_unique_placenames <- rbindlist(list(
  openarch_all_placenames[, .N, by = EVENT_PLACE],
  openarch_all_placenames[, .N, by = SOURCE_PLACE],
  openarch_all_placenames[, .N, by = SOURCEREFERENCE_PLACE],
  openarch_all_placenames[, .N, by = PR_BIR_PLACE],
  openarch_all_placenames[, .N, by = GROOM_FTHR_BIR_PLACE],
  openarch_all_placenames[, .N, by = GROOM_MTHR_BIR_PLACE ],
  openarch_all_placenames[, .N, by = BRIDE_BIR_PLACE],
  openarch_all_placenames[, .N, by = GROOM_BIR_PLACE],
  openarch_all_placenames[, .N, by = BRIDE_MTHR_BIR_PLACE],
  openarch_all_placenames[, .N, by = BRIDE_FTHR_BIR_PLACE],
  openarch_all_placenames[, .N, by = PR_RES_PLACE]
  
  
))[order(-N)]


openarch_unique_placenames2 <- openarch_unique_placenames[, sum(N), by = EVENT_PLACE][order(-V1)]
openarch_unique_placenames2 <- openarch_unique_placenames2[!is.na(EVENT_PLACE)]
openarch_unique_placenames2 <- openarch_unique_placenames2[EVENT_PLACE != ""]

setnames(openarch_unique_placenames2, c("PLACENAME","N") )
remove(openarch_unique_placenames)
openarch_unique_placenames <- openarch_unique_placenames2
remove(openarch_unique_placenames2)

# remove placenames without any letter characters

openarch_unique_placenames <- openarch_unique_placenames[!grepl("^[0-9,-]*$", PLACENAME), ]

# write to csv

fwrite(openarch_unique_placenames, file = "C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_unique_placenames.csv", 
       sep=";", row.names = FALSE, na = "") 
       

(# extract names between brackets as additional placename)

openarch_unique_placenames <- setDT(unique_placenames)[, c("PLACENAME_CLEANED","PLACENAME_ADDITIONAL") :=tstrsplit(PLACENAME, "\\(|\\)")]

  
(# compare with other placenames list as TRUE/FALSE variable)
    
openarch_unique_placenames[, DutchToponyms := PLACENAME %in% DutchToponyms1812$toponym  ]

  
  
