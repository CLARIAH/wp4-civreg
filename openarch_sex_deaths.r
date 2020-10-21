## openarch death retrieve sex by given name

rm(list = ls())
setwd("~/02. Werk/Clariah")

library("data.table")
library("stringi")
library("stringr")

setDTthreads(threads = 8)
memory.limit(54000)

openarch_death = fread("openarch/openarch2links/openarch_deaths_dedup_amco_ages.csv") 
setDT(openarch_death)


openarch_death[PR_GENDER == "Man", PR_GENDER_2 := "m"]
openarch_death[PR_GENDER == "Vrouw", PR_GENDER_2 := "f"]

### discarded for now ###
#names <- read.delim("~/02. Werk/Clariah/CLARIAH_NAMES/names_gn_1.tsv")
#setDT(names)
#names[, gn := tolower(gn)]
#names[, gn_gender := tolower(gn_gender)]
#names[, gn := stringi::stri_trans_general(gn, "Latin-ASCII")]
#names[,gn := gsub('[[:punct:] ]+',' ',gn)]
#names[,gn := gsub(" ", "",gn, fixed = TRUE)]
#names[,gn := gsub("'", "",gn, fixed = TRUE)]
#names[,gn := trimws(gn, which = c("both"))]

#names2 <- names[order(names$gn, -abs(names$gn_tokens_genlias11_1) ), ]
#names3 <- names2[ !duplicated(names2$gn) & , list(gn, gn_gender)] 
####

openarch_death[, PR_NAME_GN_ST := tolower(PR_NAME_GN)]
openarch_death[, PR_NAME_GN_ST := stringi::stri_trans_general(PR_NAME_GN_ST, "Latin-ASCII")]
openarch_death[,PR_NAME_GN_ST := gsub('[[:punct:] ]+',' ',PR_NAME_GN_ST)]
openarch_death[,PR_NAME_GN_ST := gsub(" ", "",PR_NAME_GN_ST, fixed = TRUE)]
openarch_death[,PR_NAME_GN_ST := gsub("'", "",PR_NAME_GN_ST, fixed = TRUE)]
openarch_death[,PR_NAME_GN_ST := trimws(PR_NAME_GN_ST, which = c("both"))]



openarch_names <- openarch_death[!is.na(PR_GENDER_2) & PR_NAME_GN_ST != "", .N, list(PR_NAME_GN_ST, PR_GENDER_2)]
openarch_names <- openarch_names[PR_NAME_GN_ST != "nn",]
openarch_names <- openarch_names[!grepl("levenl", PR_NAME_GN_ST),]
openarch_names <- openarch_names[PR_NAME_GN_ST != "levenslooskind",]
openarch_names <- openarch_names[PR_NAME_GN_ST != "levenooskind",]

# keep only sex of most N by name

openarch_names <- openarch_names[order(openarch_names$PR_NAME_GN_ST, -abs(openarch_names$N) ), ]
openarch_names$N <- NULL
openarch_names <- openarch_names[!duplicated(openarch_names$PR_NAME_GN_ST),]  

names(openarch_names)[2] <- "PR_GENDER_3"

# retreive no sex from deaths

openarch_names_nogender <- openarch_death[is.na(PR_GENDER_2) & PR_NAME_GN_ST != "", .N, list(PR_NAME_GN_ST, PR_GENDER_2)]
openarch_names_nogender <- openarch_names_nogender[order(openarch_names_nogender$PR_NAME_GN_ST, -abs(openarch_names_nogender$N) ), ]
openarch_names_nogender$N <- NULL
openarch_names_nogender <- openarch_names_nogender[!duplicated(openarch_names_nogender$PR_NAME_GN_ST),]



openarch_all_names <- merge(x = openarch_names, y = openarch_names_nogender, 
                            by = "PR_NAME_GN_ST", all.x=TRUE, all.y = TRUE)



openarch_all_names$PR_GENDER_2 <- NULL


openarch_death2 <- merge(openarch_all_names, openarch_death, by = "PR_NAME_GN_ST", all.y = TRUE)

# if gender_2 is missing, replace by gender_3

openarch_death2[is.na(PR_GENDER_2) | PR_GENDER_2 == "", PR_GENDER_2 := PR_GENDER_3]

openarch_death2[, PR_GENDER_3 := NULL]

fwrite(openarch_death2, 
       "openarch/openarch2links/openarch_deaths_dedup_amco_ages_sex.csv.gz", 
       sep=";", row.names = FALSE, na = "" )

