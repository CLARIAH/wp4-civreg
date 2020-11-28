## openarch death retrieve sex by given name

library("data.table")

setDTthreads(threads = 3)

openarch_death = fread(cmd = "gunzip -c openarch_deaths_amco_ages.csv.gz") 
setDT(openarch_death)

openarch_death[PR_GENDER == "Man", PR_GENDER_2 := "m"]
openarch_death[PR_GENDER == "Vrouw", PR_GENDER_2 := "f"]

openarch_death[, PR_NAME_GN_ST := tolower(PR_NAME_GN)]
openarch_death[, PR_NAME_GN_ST := stringi::stri_trans_general(PR_NAME_GN_ST, "Latin-ASCII")]
openarch_death[, PR_NAME_GN_ST := gsub('[[:punct:] ]+',' ',PR_NAME_GN_ST)]
openarch_death[, PR_NAME_GN_ST := gsub(" ", "",PR_NAME_GN_ST, fixed = TRUE)]
openarch_death[, PR_NAME_GN_ST := gsub("'", "",PR_NAME_GN_ST, fixed = TRUE)]
openarch_death[, PR_NAME_GN_ST := trimws(PR_NAME_GN_ST, which = c("both"))]


openarch_names <- openarch_death[!is.na(PR_GENDER_2) & PR_NAME_GN_ST != "", .N, list(PR_NAME_GN_ST, PR_GENDER_2)]
openarch_names <- openarch_names[PR_NAME_GN_ST != "nn",]
openarch_names <- openarch_names[!grepl("levenl", PR_NAME_GN_ST),]
openarch_names <- openarch_names[PR_NAME_GN_ST != "levenslooskind",]
openarch_names <- openarch_names[PR_NAME_GN_ST != "levenooskind",]

# keep only sex of most N by name
openarch_names <- openarch_names[order(openarch_names$PR_NAME_GN_ST, -abs(openarch_names$N) ), ]
openarch_names$N <- NULL
openarch_names <- openarch_names[!duplicated(openarch_names$PR_NAME_GN_ST),]  

setnames(openarch_names, 2, "PR_GENDER_2")

# retreive no sex from deaths
openarch_names_nogender <- openarch_death[is.na(PR_GENDER_2) & PR_NAME_GN_ST != "", .N, list(PR_NAME_GN_ST, PR_GENDER_2)]
openarch_names_nogender <- openarch_names_nogender[order(openarch_names_nogender$PR_NAME_GN_ST, -abs(openarch_names_nogender$N) ), ]
openarch_names_nogender$N <- NULL
openarch_names_nogender <- openarch_names_nogender[!duplicated(openarch_names_nogender$PR_NAME_GN_ST),]

openarch_all_names <- merge(
    x = openarch_names, 
    y = openarch_names_nogender, 
    by = "PR_NAME_GN_ST", 
    all.x=TRUE, 
    all.y = TRUE)

openarch_all_names$PR_GENDER_2.y <- NULL
setnames(openarch_all_names, "PR_GENDER_2.x", "PR_GENDER_3")

openarch_death2 <- merge(openarch_death, openarch_all_names, by = "PR_NAME_GN_ST", all.x = TRUE)

# if gender_2 is missing, replace by gender_3, and flag added sex 

#openarch_death2[is.na(PR_GENDER_2) & !is.na(PR_GENDER_3), sexflag := 1] not implemented in v1 yet

openarch_death2[is.na(PR_GENDER_2), PR_GENDER_2 := PR_GENDER_3]

# keep one sex variable 
openarch_death2[, PR_GENDER_3 := NULL]
openarch_death2[, PR_GENDER := NULL]
setnames(openarch_death2, "PR_GENDER_2", "PR_GENDER")

fwrite(
    x = openarch_death2, 
   "openarch_deaths_amco_ages_sex.csv.gz", 
   compress = "gzip")
