# comparing matches between CoD and burgerLinker


library(data.table)
library(stringr)
library(readxl)
library(stringi)

setwd("C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\Causes_of_Death")

cod_matches <- read_xlsx("1856_geboorteakten_overlijdensakten.xlsx")

setDT(cod_matches)
cod_matches <- cod_matches[!is.na(ID_geb) & !is.na(ID_ovl_akte),]

links_matches <- fread("burgerlinker_results_4\\within_b_d-maxLev-4\\results\\within-B-D-maxLev-4.csv")
  
births <- fread("burgerlinker_results_4\\Geboorteakten_1856_CoD_clean.csv")
  
deaths <- fread("burgerlinker_results_4\\Overlijdensakten_1856_1857_clean.csv")

# change burgerLinker ID's back to CoD

links_matches[, ID_geb := gsub("b-", "", id_certificate_newborn),]

links_matches[, ID_ovl_akte := gsub("d-", "", id_certificate_deceased),]

setnames(births, "id", "ID_geb")

setnames(deaths, "id_ovl_akte", "ID_ovl_akte")

# generate match key to compare similar matches

links_matches[, matchpair := paste0(ID_geb,"-",ID_ovl_akte),]

setDT(cod_matches)
cod_matches[, matchpair := paste0(ID_geb,"-",ID_ovl_akte),]

# compare matches

cod_matches[, linksmatch := ifelse(matchpair %in% links_matches$matchpair, TRUE, FALSE),]

cod_matches[, .N, list(linksmatch)]

links_matches[, codmatch := ifelse(matchpair %in% cod_matches$matchpair, TRUE, FALSE),]

links_matches[, .N, list(codmatch)] # n add up

# get cod matches only

only_cod_matches <- cod_matches[linksmatch == FALSE & !grepl("NA", matchpair),]

# get links matches only

only_links_matches <- links_matches[codmatch == FALSE,]

# save

fwrite(only_cod_matches, "only_cod_matches_2.csv", sep = ";", row.names = FALSE)

fwrite(only_links_matches, "only_links_matches_2.csv", sep = ";", row.names = FALSE)

# make overview files of cod only births and deaths

births_matched <- births[ID_geb %in% only_cod_matches$ID_geb,]

deaths_matched <- deaths[ID_ovl_akte %in% only_cod_matches$ID_ovl_akte,]

births_matched <- merge(births_matched, only_cod_matches[,c("matchpair", "ID_geb")], by = "ID_geb", all = T)

deaths_matched <- deaths_matched[,-(27:28)] # duplicated columns

deaths_matched <- merge(deaths_matched, only_cod_matches[,c("matchpair", "ID_ovl_akte")], by = "ID_ovl_akte")

# combine births and deaths on matchpair

matchpairs_cod_only <- merge(births_matched, deaths_matched, by = "matchpair")

colnames(matchpairs_cod_only) = gsub(".x", "_birth", colnames(matchpairs_cod_only))
colnames(matchpairs_cod_only) = gsub(".y", "_death", colnames(matchpairs_cod_only))
setnames(matchpairs_cod_only, "achternaam_kind", "achternaam_kind_death")

# keep names only

matchpairs_cod_only <- matchpairs_cod_only[,c(20,26,40,41,4,25,45,51,16,29,47,53,15,44,2,30)]

# check if levensthein distance affects matches between CoD and burgerLinker
# only for lastnames atm

library(stringdist)

matchpairs_cod_only[, name_dist_mother := stringdist(achternaam_moeder_clean_birth, achternaam_moeder_clean_death, method = "lv"),]

matchpairs_cod_only[, name_dist_father := stringdist(achternaam_vader_clean_birth, achternaam_vader_clean_death, method = "lv"),]

matchpairs_cod_only[, name_dist_child := stringdist(achternaam_kind_clean_birth, achternaam_kind_death, method = "lv"),]

names <- matchpairs_cod_only[achternaam_vader_clean_birth != "" & name_dist_mother > 4 
                    |achternaam_vader_clean_birth != "" & name_dist_father > 4 
                    |achternaam_vader_clean_birth != "" & name_dist_child > 4,] 

# 240 that burgerLinker excludes. Some of these seem correct matches, but a large share seems all wrong. 
# note some encoding issues as well

# for burgerlinker_results_4 father lastname is used for child_lastname, so if no father = no match

matchpairs_cod_only[name_dist_father <= 4 ,.N,list(achternaam_vader_clean_birth)][order(-N)] # another 233 

# some remaining wrong matches can be captured by different sex

matchpairs_cod_only[name_dist_mother <= 4 | name_dist_father <= 4 | name_dist_child <= 4, .N, 
                    list(geslacht_kind_birth, geslacht_kind_death)] # another 23


# remaining unexplained

unexplained <- matchpairs_cod_only[achternaam_vader_clean_birth != "" & name_dist_mother <= 4 & geslacht_kind_birth == geslacht_kind_death 
                             |achternaam_vader_clean_birth != "" & name_dist_father <= 4 & geslacht_kind_birth == geslacht_kind_death 
                             |achternaam_vader_clean_birth != "" & name_dist_child <= 4 & geslacht_kind_birth == geslacht_kind_death,]

unexplained <- unexplained[name_dist_mother <= 4 & name_dist_father <= 4 & name_dist_child <=4,]

unexplained[, name_dist_fnmother := stringdist(voornamen_moeder_birth, voornamen_moeder_death, method = "lv"),]
unexplained[, name_dist_fnfather := stringdist(voornamen_vader_birth, voornamen_vader_death, method = "lv"),]
unexplained[, name_dist_fnchild := stringdist(voornamen_kind_birth, voornamen_kind_death, method = "lv"),]

unexplained[name_dist_fnmother > 4 | name_dist_fnfather > 4 | name_dist_fnchild > 4, .N,]

unexplained <- unexplained[name_dist_fnmother <= 4 & name_dist_fnfather <= 4 & name_dist_fnchild <= 4, ]

unexplained <- merge(unexplained, births[,c("ID_geb", "datum_geboorte")], by = "ID_geb", all.x = T, all.y = F)
unexplained <- merge(unexplained, deaths[,c("ID_ovl_akte", "datum_overlijden")], by = "ID_ovl_akte", all.x = T, all.y = F)

# how many missing dates?

unexplained[is.na(datum_geboorte) | is.na(datum_overlijden), .N] # 86

unexplained <- unexplained[!is.na(datum_geboorte), ] # no missing death dates

# remaining unexplained = 48

fwrite(unexplained, "cod_only_unexplained.csv", sep = ";", row.names = F)

