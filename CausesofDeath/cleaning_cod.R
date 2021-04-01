# cleaning causes of deaths

# births

library(data.table)
library(stringr)
library(naniar)
library(stringi)

setwd("C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\Causes_of_Death")

births <- fread("Geboorteakten_1856_CoD.csv")



# "###" to NA

na_strings <- c("#", "##", "###", "####","#####","geen", "")
births <- births %>% replace_with_na_all(condition = ~.x %in% na_strings)

# rename variables

setDT(births)
setnames(births, tolower(names(births)))
names(births) <- sub(" ", "_", names(births))

setnames(births, 'leeftijd\nvader', "leeftijd_vader")
setnames(births, "blad\nnummer", "bladnummer")
setnames(births, "geslacht_\nkind", "geslacht_kind")
setnames(births, "huwelijkse_staat moeder", "huwelijkse_staat_moeder")
setnames(births, "dag", "dag_geboorte")
setnames(births, "maand", "maand_geboorte")
setnames(births, "jaar", "jaar_geboorte")

# create birth date (YYYY-MM-DD)

births[maand_geboorte == "januari", maand_geboorte := 1]
births[maand_geboorte == "februari", maand_geboorte := 2]
births[maand_geboorte == "maart", maand_geboorte := 3]
births[maand_geboorte == "april", maand_geboorte := 4]
births[maand_geboorte == "mei", maand_geboorte := 5]
births[maand_geboorte == "juni", maand_geboorte := 6]
births[maand_geboorte == "juli", maand_geboorte := 7]
births[maand_geboorte == "augustus", maand_geboorte := 8]
births[maand_geboorte == "september", maand_geboorte := 9]
births[maand_geboorte == "oktober", maand_geboorte := 10]
births[maand_geboorte == "november", maand_geboorte := 11]
births[maand_geboorte == "december", maand_geboorte := 12]

datestring <- paste0(births$jaar_geboorte,"-",str_pad(births$maand_geboorte, 2,"left", pad = "0"),"-", 
                     str_pad(births$dag_geboorte, 2, "left", pad = "0"))

births[, datum_geboorte := as.Date(datestring)]

# note that datum_erkenning needs to be changed into YYYY-MM-DD format first (too many differences here to gsub for now)


# fix 'months' in Bladnummer (Excel error)

births[, .N, list(bladnummer)]
births[, bladnummer := as.character(bladnummer)]
births[grepl("Jan", bladnummer), bladnummer := gsub("Jan", "01", bladnummer),]
births[grepl("Feb", bladnummer), bladnummer := gsub("Feb", "02", bladnummer),]      
births[grepl("Mar", bladnummer), bladnummer := gsub("Mar", "03", bladnummer),]      

# standardize names according to LINKS ('LINKS_ontwerp_2020_06_03', p. 20)

#Replace all 
#'ch' with 'g', 
#'c' with 'k',  
#'z' with 's', 
#'ph' with 'f' 
#'ij' with 'y'

births[,c(3,4,15,19,24,25,26)] <- lapply(births[,c(3,4,15,19,24,25,26)], function(y) gsub("ch", "g", y))
births[,c(3,4,15,19,24,25,26)] <- lapply(births[,c(3,4,15,19,24,25,26)], function(y) gsub("c", "k", y))
births[,c(3,4,15,19,24,25,26)] <- lapply(births[,c(3,4,15,19,24,25,26)], function(y) gsub("z", "s", y))
births[,c(3,4,15,19,24,25,26)] <- lapply(births[,c(3,4,15,19,24,25,26)], function(y) gsub("ph", "f", y))
births[,c(3,4,15,19,24,25,26)] <- lapply(births[,c(3,4,15,19,24,25,26)], function(y) gsub("ij", "y", y)) 


# save

write.csv2(births, "Geboorteakten_1856_CoD_clean.csv", quote = TRUE, fileEncoding = "UTF-8", na="", row.names = FALSE )

### seperate prefixes to enable record linkage

births <- fread("Geboorteakten_1856_CoD_clean.csv")

# names to lcase

births[, voornamen_vader := tolower(voornamen_vader)]
births[, achternaam_vader := tolower(achternaam_vader)]
births[, voornamen_moeder := tolower(voornamen_moeder)]
births[, achternaam_moeder := tolower(achternaam_moeder)]
births[, voornamen_kind := tolower(voornamen_kind)]


# function to find and seperate prefixes from surname

split_prefixes = function(strings){
    to_remove = c(
    "an",
    "da",
    "de la",
    "de",
    "der",
    "die",
    "du",
    "la",
    "le",
    "te",
    "ten",
    "ter",
    "v d",
    "van de",
    "v. .d.",
    "van den",
    "van der",
    "vander",
    "van",
    "van[.]",
    "vand der",
    "vann der",
    "vd",
    "ven",
    "vna den",
    "von")
  to_remove = c(to_remove)
  to_remove = unique(to_remove)
  to_remove = to_remove[order(-nchar(to_remove))] # longest first to extract those first
  pattern = paste0("", "^", to_remove, " ", collapse = "|")
  # pattern = paste0("\\b(", pattern, ")\\b")
  
  return(
    data.frame(
      voorvoegsel_achternaam_moeder = stringi::stri_extract_first_regex(
        strings, pattern, case_insensitive = TRUE),
      achternaam_moeder_clean = stringi::stri_replace_first_regex(
        strings, pattern, "", case_insensitive = TRUE),
      stringsAsFactors = FALSE
    )
  )
}

list <- split_prefixes(births$achternaam_moeder)

births <- cbind(births, list)

# generate surname child from surname father clean and leave empty if no father lastname (cannot be sure lastname mother is used instead?)

births[achternaam_vader == "", achternaam_vader_clean := NA]

births[!is.na(achternaam_vader_clean), achternaam_kind_clean := achternaam_vader_clean]

births[!is.na(voorvoegsel_achternaam_vader), voorvoegsel_achternaam_kind := voorvoegsel_achternaam_vader]

# save

write.csv2(births, "Geboorteakten_1856_CoD_clean.csv", sep = ";", quote = TRUE, row.names = FALSE, fileEncoding = "UTF8",  na = "")


#### DEATHS

deaths <- fread("Overlijdensakten_1856_1857_CoD.csv")

# rename variables

setDT(deaths)
setnames(deaths, tolower(names(deaths)))
names(deaths) <- sub(" ", "_", names(deaths))

setnames(deaths, "blad\nnummer", "bladnummer")
setnames(deaths, "geslacht\nkind", "geslacht_kind")

# birthplace "alhier" to Amsterdam

deaths[geboren == "alhier", geboren := "Amsterdam"]

# create death_date

deaths[maand == "januari", maand := 1]
deaths[maand == "februari", maand := 2]
deaths[maand == "maart", maand := 3]
deaths[maand == "april", maand := 4]
deaths[maand == "mei", maand := 5]
deaths[maand == "juni", maand := 6]
deaths[maand == "juli", maand := 7]
deaths[maand == "augustus", maand := 8]
deaths[maand == "september", maand := 9]
deaths[maand == "oktober", maand := 10]
deaths[maand == "november", maand := 11]
deaths[maand == "december", maand := 12]

datestring <- paste0(deaths$jaar,"-",str_pad(deaths$maand, 2,"left", pad = "0"),"-", 
                     str_pad(deaths$dag, 2, "left", pad = "0"))

deaths[, datum_overlijden := as.Date(datestring)]

# names to lcase

deaths[, voornamen_vader := tolower(voornamen_vader)]
deaths[, achternaam_vader := tolower(achternaam_vader)]
deaths[, voornamen_moeder := tolower(voornamen_moeder)]
deaths[, achternaam_moeder := tolower(achternaam_moeder)]
deaths[, voornamen_kind := tolower(voornamen_kind)]
deaths[, achternaam_kind := tolower(achternaam_kind)]

# remove diacritics
deaths[, voornamen_vader := stri_trans_general(voornamen_vader, "Latin-ASCII")]
deaths[, achternaam_vader := stri_trans_general(achternaam_vader, "Latin-ASCII")]
deaths[, voornamen_moeder := stri_trans_general(voornamen_moeder, "Latin-ASCII")]
deaths[, achternaam_moeder := stri_trans_general(achternaam_moeder, "Latin-ASCII")]
deaths[, voornamen_kind := stri_trans_general(voornamen_kind, "Latin-ASCII")]
deaths[, achternaam_kind := stri_trans_general(achternaam_kind, "Latin-ASCII")]


# standardize names according to LINKS ('LINKS_ontwerp_2020_06_03', p. 20)

#Replace all 
#'ch' with 'g', 
#'c' with 'k',  
#'z' with 's', 
#'ph' with 'f' 
#'ij' with 'y'

deaths[,c(11:12, 16:19)] <- lapply(deaths[,c(11:12, 16:19)], function(y) gsub("ch", "g", y))
deaths[,c(11:12, 16:19)] <- lapply(deaths[,c(11:12, 16:19)], function(y) gsub("c", "k", y))
deaths[,c(11:12, 16:19)] <- lapply(deaths[,c(11:12, 16:19)], function(y) gsub("z", "s", y))
deaths[,c(11:12, 16:19)] <- lapply(deaths[,c(11:12, 16:19)], function(y) gsub("ph", "f", y))
deaths[,c(11:12, 16:19)] <- lapply(deaths[,c(11:12, 16:19)], function(y) gsub("ij", "y", y))

# remove prefixes from lastnames

split_prefixes = function(strings){
  to_remove = c(
    "an",
    "da",
    "de la",
    "de",
    "der",
    "die",
    "du",
    "la",
    "le",
    "te",
    "ten",
    "ter",
    "v d",
    "van de",
    "v. .d.",
    "van den",
    "van der",
    "vander",
    "van",
    "van[.]",
    "vand der",
    "vann der",
    "vd",
    "ven",
    "vna den",
    "von")
  to_remove = c(to_remove)
  to_remove = unique(to_remove)
  to_remove = to_remove[order(-nchar(to_remove))] # longest first to extract those first
  pattern = paste0("", "^", to_remove, " ", collapse = "|")
  # pattern = paste0("\\b(", pattern, ")\\b")
  
  return(
    data.frame(
      voorvoegsel_achternaam_moeder = stringi::stri_extract_first_regex(
        strings, pattern, case_insensitive = TRUE),
      achternaam_moeder_clean = stringi::stri_replace_first_regex(
        strings, pattern, "", case_insensitive = TRUE),
      stringsAsFactors = FALSE
    )
  )
}

list1 <- split_prefixes(deaths$achternaam_moeder)

deaths <- cbind(deaths, list1)


list2 <- split_prefixes(deaths$achternaam_moeder) # first change function 

deaths <- cbind(deaths, list2)

list3 <- split_prefixes(deaths$achternaam_kind) # first change function 

deaths <- cbind(deaths, list3)

# save 

write.csv2(deaths, "Overlijdensakten_1856_1857_clean.csv", sep = ";", quote = TRUE, row.names = FALSE, fileEncoding = "UTF8",  na = "")


