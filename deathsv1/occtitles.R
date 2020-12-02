library("data.table")
library("stringi")

setDTthreads(2)

dataset = commandArgs(trailingOnly = TRUE)
dataset = "deaths1910-30.csv"

deaths191030 = fread(dataset)
deaths191030 = deaths191030[, .SD, .SDcols = -patterns("OCC|HIS")]
hsnhisco <- fread("HSN_HISCO_release_2020_02.csv")

# standardize and merge occs deaths with hsnhisco

deaths191030[, occtitle_st := tolower(occtitle)]
deaths191030[, occtitle_st := gsub('[[:punct:] ]+',' ',occtitle_st)]
deaths191030[, occtitle_st := trimws(occtitle_st, which = c("both"))]
deaths191030[, occtitle_st := gsub(" ", "", occtitle_st)]

hsnhisco[, original_st := tolower(Original)]
hsnhisco[, original_st := gsub('[[:punct:] ]+',' ',original_st)]
hsnhisco[, original_st := trimws(original_st, which = c("both"))]
hsnhisco[, original_st := gsub(" ", "", original_st)]
hsnhisco <- hsnhisco[order(original_st, -HISCO),]
hsnhisco <- hsnhisco[!duplicated(original_st),]

deaths191030 <- merge(
    deaths191030, 
    hsnhisco[,c("HISCO", "HISCLASS", "HISCLASS_5", "HISCAM_NL", "original_st", "OCC1950")], 
    by.x = "occtitle_st", 
    by.y = "original_st", 
    all.x = TRUE, 
    all.y = FALSE)

# check for duplids & code and seperate multiple matches

# fix hisclass fishermen (8 instead of 10)

deaths191030[HISCO == 64100 & !grepl("knecht", occtitle), HISCLASS := 8 ]
deaths191030[HISCO == 64100 & !grepl("knecht", occtitle), HISCLASS_5 := 3 ]

# save occtitle coding
# fwrite(hsnmatch, "deaths1910-30_occtitles_coded.csv", sep = ";", row.names = FALSE)

# merge occs with deaths191030 for ego's only

deaths191030[!is.na(HISCO), .N] # 99% of cert with occtitles coded into hisco
deaths191030[, sum(!is.na(HISCO)) / .N] # 99% of cert with occtitles coded into hisco

tail(deaths191030[, .N, by = list(HISCO)][order(N)], 10)

# add n > 100 without hisco manually (female occs)

deaths191030[occtitle_st == "fabriekarbeidster", occtitle_st := "fabrieksarbeidster"]
deaths191030[occtitle_st == "religieuse", occtitle_st := "religieuze"]

manuals <- deaths191030[is.na(HISCO), .N, list(occtitle_st)][N > 100][order(-N)]

manuals[occtitle_st %in% hsnhisco$Standard, .N]

manuals <- merge(manuals, hsnhisco[,c("HISCO", "HISCLASS", "HISCLASS_5", "HISCAM_NL", "Standard", "OCC1950")], 
                 by.x = "occtitle_st", by.y = "Standard", all.x = T, all.y = F)
manuals <- manuals[!duplicated(manuals)]


deaths191030 <- merge(deaths191030, manuals, by = "occtitle_st", all.x = T, all = F)

deaths191030[!is.na(HISCO.x), .N]
deaths191030[, sum(!is.na(HISCO.x)) / .N]

deaths191030[is.na(HISCO.x), HISCO.x := HISCO.y]
deaths191030[is.na(HISCLASS.x), HISCLASS.x := HISCLASS.y]
deaths191030[is.na(HISCLASS_5.x), HISCLASS_5.x := HISCLASS_5.y]
deaths191030[is.na(HISCAM_NL.x), HISCAM_NL.x := HISCAM_NL.y]
deaths191030[is.na(OCC1950.x), OCC1950.x := OCC1950.y]

deaths191030[, HISCO.y := NULL]
deaths191030[, HISCLASS.y := NULL]
deaths191030[, HISCLASS_5.y := NULL]
deaths191030[, HISCAM_NL.x := NULL]
deaths191030[, OCC1950.y := NULL]
deaths191030[, N.x := NULL]
deaths191030[, N.y := NULL]

names(deaths191030) <- sub(".x", "", names(deaths191030))

fwrite(
    x = deaths191030, 
    file = dataset)
