library("data.table")
library("stringi")

setDTthreads(2)

args = commandArgs(trailingOnly = TRUE)
dataset = args[1]
occupation = args[2]

cat("running with dataset ", dataset, " and occupation field ", occupation, "\n")

openarch = fread(dataset)
hsnhisco <- fread("HSN_HISCO_release_2020_02.csv")

cat(nrow(openarch), " rows\n\n")
hiscovrbs = c("HISCO", "HISCLASS", "HISCLASS_5", "HISCAM_NL", "OCC1950")

if (any(hiscovrbs %in% names(openarch))){
    warning("hisco variables already present")
}
openarch[, occtitle := get(occupation)]

# standardize and merge occs deaths with hsnhisco

openarch[, occtitle_st := tolower(occtitle)]
openarch[, occtitle_st := gsub('[[:punct:] ]+',' ',occtitle_st)]
openarch[, occtitle_st := trimws(occtitle_st, which = c("both"))]
openarch[, occtitle_st := gsub(" ", "", occtitle_st)]

hsnhisco[, original_st := tolower(Original)]
hsnhisco[, original_st := gsub('[[:punct:] ]+',' ',original_st)]
hsnhisco[, original_st := trimws(original_st, which = c("both"))]
hsnhisco[, original_st := gsub(" ", "", original_st)]
hsnhisco <- hsnhisco[order(original_st, -HISCO),]
hsnhisco <- hsnhisco[!duplicated(original_st),]

openarch <- merge(
    openarch, 
    hsnhisco[,c("HISCO", "HISCLASS", "HISCLASS_5", "HISCAM_NL", "original_st", "OCC1950")], 
    by.x = "occtitle_st", 
    by.y = "original_st", 
    all.x = TRUE, 
    all.y = FALSE)

# check for duplids & code and seperate multiple matches

# fix hisclass fishermen (8 instead of 10)

openarch[HISCO == 64100 & !grepl("knecht", occtitle), HISCLASS := 8 ]
openarch[HISCO == 64100 & !grepl("knecht", occtitle), HISCLASS_5 := 3 ]

# save occtitle coding
# fwrite(hsnmatch, "deaths1910-30_occtitles_coded.csv", sep = ";", row.names = FALSE)

# merge occs with openarch for ego's only

cat(
    "matched ",
    openarch[, sum(!is.na(HISCO)) / .N],
    " to hisco")

# add n > 100 without hisco manually (female occs)

openarch[occtitle_st == "fabriekarbeidster", occtitle_st := "fabrieksarbeidster"]
openarch[occtitle_st == "religieuse", occtitle_st := "religieuze"]

manuals <- openarch[is.na(HISCO), .N, list(occtitle_st)][N > 100][order(-N)]

manuals[occtitle_st %in% hsnhisco$Standard, .N]

manuals <- merge(
    manuals, 
    hsnhisco[,c("HISCO", "HISCLASS", "HISCLASS_5", "HISCAM_NL", "Standard", "OCC1950")], 
    by.x = "occtitle_st", 
    by.y = "Standard", 
    all.x = TRUE, 
    all.y = FALSE)
manuals <- manuals[!duplicated(manuals)]

openarch <- merge(
    openarch, 
    manuals, 
    by = "occtitle_st", 
    all.x = TRUE, 
    all = FALSE)

openarch[is.na(HISCO.x), HISCO.x := HISCO.y]
openarch[is.na(HISCLASS.x), HISCLASS.x := HISCLASS.y]
openarch[is.na(HISCLASS_5.x), HISCLASS_5.x := HISCLASS_5.y]
openarch[is.na(HISCAM_NL.x), HISCAM_NL.x := HISCAM_NL.y]
openarch[is.na(OCC1950.x), OCC1950.x := OCC1950.y]

openarch[, HISCO.y := NULL]
openarch[, HISCLASS.y := NULL]
openarch[, HISCLASS_5.y := NULL]
openarch[, HISCAM_NL.x := NULL]
openarch[, OCC1950.y := NULL]
openarch[, N.x := NULL]
openarch[, N.y := NULL]

names(openarch) <- sub(".x", "", names(openarch))

cat(
    "matched ",
    openarch[, sum(!is.na(HISCO)) / .N],
    " to hisco including manual")

fwrite(
    x = openarch, 
    file = dataset,
    verbose = TRUE)
