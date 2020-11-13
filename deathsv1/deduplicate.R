library("data.table")
library("stringi")
library("stringdist")

setDTthreads(threads = 8)

deaths = data.table::fread("C:\\Users\\Ruben\\Desktop\\openarch\\death\\openarch_deaths_amco_ages_sex.csv.gz")
cases = nrow(deaths)

# deduplication using string distances
# first an estimate of the number of candidates
deaths[, rowid := .I] # id to track duplicates

# blocking key
deaths[, regid := paste(
    "o", 
    EVENT_YEAR,
    EVENT_MONTH,
    EVENT_DAY,
    stringi::stri_trans_tolower(EVENT_PLACE_ST),
    # PR_GENDER_2, # standardised version made by Ruben, may miss out on some typos in names
    # stringi::stri_sub(PR_NAME_GN_ST, 1, 1), # assuming first letter is safest
    sep = "_")]

# check resulting number of candidates for comparison
deaths[, count := .N, by = regid]

# biggest candidate clusters (mostly WWII)
tail(deaths[!is.na(EVENT_YEAR), .N, by = regid][order(N)], 30)

# expected size is sum(N_cluster[i]*N_cluster[i])
# here as multiple of total dataset
deaths[, .N, by = regid][, sum(N ^ 2) / cases]
# 88, so should max at approx 88 GB of RAM

# only keep relevant variables to minimise memory usage
tomerge = deaths[
    !is.na(EVENT_MONTH) & 
    !is.na(EVENT_DAY) & 
    !is.na(EVENT_YEAR), 
    .(rowid, regid, PR_NAME_GN_ST, PR_NAME_SURN)]

candidates = merge(
    x = tomerge, 
    y = tomerge, 
    by = "regid",
    all = TRUE, 
    allow.cartesian = TRUE)

# could already exclude/drop rowid.x == rowid.y here
candidates[, firsim := stringsim(
    PR_NAME_GN_ST.x, 
    PR_NAME_GN_ST.y, 
    method = "osa", 
    nthread = 8)]
candidates[, sursim := stringsim(
    PR_NAME_SURN.x, 
    PR_NAME_SURN.y, 
    method = "osa", 
    nthread = 8)]

dupls = candidates[firsim * sursim > 0.8 & rowid.y != rowid.x, 
    .(clarid = c(unique(rowid.x), unique(rowid.y))),
    by = rowid.x]

# before merging this back in, we need to get address  that duplicates
# have been listed from both IDs; solution is to order dataset and then
# depuplicate the duplicates ¯\_(ツ)_/¯
setorder(dupls, rowid.x)
dupls = unique(dupls, by = "clarid")

deaths_dedup = dupls[deaths, on = c(clarid = "rowid")]

# non-na rowid.x is duplicates, so replace V1 with rowid.x there
deaths_dedup[, .N, by = list(is.na(rowid.x), PR_NAME_GN_ST == "")]
# 7.7m unduplicated, so we'll add 4-4.5m to that

# assign rowid to clarid for duplicates
deaths_dedup[!is.na(rowid.x), clarid := rowid.x]
# and flag duplicates (both)
deaths_dedup[, duplicate := FALSE]
deaths_dedup[!is.na(rowid.x), duplicate := TRUE]

deaths_dedup[, uniqueN(clarid)]
# so 12m deaths after dedup

# still some duplicates, identified by identical SOURCE_DIGITAL_ORIGINAL (ca. 86k)

# first seperate missing SOURCE_DIGITAL_ORIGINAL

deaths_nourl <- deaths_dedup[SOURCE_DIGITAL_ORIGINAL == "",]

# order remaining to keep most recent death year (some have no dates at all)

deaths_dedup <- deaths_dedup[order(SOURCE_DIGITAL_ORIGINAL, EVENT_YEAR, decreasing = TRUE),]
deaths_dedup <- deaths_dedup[!duplicated(SOURCE_DIGITAL_ORIGINAL) ,]
deaths_dedup <- rbind(deaths_dedup, deaths_nourl) # should give appr. 11.9 million

# sample a couple of duplicates to see if all makes sense
out = deaths_dedup[duplicate == TRUE
    ][ clarid %in% sample(clarid, 100)
    ][ order(clarid), 
        .(regid, PR_NAME_GN_ST, PR_NAME_SURN, PR_AGE_year, PR_FTHR_NAME_SURN, PR_MTHR_NAME_SURN)]
fwrite(out, "~/civreg/openarch/deathduplicates.csv")

fwrite(deaths_dedup, "C:\\Users\\Ruben\\Desktop\\openarch\\death\\openarch_deaths_clean.csv.gz")
