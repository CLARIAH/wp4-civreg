library("data.table")
library("stringi")
library("stringdist")

setDTthreads(threads = 2)

args = commandArgs(trailingOnly = TRUE)
dataset = args[1]
namevars = args[2:length(args)]
namevars = c("GROOM_NAME_GN", "GROOM_NAME_SURN")


opeanarch = data.table::fread(dataset)
cases = nrow(opeanarch)

# still some duplicates, identified by identical SOURCE_DIGITAL_ORIGINAL (ca. 86k)
opeanarch = rbindlist(
    list(
        opeanarch[SOURCE_DIGITAL_ORIGINAL != "" ][ 
            order(-EVENT_YEAR) ][ # keep earliest instance
            !duplicated(SOURCE_DIGITAL_ORIGINAL)],
        opeanarch[SOURCE_DIGITAL_ORIGINAL == ""]
    )
)

# further deduplication using string distances on names
opeanarch[, rowid := .I] # id to track duplicates

# blocking key
opeanarch[, regid := paste(
    "o", # still useful?
    EVENT_YEAR,
    EVENT_MONTH,
    EVENT_DAY,
    stringi::stri_trans_tolower(EVENT_PLACE_ST),
    # PR_GENDER_2, # standardised version made by Ruben, may miss out on some typos in names
    # stringi::stri_sub(PR_NAME_GN_ST, 1, 1), # assuming first letter is safest
    sep = "_")]

# check resulting number of candidates for comparison
opeanarch[, count := .N, by = regid]

# biggest candidate clusters
tail(opeanarch[!is.na(EVENT_YEAR), .N, by = regid][order(N)], 30)

# expected size is sum(N_cluster[i]*N_cluster[i])
# here as multiple of total dataset
opeanarch[, .N, by = regid][, sum(N ^ 2) / cases]
# 10

gc()

# only keep relevant variables to minimise memory usage
tomerge = opeanarch[
    !is.na(EVENT_MONTH) & 
    !is.na(EVENT_DAY) & 
    !is.na(EVENT_YEAR), 
    .(rowid, regid, .SD),
    .SDcols = namevars]

setnames(tomerge, paste0(".SD.", namevars), namevars)

candidates = merge(
    x = tomerge, 
    y = tomerge, 
    by = "regid",
    all = TRUE, 
    allow.cartesian = TRUE)

# could already exclude/drop rowid.x == rowid.y here
candidates[, firsim := stringsim(
    get(paste0(namevars[1], ".x")),
    get(paste0(namevars[1], ".y")),
    # PR_NAME_GN_ST.x, 
    # PR_NAME_GN_ST.y, 
    method = "osa", 
    nthread = 2)]
candidates[, sursim := stringsim(
    get(paste0(namevars[2], ".x")),
    get(paste0(namevars[2], ".y")),
    # PR_NAME_SURN.x, 
    # PR_NAME_SURN.y, 
    method = "osa", 
    nthread = 2)]

dupls = candidates[firsim * sursim > 0.8 & rowid.y != rowid.x, 
    .(clarid = c(unique(rowid.x), unique(rowid.y))),
    by = rowid.x]

# before merging this back in, we need to get address that duplicates
# have been listed from both IDs; solution is to order dataset and take unique
setorder(dupls, rowid.x)
dupls = unique(dupls, by = "clarid")

opeanarch_dedup = dupls[opeanarch, on = c(clarid = "rowid")]

# non-na rowid.x is duplicates, so replace V1 with rowid.x there
opeanarch_dedup[, .N, by = list(is.na(rowid.x), get(namevars[1]) == "")]
# 7.7m unduplicated, so we'll add 4-4.5m to that

# assign rowid to clarid for duplicates
opeanarch_dedup[!is.na(rowid.x), clarid := rowid.x]
# and flag duplicates (both)
opeanarch_dedup[, duplicate := FALSE]
opeanarch_dedup[!is.na(rowid.x), duplicate := TRUE]

opeanarch_dedup[, uniqueN(clarid)]

# first seperate missing SOURCE_DIGITAL_ORIGINAL
# sample a couple of duplicates to see if all makes sense
out = opeanarch_dedup[duplicate == TRUE
    ][ clarid %in% sample(clarid, 100)
    ][ order(clarid), 
        .SD,
        .SDcols = patterns("regid|NAME|AGE|YEAR|year")]
fwrite(out, "example_duplicates.csv")

fwrite(
    x = opeanarch_dedup, 
    file = dataset,
    verbose = TRUE)
