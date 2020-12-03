# death certificate pipeline

# update url list for open arch csvs
curl 'https://www.openarch.nl/exports/5cfb55ebaf9fe5aaf216f6da1229c632/' \
    | grep 'http.*bso.*zip' \
    > bso_urls.txt

v2020jan20=openarch_bso_url_list_januari2020.txt
vtoday=bso_urls.txt
dataset="deathstest.csv"

# fetch csvs from openarch
# set first argument to vtoday to get a new set straight from openarch
Rscript --verbose openarch_csv_per_certificatetype.R $v2020jan20 $dataset > openarch_csv_per_certificatetype.Rout 2>&1

# standardise placenames
Rscript --verbose openarch_placename_matching_deaths.R $dataset > openarch_placename_matching_deaths.Rout 2>&1

# standardise ages
Rscript --verbose 'Standardize ages in openarch_csv_per_certificatetype.R' $dataset > 'Standardize ages in openarch_csv_per_certificatetype.Rout' 2>&1

# standardise occupations
Rscript --verbose occtitles.R $dataset PR_OCCUPATION > occtitles.Rout 2>&1

# impute sex from names
Rscript --verbose openarch_death_sex.R $dataset > openarch_death_sex.Rout 2>&1

# deduplicate
Rscript --verbose deduplicate.R $dataset > deduplicate.Rout 2>&1
