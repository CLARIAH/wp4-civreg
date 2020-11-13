These scripts allow to replicate the death certificates dataset (version 1.1) as can be found on
the [IISH Dataverse Dutch Civil Registration](https://datasets.iisg.amsterdam/dataset.xhtml?persistentId=hdl:10622/PCAEGG) 

This version uses the January 2020 csv files from [openarch.nl](https://www.openarch.nl/). An updated version is currently under construction.

  # Order of execution of .R scripts:

  1. openarch_csv_per_certificatetype.R
  2. openarch_placename_matching_deaths.R
  3. Standardize ages in openarch_csv_per_certificatetype.R
  4. openarch_death_sex.R
  5. deduplicate.R
