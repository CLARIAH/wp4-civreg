# reshape openarch data into zeeland challenge format
### script to generate the 'Registrations' table according to LINKS/ Zeelandchallenge

#   use openarch_birth, openarch_marriage & openarch_death files:
#   generated with openarch_per_certificatetype.R at https://github.com/CLARIAH/wp4-civreg or as csv available at Surfdrive > openarch > 
#   openarch_merged_per_certificate


### Generating openarch_registrations.csv

## from openarch_birth

library(data.table)
openarch_birth <- fread("C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_birth.csv", 
                                       header = T, sep = ';') 

## Add id_registration per certificate 

openarch_birth$id_registration <- seq.int(nrow(openarch_birth))


## recode variables according to LINKS

registrations_birth <- openarch_birth[, list(id_registration, EVENT_TYPE, SOURCE_TYPE, SOURCE_PLACE, SOURCE_DATE_DAY, SOURCE_DATE_MONTH,
                                             EVENT_DAY, EVENT_MONTH, EVENT_YEAR,
                                             SOURCE_DATE_YEAR, SOURCEREFERENCE_DOCUMENTNUMBER, SOURCE_DIGITAL_ORIGINAL, SOURCE_REMARK)]

registrations_birth[SOURCE_TYPE == "BS Geboorte", registration_maintype := 1,  ]

registrations_birth[EVENT_TYPE == "Geboorte", registration_type := "g",  ]

registrations_birth[, registration_location := SOURCE_PLACE,  ]

# registration_flag is set to "0" instead of "NULL" (as in LINKS) to avoid R reading it as a deletion 

registrations_birth[!is.na(SOURCE_DATE_DAY), registration_day := SOURCE_DATE_DAY,  ][!is.na(SOURCE_DATE_DAY), registration_flag := 0, ]
registrations_birth[!is.na(SOURCE_DATE_MONTH), registration_month := SOURCE_DATE_MONTH,  ][!is.na(SOURCE_DATE_MONTH), registration_flag := 0, ]
registrations_birth[!is.na(SOURCE_DATE_YEAR), registration_year := SOURCE_DATE_YEAR,  ][!is.na(SOURCE_DATE_YEAR), registration_flag := 0, ]

registrations_birth[is.na(SOURCE_DATE_DAY), registration_day := EVENT_DAY,  ][is.na(SOURCE_DATE_DAY), registration_flag := 1, ]
registrations_birth[is.na(SOURCE_DATE_MONTH), registration_month := EVENT_MONTH,  ][is.na(SOURCE_DATE_MONTH), registration_flag := 1, ]
registrations_birth[is.na(SOURCE_DATE_YEAR), registration_year := EVENT_YEAR,  ][is.na(SOURCE_DATE_YEAR), registration_flag := 1, ]

registrations_birth[, registration_seq := SOURCEREFERENCE_DOCUMENTNUMBER,]

registrations_birth[, registration_url := SOURCE_DIGITAL_ORIGINAL,]


# keep LINKS variables plus URL to certificate

registrations_birth_final <- registrations_birth[, list(id_registration, registration_maintype,registration_type, registration_location, registration_day,
                                                        registration_month, registration_year, registration_flag, registration_seq, registration_url)]

## from openarch_marriages

openarch_marriage <- fread("C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_marriage.csv", 
                        header = T, sep = ';') 

## Add id_registration starting from last # of id_registration birth

openarch_marriage$id_registration <- seq.int(16068976, 21509471)

## recode variables according to LINKS

registrations_death <- openarch_marriage[, list(id_registration, EVENT_TYPE, SOURCE_TYPE, SOURCE_PLACE, SOURCE_DATE_DAY, SOURCE_DATE_MONTH,
                                             EVENT_DAY, EVENT_MONTH, EVENT_YEAR,
                                             SOURCE_DATE_YEAR, SOURCEREFERENCE_DOCUMENTNUMBER, SOURCE_DIGITAL_ORIGINAL, SOURCE_REMARK)]


registrations_death[SOURCE_TYPE == "BS Huwelijk", registration_maintype := 2,  ]

registrations_death[EVENT_TYPE == "Huwelijk", registration_type := "h",  ]
registrations_death[EVENT_TYPE == "Echtscheiding", registration_type := "s",  ]

registrations_death[, registration_location := SOURCE_PLACE,  ]

# registration_flag is set to "0" instead of "NULL" (as in LINKS) to avoid R reading it as a deletion 

registrations_death[!is.na(SOURCE_DATE_DAY), registration_day := SOURCE_DATE_DAY,  ][!is.na(SOURCE_DATE_DAY), registration_flag := 0, ]
registrations_death[!is.na(SOURCE_DATE_MONTH), registration_month := SOURCE_DATE_MONTH,  ][!is.na(SOURCE_DATE_MONTH), registration_flag := 0, ]
registrations_death[!is.na(SOURCE_DATE_YEAR), registration_year := SOURCE_DATE_YEAR,  ][!is.na(SOURCE_DATE_YEAR), registration_flag := 0, ]

registrations_death[is.na(SOURCE_DATE_DAY), registration_day := EVENT_DAY,  ][is.na(SOURCE_DATE_DAY), registration_flag := 1, ]
registrations_death[is.na(SOURCE_DATE_MONTH), registration_month := EVENT_MONTH,  ][is.na(SOURCE_DATE_MONTH), registration_flag := 1, ]
registrations_death[is.na(SOURCE_DATE_YEAR), registration_year := EVENT_YEAR,  ][is.na(SOURCE_DATE_YEAR), registration_flag := 1, ]

registrations_death[, registration_seq := SOURCEREFERENCE_DOCUMENTNUMBER,]

registrations_death[, registration_url := SOURCE_DIGITAL_ORIGINAL,]

registrations_marriage_final <- registrations_death[, list(id_registration, registration_maintype,registration_type, registration_location, registration_day,
                                                        registration_month, registration_year, registration_flag, registration_seq, registration_url)]


## from openarch_death

openarch_death <- fread("C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_death.csv", 
                           header = T, sep = ';') 

## Add id_registration starting from last # of id_registration marriages

openarch_death$id_registration <- seq.int(21509472, 36108333)

## recode variables according to LINKS

registrations_death <- openarch_death[, list(id_registration, EVENT_TYPE, SOURCE_TYPE, SOURCE_PLACE, SOURCE_DATE_DAY, SOURCE_DATE_MONTH,
                                                   EVENT_DAY, EVENT_MONTH, EVENT_YEAR,
                                                   SOURCE_DATE_YEAR, SOURCEREFERENCE_DOCUMENTNUMBER, SOURCE_DIGITAL_ORIGINAL, SOURCE_REMARK)]


registrations_death[SOURCE_TYPE == "BS Overlijden", registration_maintype := 3,  ]

registrations_death[EVENT_TYPE == "OVerlijden", registration_type := "o",  ]


registrations_death[, registration_location := SOURCE_PLACE,  ]

# registration_flag is set to "0" instead of "NULL" (as in LINKS) to avoid R reading it as a deletion 

registrations_death[!is.na(SOURCE_DATE_DAY), registration_day := SOURCE_DATE_DAY,  ][!is.na(SOURCE_DATE_DAY), registration_flag := 0, ]
registrations_death[!is.na(SOURCE_DATE_MONTH), registration_month := SOURCE_DATE_MONTH,  ][!is.na(SOURCE_DATE_MONTH), registration_flag := 0, ]
registrations_death[!is.na(SOURCE_DATE_YEAR), registration_year := SOURCE_DATE_YEAR,  ][!is.na(SOURCE_DATE_YEAR), registration_flag := 0, ]

registrations_death[is.na(SOURCE_DATE_DAY), registration_day := EVENT_DAY,  ][is.na(SOURCE_DATE_DAY), registration_flag := 1, ]
registrations_death[is.na(SOURCE_DATE_MONTH), registration_month := EVENT_MONTH,  ][is.na(SOURCE_DATE_MONTH), registration_flag := 1, ]
registrations_death[is.na(SOURCE_DATE_YEAR), registration_year := EVENT_YEAR,  ][is.na(SOURCE_DATE_YEAR), registration_flag := 1, ]

registrations_death[, registration_seq := SOURCEREFERENCE_DOCUMENTNUMBER,]

registrations_death[, registration_url := SOURCE_DIGITAL_ORIGINAL,]

registrations_death_final <- registrations_death[, list(id_registration, registration_maintype,registration_type, registration_location, registration_day,
                                                              registration_month, registration_year, registration_flag, registration_seq, registration_url)]


## merge three registration files into openarch_registrations 


openarch_registrations <- rbind(registrations_birth_final, registrations_marriage_final, registrations_death_final)

fwrite(openarch_registrations, 
       "C:\\Users\\Ruben\\Documents\\02. Werk\\Clariah\\openarch\\openarch_registrations.csv", 
       sep=";", row.names = FALSE, na = "")

### Generate openarch_persons.csv

# TO DO


