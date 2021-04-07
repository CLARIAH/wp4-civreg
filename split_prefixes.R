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
      NEW_PREFIX_VAR = stringi::stri_extract_first_regex(
        strings, pattern, case_insensitive = TRUE),
      CLEANED_SURNAME_VAR = stringi::stri_replace_first_regex(
        strings, pattern, "", case_insensitive = TRUE),
      stringsAsFactors = FALSE
    )
  )
}

list_cleaned_names <- split_prefixes(data$SURNAME_VAR)

data <- cbind(data, list_cleaned_names)

