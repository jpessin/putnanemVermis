# note these are functions source'd elsewhere

# library(readr) # for read_csv
# #library(stringr) # not directly used at the moment
# #library(tibble) # not directly used at the moment
# library(dplyr) #  for %>%
# library(hablar) # for convert and convert types

convert.colTypes <- function(header, full.read) {
  SD.cols <- header[grepl('%SD', header)][1,]
  Cr_normed.cols <- grep("Cr\\+PCr", header)
  int.type <- c("SubjectId", "age", "Col", "Row",
                "ses_ladder_rung", "madrs_score", "ymrs_score",
                as.vector(SD.cols, mode = "character")
                )
  factor.type <- c("group", "gender","Region")
  char.type <-c("ScanId")
  full.read <- full.read %>% convert(
    int(all_of(int.type)),
    fct(all_of(factor.type)),
    chr(all_of(char.type))
  )
  # while we're in here; replace the 999 with NA's in the %SD cols
  #full.read[,SD.cols] <- na_if(full.read[,SD.cols], 999)

  return(full.read)
}

mrs.csvparse <- function(pathloc) {
  header <- read_csv(pathloc, col_names=F, n_max=1, trim_ws=T)
  full.read <- read_csv(pathloc, col_names=T, trim_ws=T)
  full.read <- convert.colTypes(header, full.read)

  return(full.read)
}

# Assumes values that exist match - these won't be duplicated, if in doubt
# reduce to "SubjectId" and accept the duplication.
join.cols <- c("SubjectId", "age", "group", "gender",
               "ses_ladder_rung", "madrs_score", "ymrs_score")

