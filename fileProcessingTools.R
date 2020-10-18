# note these are functions sourced elsewhere

# library(readr) # for read_csv
# #library(stringr) # not directly used at the moment
# #library(tibble) # not directly used at the moment
# library(dplyr) #  for %>%

reorg <- function(full.read) {
  # replaces 999 with NA's in %SD columns,
  # re-orders columns to place Cr norm'd then %SD cols to the right
  col.names <- colnames(full.read)
  SD.cols <- grep('%SD', col.names)
  Cr_normed.cols <- grep("Cr\\+PCr", col.names)

  Cr_normed.cols <- setdiff(Cr_normed.cols, SD.cols) # set differenct keeping the left
  full.read[,SD.cols] <- na_if(full.read[,SD.cols], 999)  # set %SD col 999 as NA

  full.read <-cbind(
    full.read[,-c(Cr_normed.cols, SD.cols)],  # remove norm'd and SD cols
    full.read[,Cr_normed.cols], full.read[,SD.cols])   # replace them as a group at end
  return(full.read)
}

mrs.csvparse <- function(pathloc) {
  # generic read csv and reorg (above) for mrs csv files.
  full.read <- read_csv(pathloc, col_names=T, trim_ws=T,
                        col_types=cols("ScanId"=col_character(),
                                       "group"=col_factor(),
                                       "gender"=col_factor(),
                                       "Region"=col_factor()
                        )
  )
  full.read <- reorg(full.read)
  return(full.read)
}

# why do these (commented) ones work at the command line and not in a function
# FIXME
# parameter: NULL becomes  parameter: NA
# for (i in 1:length(tst) ) { tst[i] <- ifelse( is.null(tst[[i]] ), NA, tst[[i]])  }
# for ( i in 1:length(lst) ) {  if ( is.null(lst[[i]]) ) { lst[i] <- NA } }
null2na.lst <- function (lst) {
  for ( i in 1:length(lst) ) {
      if ( is.null(lst[[i]]) ) { lst[i] <- NA }

  return(lst)
  }
}

# convenience for later operations
# assumes values that exist match - these won't be duplicated,
# if in doubt reduce to "SubjectId" and accept the duplication.
join.cols <- c("SubjectId", "age", "group", "gender",
               "ses_ladder_rung", "madrs_score", "ymrs_score")
