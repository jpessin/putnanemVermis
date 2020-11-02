# note these libraries are intentionally commented and only here for reference
# they are called in by the sourcing script.

# library(readr) # for read_csv in mrs.csvparse
# #library(stringr) # not used at the moment could be used to replace grep w/Tidy func


reorg <- function(full.read) {
  # replaces 999 with NA's in %SD columns,
  # re-orders columns to place Cr norm'd then %SD cols to the right
  col.names <- colnames(full.read)
  SD.cols <- grep('%SD', col.names)
  Cr_normed.cols <- grep("Cr\\+PCr", col.names)

  Cr_normed.cols <- setdiff(Cr_normed.cols, SD.cols) # set difference keeping the left
  full.read[,SD.cols] <- na_if(full.read[,SD.cols], 999)  # set %SD col 999 as NA

  full.read <-cbind(
    full.read[,-c(Cr_normed.cols, SD.cols)],  # remove norm'd and SD cols
    full.read[,Cr_normed.cols], full.read[,SD.cols])   # replace them as a group at end
  return(full.read)
}


mrs.csvparse <- function(pathloc) {
  # use the tidyverse readr::read_csv and reorg (above) for mrs csv files.
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
