#

library(dplyr)
library(stringr)
library(readr)
library(tibble)
library(hablar)
library(ggplot2)

# bring in functions from another R file using a relative filepath
source("fileProcessingTools.R")

#read and parse the individual csv files
putamen <- mrs.csvparse("putamen_demo.csv")
vermis <- mrs.csvparse("vermis_demo.csv")

full.data <- full_join(putamen, vermis, by=join.cols, suffix=c(".p", ".v") )
# reorg so its id's categorical numeric
# SubjectId, ScanId.p Region.p ScanID.v Region.v group, gender, age, other-numeric
full.data <- cbind(full.data[,c(1,8,9, 125, 126, 2 )], full.data[,c(4, 3)], full.sample[,c(5,6,7)], full.data[,10:124], full.data[,127:241])
# its not all useful, but 3, and 5 to 7 are factors (catagorical) and 8 on are numeric (int or double) now, 1,2,4 are only external labels.

# args for cov and cor
# na.rm=T remove NA's - (ignore) default F
# use  one of  "everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs
# method pearson kedall or spearman (default pearson)

# cov --> use person
pearson.cov <- cor()




# cov kendall (or spearman) probably should use kendall-tau b insead
kendall.cor <- cor()
kendal.test <- cor.test()

# regression loop numerics
# one ~ one, lin-lin, lin-log, log-log
# one ~ polynom 4
# one ~ two, non-%sd
# Weighted RMSD's )pvals?

# regression logit
# logit ~ logit
# logit ~ lin
# logit ~ log
# logit ~ poly 4
# weighted RMDSD's Pvals bH adj-p

#GRAPHING
