# pearson's r, spearmans rho, as abs.r
target.cutoffs <- c("abs.r"=0.7,"p.val"=0.05, "adj.type"="holm")
kendall.cutoffs <- c("abs.tau"=0.65, "p.val"=0.05, "adj.type"="holm")
# adj.type string: used with p.val both will be saved, adj.p will be used for the cutoff
# see p.adjust.methods for details
#"holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")

library(dplyr)
#library(stringr)
library(readr)
# library(ltm)
# library(Hmisc)
# library(ggplot2)
# library(ggcorrplot)
library(ggpubr)
# ggcorplot  ggcor  , "GGally",
#     graph a corelation coef graphing on a scatter plot (nifty)
### https://rpkgs.datanovia.com/ggpubr/reference/stat_cor.html   stat_cor

# bring in functions from another R file using a relative filepath
source("fileProcessingTools.R")
source("StatsTools.R")
#read and parse the individual csv files
putamen <- mrs.csvparse("putamen_demo.csv")
vermis <- mrs.csvparse("vermis_demo.csv")
############################################################################
## Note 1
## IF we are no concerned with the % SD (or know otherwise know a priori /interactively
## which columns can be trimmed, proper form would be to only read those columns and others
## needed for output. Combine like with main, but split into to vars/memory objects
## analysis and output records only (ex. ScanID)
## then do a filter-split (-join) etc. on the fly for each analysis set
#############################################################################
#########


# row wise is just for a single bulk pass and dump - OK for storage (sql style and all) but
# col wise is more data analystic and very much the R way
rowwise <- full_join(putamen, vermis, by=join.cols, suffix=c(".p", ".v") )
# reorg so its id's;  categorical;  numeric (raw, norm'd, SD)
# SubjectId, ScanId.p Region.p ScanID.v Region.v group, gender, age, other-numeric
rowwise <- cbind(rowwise[,c(1, 8, 125, 9, 126, 2)], rowwise[,c(4, 3)], rowwise[,c(5,6,7)], rowwise[,10:124], rowwise[,127:241])
# its not all useful, but 4 to 7 are factors (categorical) and 8 on are numeric (int or double) now, 1,2,3 are only external labels (subject & scan id's.

main <- rbind(putamen, vermis)
main <- cbind(main[,c(1,8,9,2,4,3,5,6,7)], main[,10:124])
# main col  6 on are numeric

# 0's: PCh [col23], lip13b [col,38],
# also remove 46 lib13a+b
main.trimmed <- cbind(main[,3:9], main[,14:87])


# all by all of covariance and correlation just a data barf
# pearsons using Base R
main.covar <- cov(main[,6:124])
rowwise.covar <- cov(rowwise[,8:241]) # remove join cols also

# pvals  failing on NA rows add check/cleaner or any pval testing etc will break
# corWtest

# corAll  -- cor.test (inner from the inner func) doesn't do well with a high fraction of NA's
## note that  the corWtest and corAll are because of the NA's and working blind
## with NA free or removable (known) use a prebuilt like ltm::rcor.test

main.kendall <- corAll(main[,6:124], min_include=75)
rowwise.kendall <- corAll(rowwise[,8:241], min_include=75) # remove join.cols also

main.trimmed.pearson <- ltm::rcor.test(main.trimmed[,4:81], method="pearson")
main.trimmed.spearman <- ltm::rcor.test(main.trimmed[,4:81], method="spearman")
main.trimmed.kendall <- ltm::rcor.test(main.trimmed[,4:81], method="kendall")

# p.adj holm (and BH?)

# plot age, scores raw
# age scores, normed (removed the fraction in the name
#  page as raw, normed
#           pearson
#           spearman
#           kendall

# for any meeting criteria ...


# then
# "removables" dump - external to analysis scanId, FWHM (full width / height max) , SNR (single/noise), Row, Col, maybe the %SD's (of individual) - can we even compare between tables ( V & P )- down to "working values" only



# for col in (group, gender, age-group) and 2 of (more if there were sufficient samples)
# 1) simple things, mean & median avg +/- SD (of net) by location by factor(intersections): age-slice, ses-slice, ymrs-slice, madrs-slice, gender, group
# check for diffs and trends

# 2) check for creatine ratio's see if anything shifts in normalizer

# run regression (with normed rmsd) , one-to-one, one-to-sum(pair)/2, one-to-sum(triple)/3, pair-to-pair, (norm'd & un-norm'd), ?? other norm'd) ,  un-weighted, mean-weighted (all & all-gender/ if more samples gender+age also),




# https://ggplot2.tidyverse.org/reference/geom_smooth.html

# p.adjust

#GRAPHING
# graph sets reverse this order (age-slice) ses-slice, ymrs-slice, madrs-slice, gender, group
# # 'cause I always forget how to do multiple plots in an image with gg
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html
# https://cran.r-project.org/web/packages/egg/vignettes/Overview.html
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# https://cran.r-project.org/web/packages/gtable/vignettes/profiling.html # oy
# https://cran.r-project.org/web/packages/gridExtra/vignettes/gtable.html
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
