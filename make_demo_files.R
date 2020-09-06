# a script to create full size demo files from the provided example files

# read the header and contents separately - treating the header as a row of contents
# we're doing this because:
# A) for a row or column name (column header), R strongly prefers only
#   Alpha-numeric underscore and period, by default these file-reading functions
#   will substitute a period for everything else
# B) it doesn't make sense to change the headers beforehand

# typically if we do this we want convert the header to a char-vector,
# here we are trimming whitespace,
# the trimws function will do this for us and convert along the way.

putamen.head <- read.csv('starting_info/MRS_H1_Putamen_JacobTemplate.csv',
                         header=F, nrows=1)
putamen.head <- trimws(putamen.head)
putamen.data <- read.csv('starting_info/MRS_H1_Putamen_JacobTemplate.csv',
                        header=F, skip=1)

vermis.head <- read.csv('starting_info/MRS_H1_Vermis_JacobTemplate.csv',
                        head=F, nrows=1)
vermis.head <- trimws(vermis.head)
vermis.data <- read.csv('starting_info/MRS_H1_Vermis_JacobTemplate.csv',
                        head=F, skip=1)

## example method for identifying columns by header content

## note that the grep func is using a regex, where the '+' has a special meaning
##  we can use the standard syntax of a double backslash to ignore the special meaning
## putamen.cr_normed <- grep("Cr\\+PCr", putamen.head)
## or to keep it easier to read add the fixed=TRUE argument, (run as "fixed string")
## this will then do plain string matching like the terminal fgrep, grep -F.

# putamen.cr_normed <- grep("Cr+PCr", putamen.head, fixed=T)
# putamen.SD <- grep('%SD', putamen.head)

# vermis.cr_normed <- grep("Cr+PCr", vermis.head, fixed=T)
# vermis.SD <- grep('%SD', vermis.head)

# factor_cols: group gender  Region
# int_cols: age ses_ladder_rung madrs_score ymrs_socre Row Column
# special % SD  is percent stdev, 999 is not detected -> NaN (no stdev) 0 on raw
# putamen dim 100,123
# vermis dim 115, 123

# this just naively replicates the exmaple values
putamen.out <- do.call("rbind", replicate(25, putamen.data[,2:124], simplify=F))
putamen.out <- cbind(seq(1:100), putamen.out)

vermis.out <- do.call("rbind", replicate(28, vermis.data[,2:124], simplify=F))
vermis.out <- rbind(vermis.out, vermis.data[1:3,2:124])
vermis.out <- cbind(seq(1:115), vermis.out)

# To reinsert the header use write.table (not write.csv)
write.table(putamen.out, "putamen_demo.csv", row.names=F, col.names=putamen.head,
            sep=',')
write.table(vermis.out, "vermis_demo.csv", row.names=F, col.names=vermis.head,
            sep=',')
