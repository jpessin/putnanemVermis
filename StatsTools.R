
corWtest <- function(x, y, use="pairwise.complete", method="kendall",
                     alternative="two.sided", exact=NULL, continuity=FALSE) {
  correl <- cor(x,y, use=use, method=method)
  test <-  cor.test(x, y, alternative=alternative, method=method,
                    exact=exact, continuity=continuity)
  return(c("Cor"=correl, test))
}

# reformat to have same share as ltm::rcor.test
corAll <- function (df, min_include=75, use="pairwise.complete", method="kendall",
                    alternative="two.sided", exact=NULL, continuity=FALSE)
  # min_include is minimum numeric values for each column, else just NA's which cause problems for cor.test
# FIXME -- align output with ltm:rcor.test, $cor.mat and $p.values --> origin col1, origin col2, pval, other info, (adj.p)
    {
  col.names <- colnames(df)
  dim.size <- dim(df)
  colnum <- dim.size[2]
  # lets prepopulate a new DF - corWtest is  9 by:
  prepop <- as.data.frame( replicate( 9, vector(mode="double",
                      length=( colnum^2 %/% 2 + colnum %/% 2 )
                                 )))
  prepop.names <- c("Correlation", "statistic", "parameter", "p.val", "estimate",
                   "null.val", "alternative", "method", "data.name")
  colnames(prepop) <- prepop.names

  # pre cal for later checks of at least min_include non_na values
  # We probably shouldn't be checking %SD with these funcs but lets not break
  # during a run - adding a check min numeric values in col, as an if block
  numeric.cnt <- sapply(df, function(x) dim.size[1] - sum(is.na(x)))
  cnt <- 1
  # here's a fun one in R operator precedence is unary arthmetic sequences binary arithmetic
  for (c1 in 1: (colnum-1) ) { # Inner loop can be a column_slice->apply instead
    for (c2 in(c1+1):colnum) {
      if ( (numeric.cnt[[c1]] >= min_include ) & (numeric.cnt[[c2]] > min_include ) ) {
        buff <- corWtest(df[,c1], df[,c2], use=use, method=method,
                                alternative=alternative,
                                exact=exact, continuity=continuity)
        buff$data.name <- paste(col.names[c1], "and", col.names[c2])
        # fixme -- this should be taken care of by something like null2na.lst from
        # fileProcessingTools but for now knowing with the defaults its coming from
        # parameters we're hardcoding it
        # buff <- null2na.lst(buff) # So we replace them with NA (func in file processing tools)
        # FIXME
        buff$parameter <- NA
        prepop[cnt,] <- buff
      }
      else {
        buff <- list(NA,NA,NA,NA,NA,NA,NA,NA, paste(col.names[c1], "and", col.names[c2]) )
        prepop[cnt,] <- buff
      }
      cnt <- cnt + 1 # updating cnt on the inner loop
    }
  }
  # convert to tidy-tibble
  prepop <- as_tibble(prepop)
  return(prepop)
}


CorAll.indiv.factors <- function(factor.col, col.start, col.stop, correlation.df, min_include=75,
                                  use="pairwise.complete", method="kendall",
                                  alternative="two.sided", exact=NULL,
                                  continuity=FALSE) {
  # factor col is column number on corallation.df
  buff.list <- list()
  facs <- levels(correlation.df[[factor.col]]) # of factor names
  for (fac in facs) {
    buff <- correlation.df[correlation.df[[factor.col]] == fac, col.start:col.stop]
    buff <- corAll(buff, min_include=min_include,
                   use=use, method=method,
                   alternative=alternative, exact=exact,
                   continuity=continuity)
    buff.list[[ paste(fac, factor.col, sep='.') ]] <- buff
  }
  return(buff.list)
}
