#' A function for finding y of x
#' 
#' This functiont takes a two column data frame as input. It
#' computes the max of the second column, and then returns the first
#' corresponding value from the first column
#' 
#' @author Richard D. Yentes \email{rdyentes@ncsu.edu}
#' @param x an nx2 data frame
#' @param right indicates whether to take the first or last instance of max, 
#' if it appears more than once
#' @export
maxLookup <- function(x, right=FALSE) {
  max <- max(x[,2])
  meanJ <- mean(x)
  sdJ <- sd(x)
  isMax <- which(x[,2] == max)
  if (length(isMax) > 1) { 
    if (right == FALSE) { 
      return(
        cbind.data.frame(
          x[first(isMax),],
          meanJ=meanJ,
          sdJ=sdJ
        )
      ) 
    }
    else { 
      return(
        cbind.data.frame(
          x[last(isMax),],
          meanJ=meanJ,
          sdJ=sdJ
        )
      ) 
    }
  }
  else { 
    return(
      cbind.data.frame(
        x[isMax,],
        meanJ=meanJ,
        sdJ=sdJ
      )
    )
  }
}