% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maxLookup.R
\name{maxLookup}
\alias{maxLookup}
\title{A function for finding y of x}
\usage{
maxLookup(x, right = FALSE)
}
\arguments{
\item{x}{an nx2 data frame}

\item{right}{indicates whether to take the first or last instance of max, 
if it appears more than once}
}
\description{
This functiont takes a two column data frame as input. It
computes the max of the second column, and then returns the first
corresponding value from the first column
}
\author{
Richard D. Yentes \email{rdyentes@ncsu.edu}
}
