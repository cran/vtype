\name{date_format_i}
\alias{date_format_i}
\title{
The function try to identify the format of a Date variable
}
\description{
Internal function.
}
\usage{
date_format_i(x, qvalue=0.75, miss_values = NULL)
}
\arguments{
  \item{x}{
a character vector
}
  \item{qvalue}{
quality value
}
  \item{miss_values}{
a character vector of values considered to be invalid (missing)
}
}
\value{
a single string with date format
}
\author{
Andreas Schulz
}

\keyword{internal}
