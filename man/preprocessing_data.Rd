\name{preprocessing_data}
\alias{preprocessing_data}
\title{
pre processing of crude data 
}
\description{
Internal function.
}
\usage{
preprocessing_data(data, qvalue = 0.75, miss_values=NULL)
}
\arguments{
  \item{data}{
  a data.frame
}
  \item{qvalue}{
  quality value in [0.1, 1.0]
}
  \item{miss_values}{
  a character vector of values considered to be invalid (missing)
}
}

\value{
a data.frame
}

\author{
Andreas Schulz
}

\keyword{internal}