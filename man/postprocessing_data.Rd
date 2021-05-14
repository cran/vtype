\name{postprocessing_data}
\alias{postprocessing_data}
\title{
post processing of data after prediction
}
\description{
Internal function.
}
\usage{
postprocessing_data(rf_model, agr_data, data, qvalue=0.75, miss_values=NULL)
}
\arguments{
  \item{rf_model}{
  RF model
}
  \item{agr_data}{
  agregeted data
}
  \item{data}{
  crude data (data,frame)
}
  \item{qvalue}{
  quality value in [0.1, 1.0]
}
  \item{miss_values}{
 a character vector of values considered to be invalid (missing)
}
}
\value{
a data frame
}
\author{
Andreas Schulz
}

\keyword{internal}