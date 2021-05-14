\name{vtype}
\alias{vtype}
\title{
Estimates the Variable Type in Error Afflicted Data.
}
\description{
Estimates the type of variables in not quality controlled data.
}
\usage{
vtype(data, qvalue=0.75, miss_values=NULL)
}

\arguments{
  \item{data}{
a data frame.
}
  \item{qvalue}{
Quality value from 0.1 to 1, specifies the proportion of data assumed to be well formatted.
The default value of 0.75 works very well most of the time. If the quality of the data is very poor, the q-value can be reduced.
If the sample size is very small, it can be increased to use a greater portion of data.
}
  \item{miss_values}{
a character vector of values considered to be invalid (missing).
Important, if missing values were coded as -9 or 9999, otherwise it looks like valid numeric values.
Values as NA, NaN, Inf, -Inf, NULL and spaces are automatic considered as invalid (missing) values.
}
}
\value{
A data frame with following entries
\itemize{
      \item{variable:     name of the variable }
      \item{type:         estimated variable type }
      \item{probability:  probability for estimated type }
      \item{format:       format of the variable (depending on the type) }
      \item{class:        broader categorization of type }
      \item{alternative:  posible alternative type with lesser probability  }
      \item{n:            number of non-missing values }
      \item{missings:     number of missing values }
}
}
\details{
The prediction is based on a pre-trained random forest model, trained on over 5000 medical variables with OOB accuracy of 99pct.
The accuracy depends heavily on the type and coding style of data. For example, often categorical variables are coded as integers 1 to x, if the number of categories is very large, there is no way to distinguish it from a continuous integer variable.
Some types are per definition very sensitive to errors in data, like ID, missing or constant, where a single alternative non-missing value makes it not constant or not missing anymore.
The data is assumed to be cross sectional, where ID is unique (no multiple entries per ID).
}

\examples{

# Application to a sample data set included in the package. 

vtype(sim_nqc_data, miss_values='9999')

}
\keyword{type}
\keyword{variable}
\keyword{data}
