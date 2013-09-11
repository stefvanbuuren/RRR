\name{get.table}
\alias{get.table}
\title{Calculate table of pair (n1, n2) with a given minimal power}
\usage{
  get.table(powertables, k = 3)
}
\arguments{
  \item{powertables}{A list produced by the
  \code{calculate.powertables()}}

  \item{k}{number of active treatment groups to be tested}
}
\value{
  A data frame combinations of (n1, n2)
}
\description{
  Calculate table of pair (n1, n2) with a given minimal
  power
}

