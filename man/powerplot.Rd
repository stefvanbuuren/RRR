\name{powerplot}
\alias{powerplot}
\title{Create the chart with power lines}
\usage{
  powerplot(powertables, power = 0.8, main = NULL,
    isPBS = TRUE, ...)
}
\arguments{
  \item{powertables}{list of calculated power on a grid on
  n1, n2 and percent}

  \item{power}{Power of the test}

  \item{main}{Text for title}

  \item{isPBS}{Logical indicating the the x-axis is PBS or
  compound group}

  \item{\dots}{Additional arguments passed to
  \code{eqscplot()}}
}
\value{
  NULL
}
\description{
  Create the chart with power lines
}

