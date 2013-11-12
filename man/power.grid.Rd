\name{power.grid}
\alias{power.grid}
\title{Calculate the power of t-test with unequal variances}
\usage{
  power.grid(n1 = 2:30, n2 = 2:30, mu1 = 0, mu2 = 1,
    sd1 = 1, sd2 = 1,
    alternative = c("two.sided", "less", "greater"),
    percent = 100, alpha = 0.05, power = 0.8,
    interpolate.sd2 = FALSE, ...)
}
\arguments{
  \item{n1}{Vector of samples sizes for untreated, diseased
  group}

  \item{n2}{Vector of samples sizes for comparison group}

  \item{mu1}{Mean of outcome for untreated group}

  \item{sd1}{Standard deviation of outcome for untreated
  group}

  \item{mu2}{Mean of outcome for comparison group}

  \item{sd2}{Standard deviation of outcome for comparison
  group}

  \item{alternative}{String indicating one- or two sided
  test: "two.sided", "less" or "greater". Default
  "two.sided"}

  \item{percent}{vector indicating for which percentages of
  effect size the power curves should be calculated}

  \item{alpha}{Significance level of the test}

  \item{power}{Power of the test}

  \item{interpolate.sd2}{Should the sd2 parameter be
  interpolated along with \code{percent}?}

  \item{\dots}{Additional arguments passed to
  \code{pwr.t2n.test()}}
}
\value{
  A data frame with rows corresponding to different designs
}
\description{
  Calculate the power of t-test with unequal variances
}

