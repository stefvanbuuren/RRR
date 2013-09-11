\name{get.musd}
\alias{get.musd}
\title{Returns the musd matrix from the inputs}
\usage{
  get.musd(outcome, hist.induced.mu, hist.induced.sd,
    hist.control.mu, hist.control.sd, hist.treated.mu,
    hist.treated.sd, cola.induced.mu, cola.induced.sd,
    cola.control.mu, cola.control.sd, cola.treated.mu,
    cola.treated.sd)
}
\arguments{
  \item{outcome}{A string, either \code{hist} or
  \code{cola}}

  \item{hist.induced.mu}{Mean of the induced group,
  histology}

  \item{hist.induced.sd}{SD of the induced group,
  histology}

  \item{hist.control.mu}{Mean of the control group,
  histology}

  \item{hist.control.sd}{SD of the control group,
  histology}

  \item{hist.treated.mu}{Mean of the treated group,
  histology}

  \item{hist.treated.sd}{SD of the treated group,
  histology}

  \item{cola.induced.mu}{Mean of the induced group,
  collagen}

  \item{cola.induced.sd}{SD of the induced group, collagen}

  \item{cola.control.mu}{Mean of the control group,
  collagen}

  \item{cola.control.sd}{SD of the control group, collagen}

  \item{cola.treated.mu}{Mean of the treated group,
  collagen}

  \item{cola.treated.sd}{SD of the treated group, collagen}
}
\value{
  A data frame of 3 rows and 2 columns
}
\description{
  Returns the musd matrix from the inputs
}

