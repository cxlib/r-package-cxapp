% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cxapp_appnode.R
\name{cxapp_appnode}
\alias{cxapp_appnode}
\title{Pseudo-random string to identify an application node by unique ID}
\usage{
cxapp_appnode()
}
\value{
Vector of identifying attributes
}
\description{
The intent is to generate a sufficiently unique string such that the likelihood
that any two nodes active at the same time will not have the same identify.
}
