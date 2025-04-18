% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cxappcache.R
\name{.cxappcache}
\alias{.cxappcache}
\title{Utility function representing a global application cache object}
\usage{
.cxappcache(x)
}
\arguments{
\item{x}{Application cache object}
}
\value{
Application cache object
}
\description{
Caching is used extensively throughout apps using the cxapp package and
importing cache configurations from property files every time the cache
is required will include unnecessary processing for a very static reference.

The function will return the \emph{cached} \link[cxapp]{cxapp_applicationcache}
object from the global environment.

The configured application cache object is \code{.cxapp.wrkcache.appcache} in the
\link[base]{.GlobalEnv} environment.

If an application cache object is specified as input \code{x}, the global
environment cache object is updated before it is returned.
}
