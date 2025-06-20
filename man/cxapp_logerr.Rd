% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cxapp_logerr.R
\name{cxapp_logerr}
\alias{cxapp_logerr}
\title{Simple function to log error messages}
\usage{
cxapp_logerr(x, echo = base::interactive())
}
\arguments{
\item{x}{bject or vector of messages to log}

\item{echo}{Enable/Disable output of log messages to console}
}
\value{
Invisible vector of log messages
}
\description{
The \code{cxxapp_logerr} function logs messages in specified order prefixed with the current
date and time.

If \code{x} is not a vector, the object is expected to inherit \code{try-error}
(see \link[base]{try}).

The log file parent directory path is defined by the \code{LOG.PATH} cxapp property. If
\code{LOG.PATH} is not defined log messages are written to the console.

The \code{LOG.NAME} option specifies the basis for the log and error log file names.
Any log file name parts associated with log rotation is appended to the log file base name
separated by an underscore (\verb{_}). Error log file names end in the file extension \code{err}.

If \code{LOG.NAME} is not defined, the log file name is \code{app.err}.

The \code{LOG.ROTATION} cxapp property defines the log file rotation. Valid rotations
are \code{YEAR}, \code{MONTH} and \code{DAY}. The log rotation follows the format four digit
year and two digit month and day.

Log messages follow the convention \verb{[\%Y-\%m-\%d \%H:\%M:\%S] <message>  [record]}
where \code{record} is a pseudo-random string that identifies lines that are submitted
in the same call to \code{cxapp_logerr()}.

if executing in \code{interactive()} mode, error log messages are written to the console.
}
