% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vegie.R
\name{buildDB}
\alias{buildDB}
\title{Build database from legacy format.}
\usage{
buildDB(dsn, layer, dbfile, fidoffset = 0, slurp = FALSE, verbose = TRUE)
}
\arguments{
\item{dsn}{data source name}

\item{layer}{layer name}

\item{dbfile}{SQLite file name}

\item{slurp}{if TRUE read all and write all in one go}

\item{verbose}{if TRUE report on progress while building DB}
}
\description{
Build database from legacy format.
}

