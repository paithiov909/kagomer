% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chains.R
\name{queue}
\alias{queue}
\title{Create asynchronous requests}
\usage{
queue(params, url = "http://localhost:6060/tokenize")
}
\arguments{
\item{params}{List of json string.}

\item{url}{An URL of Kagome server.}
}
\value{
a list of json strings and asynchronous function.
}
\description{
Create asynchronous requests
}
