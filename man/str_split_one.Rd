% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_split_one.R
\name{str_split_one}
\alias{str_split_one}
\title{str_split_one}
\usage{
str_split_one(string, pattern, n = Inf)
}
\arguments{
\item{string}{A character vector with one element.}

\item{pattern}{What to split on.}

\item{n}{i dont know what is it}
}
\value{
A character vector.
}
\description{
str_split_one
}
\examples{
x <- "alfa,bravo,charlie,delta"
str_split_one(x, pattern = ",")
}
