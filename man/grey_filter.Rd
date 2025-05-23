% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{grey_filter}
\alias{grey_filter}
\title{Convert RGB pixel to greyscale}
\usage{
grey_filter(pixel)
}
\arguments{
\item{pixel}{A raw vector of length 3 (representing R, G, B).}
}
\value{
A raw vector of 3 identical greyscale values.
}
\description{
Applies luminance weights to RGB values to generate a greyscale version.
}
\examples{
this function is the argument of the apply_filter_to_bitmap

}
