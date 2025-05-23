% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{apply_filter_to_bitmap}
\alias{apply_filter_to_bitmap}
\title{Apply pixel-wise filter to bitmap}
\usage{
apply_filter_to_bitmap(bitmap, filter_func, ...)
}
\arguments{
\item{bitmap}{A 3D raw array representing the image (dimensions: 3 x height x width).}

\item{filter_func}{Function to apply to each pixel (e.g., grey_filter, cutoff_filter).}

\item{...}{Additional arguments passed to the filter function (e.g., threshold).}
}
\value{
A filtered bitmap (3D raw array).
}
\description{
Applies a transformation function to each pixel in the bitmap image.
}
\examples{
grey_img <- apply_filter_to_bitmap(bitmap, grey_filter)
bw_img <- apply_filter_to_bitmap(bitmap, cutoff_filter, threshold = 127)

}
