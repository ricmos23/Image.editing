#' Read image and convert to bitmap
#'
#' Loads an image using a file picker, displays it (optionally), and extracts its bitmap data.
#'
#' @param show Logical; whether to display the image after reading. Default is TRUE.
#'
#' @return A 3D array (bitmap) of raw RGB values.
#'
#' @examples
#' bitmap <- read_image_bitmap(show = TRUE)
#'
#' @export
read_image_bitmap <- function(show = TRUE) {
  path <- file.choose()
  img <- magick::image_read(path)
  if (show) print(img)
  bitmap <- img[[1]]
  return(bitmap)
}

#' Convert RGB pixel to greyscale
#'
#' Applies luminance weights to RGB values to generate a greyscale version.
#'
#' @param pixel A raw vector of length 3 (representing R, G, B).
#'
#' @return A raw vector of 3 identical greyscale values.
#'
#' @examples
#' this function is the argument of the apply_filter_to_bitmap
#'
#' @export
grey_filter <- function(pixel) {
  r <- as.integer(pixel[1])
  g <- as.integer(pixel[2])
  b <- as.integer(pixel[3])
  grey_val <- round(r * 0.299 + g * 0.587 + b * 0.114)
  return(as.raw(rep(grey_val, 3)))
}

#' Apply cutoff filter to RGB pixel
#'
#' Converts a pixel to black or magenta depending on its greyscale value and a threshold.
#'
#' @param pixel A raw vector of length 3 (RGB).
#' @param threshold Integer threshold for greyscale cutoff (default is 200).
#'
#' @return A raw RGB vector representing either black or magenta.
#'
#' @examples
#' this function is the argument of the apply_filter_to_bitmap
#'
#' @export
cutoff_filter <- function(pixel, threshold = 127) {
  r <- as.integer(pixel[1])
  g <- as.integer(pixel[2])
  b <- as.integer(pixel[3])

  grey_val <- round(r * 0.299 + g * 0.587 + b * 0.114)

  if (grey_val > threshold) {
    return(as.raw(c(0, 0, 0)))  #black
  } else {
    return(as.raw(c(255, 0, 255)))        # magenta
  }
}


#' Apply pixel-wise filter to bitmap
#'
#' Applies a transformation function to each pixel in the bitmap image.
#'
#' @param bitmap A 3D raw array representing the image (dimensions: 3 x height x width).
#' @param filter_func Function to apply to each pixel (e.g., grey_filter, cutoff_filter).
#' @param ... Additional arguments passed to the filter function (e.g., threshold).
#'
#' @return A filtered bitmap (3D raw array).
#'
#' @examples
#' grey_img <- apply_filter_to_bitmap(bitmap, grey_filter)
#' bw_img <- apply_filter_to_bitmap(bitmap, cutoff_filter, threshold = 127)
#'
#' @export
apply_filter_to_bitmap <- function(bitmap, filter_func, ...) {
  height <- dim(bitmap)[2]
  width <- dim(bitmap)[3]
  new_bitmap <- array(as.raw(0), dim = c(3, height, width))

  for (i in 1:height) {
    for (j in 1:width) {
      pixel <- bitmap[, i, j]
      new_pixel <- filter_func(pixel, ...)
      new_bitmap[, i, j] <- new_pixel
    }
  }

  return(new_bitmap)
}

#' Display bitmap image using magick
#'
#' Displays a filtered image by converting the bitmap to a viewable raster format.
#'
#' @param filtered_image A 3D raw array of filtered pixel values.
#'
#' @return Displays the image using magick and returns nothing.
#'
#' @examples
#' display_filtered_image(grey_img)
#'
#' @export
display_filtered_image <- function(filtered_image) {
  transposed <- aperm(filtered_image, c(1, 3, 2))
  numeric_img <- array(as.numeric(transposed) / 255, dim = dim(transposed))
  raster_img <- as.raster(aperm(numeric_img, c(2, 3, 1)))
  magick_img <- magick::image_read(raster_img)
  print(magick_img)
}
