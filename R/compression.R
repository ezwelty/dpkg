# TODO: Add support for compressed file read and write (e.g. "data/data.csv.gz")

#' Compress file
#'
#' @param file (character) Path to file.
#' @param ext (character) Compression file extension.
#' @param remove (logical) Whether to remove the original file.
compress_file <- function(file, ext = c("gz", "bz2"), remove = FALSE) {
  ext <- match.arg(ext)
  outfile <- paste0(file, ".", ext)
  switch(
    ext,
    gz = R.utils::gzip(filename = file, remove = remove, overwrite = TRUE),
    bz2 = R.utils::bzip2(filename = file, remove = remove, overwrite = TRUE)
  )
  outfile
}

#' Decompress file
#'
#' @param file (character) Path to compressed file.
#' @param remove (logical) Whether to remove the original file.
decompress_file <- function(file, remove = FALSE) {
  ext <- tools::file_ext(file)
  switch(
    ext,
    gz = R.utils::gunzip(filename = file, remove = remove, overwrite = TRUE),
    bz2 = R.utils::bunzip2(filename = file, remove = remove, overwrite = TRUE)
  )
}

#' Test if path is for a compressed file
#'
#' @param file (character) Path to file.
is_compressed <- function(file) {
  grepl("\\.(gz|bz2)$", file)
}
