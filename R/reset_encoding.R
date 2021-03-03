#' @noRd
#' @keywords internal
reset_encoding <- function(chr, encoding = "UTF-8") {
  Encoding(chr) <- encoding
  return(chr)
}
