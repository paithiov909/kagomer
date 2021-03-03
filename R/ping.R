#' Sends a HEAD request to Kagome server
#' @param url URL stirng
#' @return returns the status code of request invisibly.
ping <- function(url = "http://localhost:6060") {
  request <- async::async(function() {
    async::http_head(url)$then(function(x) x$status_code)
  })
  return(invisible(async::synchronise(request())))
}
