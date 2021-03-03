#' Creates json data
#' @param sentence Character scalar to be analyzed.
#' @param mode One of `normal`, `search` or `extended`.
#' @param dict One of `ipa` (the IPA-dic) or `uni` (the Uni-dic).
#' @return returns a json string.
#' @export
serialize <- function(sentence, mode = c("normal"), dict = c("ipa", "uni")) {
  params <- list(
    sentence = paste0(stringi::stri_enc_toutf8(sentence), collapse = ""),
    mode = match.arg(mode),
    dict = match.arg(dict)
  )
  return(jsonlite::toJSON(params, simplifyVector = TRUE, auto_unbox = TRUE))
}


#' Creates an asynchronous request
#' @param url URL of Kagome server API.
#' @return returns a asynchronous function.
#' @export
queue <- function(params, url = "http://localhost:6060/tokenize") {
  request <- async::async(function() {
    async::http_post(
      url,
      data = params,
      headers = c("content-type" = "application/json")
    )$then(async::http_stop_for_status)$then(function(x) {
      x$content %>%
        readr::read_lines() %>%
        jsonlite::fromJSON(simplifyVector = TRUE)
    })
  })
  return(request)
}


#' Kicks a request
#' @param request An asynchronous function to be kicked.
#' @param keep Column names to keep in the results.
#' @param .skip_enc_reset Logical. If true, leaves encoding of results as the environment native.
#' @return data.frame.
#' @export
kick <- function(request,
                 keep = c("surface", "class", "base_form", "reading"),
                 .skip_enc_reset = FALSE) {
  response <- async::synchronise(request())
  response$token <- dplyr::mutate(
    response$token,
    dplyr::across(where(is.list),
                  ~ paste0(purrr::flatten(.), collapse = ",")))
  result <- dplyr::select(response$token, {{ keep }})
  if (.Platform$pkgType != "win.binary" && !.skip_enc_reset) {
    return(result)
  } else {
    return(dplyr::mutate(result, dplyr::across(where(is.character), ~ reset_encoding(.))))
  }
}
