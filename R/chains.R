#' Create json data
#' @param sentences Character to be analyzed.
#' @param mode One of `normal`, `search` or `extended`.
#' @param dict One of `ipa` (the IPA-dic) or `uni` (the Uni-dic).
#' @return a list of json strings.
#' @export
serialize <- function(sentences,
                      mode = c("normal", "search", "extended"),
                      dict = c("ipa", "uni")) {
  mode <- rlang::arg_match(mode)
  dict <- rlang::arg_match(dict)
  params <- lapply(sentences, function(sentence) {
    list(
      sentence = stringi::stri_enc_toutf8(sentence),
      mode = mode,
      dict = dict
    )
  })
  return(lapply(params, function(param) {
    jsonlite::toJSON(param, simplifyVector = TRUE, auto_unbox = TRUE)}
  ))
}


#' Create asynchronous requests
#' @param params List of json string.
#' @param url An URL of Kagome server.
#' @return a list of json strings and asynchronous function.
#' @export
queue <- function(params, url = "http://localhost:6060/tokenize") {
  promise <- async::async(function(param) {
    async::http_post(
      url,
      data = param,
      headers = c("content-type" = "application/json")
    )$then(async::http_stop_for_status)$then(function(x) {
      x$content %>%
        readr::read_lines() %>%
        jsonlite::fromJSON(simplifyVector = TRUE)
    })
  })
  return(list(params = params, func = promise))
}


#' Kick requests
#' @param requests List that comes out from \code{queue}.
#' @param keep Column names to keep in the results.
#' @param .skip_enc_reset Logical. If true, leaves encoding of results as environment native.
#' @return data.frame.
#' @export
kick <- function(requests,
                 keep = c("surface", "class", "base_form", "reading"),
                 .skip_enc_reset = FALSE) {
  response <- async::synchronise(async::async_map(requests$params, requests$fun))
  purrr::imap_dfr(response, function(elem, index) {
    elem$token <- dplyr::mutate(
      elem$token,
      dplyr::across(
        where(is.list),
        ~ paste0(purrr::flatten(.), collapse = ",")
      )
    )
    result <- data.frame(
      sentence_id = index,
      dplyr::select(elem$token, {{ keep }})
    )
    if (.Platform$pkgType != "win.binary" || .skip_enc_reset) {
      return(result)
    } else {
      return(dplyr::mutate(result, dplyr::across(where(is.character), ~ reset_encoding(.))))
    }
  })
}
