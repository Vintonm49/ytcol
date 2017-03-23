#' Create a dataframe from JSON
#'
#' This function converts the JSON provided by the YouTube API from the list_channel_activities
#' function in the tuber package when the part parameter is set to "contentDetails".  The goal is to
#' capture the video IDs from the channel.
#'
#' @param l Dataframe$items from the list_channel_activities()
#' @return A dataframe of the items
#' @export
dataframeFromJSON <- function(l) {
  l1 <- lapply(l, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  keys <- unique(unlist(lapply(l1, names)))
  l2 <- lapply(l1, '[', keys)
  l3 <- lapply(l2, setNames, keys)
  res <- data.frame(do.call(rbind, l3))
  return(res)
}


#'Check if authenitication token is in options.
#'From the tuber package, original function not exported.
#'@export
yt_check_token <- function() {

  app_token = getOption('google_token')
  if (is.null(app_token)) stop("Please get a token using yt_oauth().\n")

}


#' Base GET function.
#' From the tuber package, but not exported.
#' #' @param path path to specific API request URL
#' @param query query list
#' @param \dots Additional arguments passed to \code{\link[httr]{GET}}.
#' @return list
#' @export
yt_GET <-
  function(path, query, ...) {

    ytcol::yt_check_token()

    req <- httr::GET("https://www.googleapis.com", path=paste0("youtube/v3/", path), query=query, httr::config(token = getOption("google_token")), ...)

    ytcol::yt_check(req)
    res <- httr::content(req)

    res
  }

#' Request Response Verification.
#' From the tuber package, but not exported.
#'
#' @param req Request
#' @return in case of failure, a message
#' @export
yt_check <-
  function(req) {

    if (req$status_code < 400) return(invisible())

    stop("HTTP failure: ", req$status_code, "\n", call. = FALSE)
  }
