#' Create a dataframe from JSON
#'
#' This function converts the JSON provided by the YouTube API to a dataframe.  Utilized
#' by multiple other functions in the ytcol package.
#'
#' @param l Dataframe$items
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


#'Check for Authenitication Token in Options.
#'
#'Supports the yt_GET function in the ytcol package.  From the tuber package, original function not exported.
#'@export
yt_check_token <- function() {

  app_token = getOption('google_token')
  if (is.null(app_token)) stop("Please get a token using yt_oauth().\n")

}


#' Base GET function.
#'
#' From the tuber package, but not exported.
#' @param path path to specific API request URL
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
#'
#'
#' Supports the yt_GET function in the ytcol package.  From the tuber package, but not exported.
#'
#' @param req Request
#' @return In case of failure, a message.
#' @export
yt_check <-
  function(req) {

    if (req$status_code < 400) return(invisible())

    stop("HTTP failure: ", req$status_code, "\n", call. = FALSE)
  }


#' Get YouTube Channel ID from Vanity or Legacy Name
#'
#' Some YouTube channels have vanity names (sometimes referred to as legacy names)
#' in the channel URL instead of channel IDs.  The channel ID is needed to collect
#' any data from the channel, the API will not accept the vanity name.  This function
#' provides the channel ID for a channel, given the vanity name.
#'
#' @param term  Character. Vanity name. Required.
#' @export
yt.GetChannelID <- function(term = term,  ...){
  if (!is.character(term)) stop("Must specify a search term.\n")

  #for queries with spaces
  format_term <- paste0(unlist(strsplit(term, " ")), collapse = "%20")

  querylist <- list(part = "snippet", q = format_term, maxResults = 1, type = "channel")
  # eliminated NULLs from querylist
  querylist <- querylist[names(querylist)[sapply(querylist, function (x) !is.null(x))]]

  res <- ytcol::yt_GET("search", querylist, ...)

  if(res$pageInfo$totalResults != 0) {
    res <- ytcol::dataframeFromJSON(res$items)
    res <- as.character(res[1,6])
    return(res)
  } else {
    return(data.frame())
  }
}

#' Paste Columns in a Dataframe and Suppress NAs
#'
#' The function suppresses NAs when pasting columns together
#' in a data frame.
#'
#' @param ...  one or more R objects to be converted to character vectors.
#' @param sep  String  A character string to separate the terms
#' @param collapse String  An optional character string to separate the results.
#' See the base paste function help for details.  Default is NULL
#' @param na.rm  Must set to TRUE to suppress NAs
#' @export
#'
pasteNA <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}


#' Authentication to YouTube
#' 
#' The function looks for \code{.httr-oauth} in the working directory. If it doesn't find it, it expects an application ID and a secret.
#'@param app_id client ID.  Required.
#'@param app_secret  client secret. Required.
#'@param scope  Character.  Default = "ssl".  Do not change.
#'@param token  Path to file containing token.
#'@param \dots Additional arguments
#'@export 
yt.oauth <- function (app_id = NULL, app_secret = NULL, scope = "ssl", token = ".httr-oauth", ...)  {
  
  if(file.exists(token)){
    google_token <- try(suppressWarnings(readRDS(token)), silent = TRUE)
    if(inherits(google_token, "try-error")){
      stop(sprintf("Unable to read token from:%s", token))
    }
    google_token <- google_token[[1]]
  }else if (is.null(app_id) | is.null(app_secret)) {
    stop("Please provide values for app_id and app_secret")
  } else {
    myapp <- httr::oauth_app("google", key = app_id, secret = app_secret)
    google_token <- oauth2.0_token(oauth_endpoints("google"), myapp, 
                                   scope = "https://www.googleapis.com/auth/youtube.force-ssl", ...) 
    
  }
  options(google_token=google_token)
}
  
