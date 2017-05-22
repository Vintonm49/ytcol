#' Search YouTube for a key term with a date range
#'
#' This function makes use of the yt.GetSearch function in the ytcol package
#' and pulls videos for the set term within the set date range.  The number of videos
#' returned is set with the max_results parameter.
#'
#' @param term  Character. Search term. Required.
#' @param published_before Date.  Optional. RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @param published_after  Date.  Optional. RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @param max_results  Integer.  Maximum number of results returned.  Can be between 1 and 50. Default is 50.
#' @return A dataframe with 6 variables: \code{video_ID, dateTime, channel_ID, title, description, channel_title}
#' @export

yt.Search <- function(term=NULL, published_before=NULL, published_after=NULL, max_results = 50){

  search_vids <- ytcol::yt.GetSearch(term = term, published_before = published_before,
                                     published_after = published_after, max_results = max_results)
  #date <- format(Sys.time(),"%Y%m%d_%H%M")
  #write.csv(search_vids, file=paste("./yt_collection/","search_",term,"_!_",date,".csv", sep = ""), row.names = FALSE)
  return(search_vids)
}


#' Underlying GET Function for Search
#'
#' This function is used by yt.Search in the ytcol package to call the API to get
#' videos related to a search term.
#' @param term  Character.  Search term. Required.  No default.
#' @param max_results  Integer. Maximum number of items returned. Can be between 1 and 50. Default is 50.
#' @param published_after  Date. Optional. RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @param published_before  Date. Optional. RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @param type  Character. Optional. Takes one of three values: \code{'video', 'channel', 'playlist'}. Default is \code{'video'}.
#' @export
yt.GetSearch <- function(term = term, max_results = max_results, published_after = NULL, published_before = NULL,
                              type = "video", ...){
  if (!is.character(term)) stop("Must specify a search term.\n")

  if (max_results < 0 | max_results > 50) {
    stop("max_results only takes a value between 0 and 50.")
  }

  if (is.character(published_after)) {
    if (is.na(as.POSIXct(published_after,  format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("The date is not properly formatted in RFC 339 Format.")
    }
  }

  if (is.character(published_before)) {
    if (is.na(as.POSIXct(published_before, format = "%Y-%m-%dT%H:%M:%SZ"))) {
      stop("The date is not properly formatted in RFC 339 Format.")
    }
  }
  #for queries with spaces
  format_term <- paste0(unlist(strsplit(term, " ")), collapse = "%20")

  querylist <- list(part = "snippet", q = format_term, maxResults = max_results, publishedAfter = published_after,
                    publishedBefore = published_before, type = type)
  # eliminated NULLs from querylist
  querylist <- querylist[names(querylist)[sapply(querylist, function (x) !is.null(x))]]

  res <- ytcol::yt_GET("search", querylist, ...)

  if(res$pageInfo$totalResults != 0) {
    res <- ytcol::dataframeFromJSON(res$items)
    res <- res[, c("id.videoId","snippet.publishedAt","snippet.channelId", "snippet.title", "snippet.description",
                   "snippet.channelTitle")]
    names(res) <- c("video_ID","dateTime","channel_ID","title","description","channel_title")
    return(res)
  } else {
    return(data.frame())
  }
}




