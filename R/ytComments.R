#' Underlying Function to Get Comments on a Video on YouTube
#' Basic function, adapted from tuber package, get_comment_threads()
#' Used by other comment functions in the ytcol package
#'
#' @param filter string; Required.
#' named vector of length 1
#' potential names of the entry in the vector:
#' \code{video_id}: video ID.
#' \code{channel_id}: channel ID.
#' \code{thread_id}: comma-separated list of comment thread IDs
#' \code{threads_related_to_channel}: channel ID.
#'
#' @param part  Comment resource requested. Required. Comma separated list of one or more of the
#' following: \code{id, snippet}. e.g., \code{"id, snippet"}, \code{"id"}, etc. Default: \code{snippet}.
#' @param max_results  Maximum number of items that should be returned. Integer. Optional. Can be between 20 and 100. Default is 100.
#' @param page_token  Specific page in the result set that should be returned. Optional.
#' @param text_format Data Type: Character. Default is \code{"html"}. Only takes \code{"html"} or \code{"plainText"}. Optional.
#' @param simplify Data Type: Boolean. Default is \code{TRUE}. If \code{TRUE}, the function returns a data frame. Else a list with all the information returned.
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#' @return Nested named list.
#' @export
#'
yt.GetComments <- function (filter=NULL, part="snippet", text_format="html", simplify=TRUE, max_results=100, page_token = NULL, ...) {

  if (max_results < 20 | max_results > 100) stop("max_results only takes a value between 20 and 100.")
  if (text_format != "html" & text_format !="plainText") stop("Provide a legitimate value of textFormat.")

  if (!(names(filter) %in% c("video_id", "channel_id", "thread_id", "threads_related_to_channel"))) stop("filter can only take one of values: channel_id, video_id, parent_id, threads_related_to_channel.")
  if ( length(filter) != 1) stop("filter must be a vector of length 1.")

  translate_filter   <- c(video_id = 'videoId', thread_id ='id', threads_related_to_channel = 'allThreadsRelatedToChannelId', channel_id = 'channelId')
  yt_filter_name     <- as.vector(translate_filter[match(names(filter), names(translate_filter))])
  names(filter)      <- yt_filter_name

  querylist <- list(part=part, maxResults=max_results, textFormat=text_format, pageToken=page_token)
  querylist <- c(querylist, filter)

  res <- ytcol::yt_GET("commentThreads", querylist, ...)

  if (simplify==TRUE & part=="snippet") {
    simple_res  <- lapply(res$items, function(x) unlist(x$snippet$topLevelComment$snippet))
    simpler_res <- plyr::ldply(simple_res, rbind)
    return(simpler_res)
  }

  res

}

#' Get Comments on a Video on YouTube
#'
#'This function collects all the comments on a video on YouTube.
#' It calls the yt.GetComments function in the ytcol package.
#'
#' @param video_id String.  Video ID from YouTube.
#' @return Dataframe with variables including author information, comment text, date-time posted,
#' and date-time updated.
#' @export
#' @example
#' yt.VideoComments(video_id = "tgchTz8XjrI")
yt.VideoComments <- function(video_id = NULL){
  comment1 <- ytcol::yt.GetComments(filter=c(video_id = video_id))
  comment2 <- ytcol::yt.GetComments(filter=c(video_id = video_id), simplify=FALSE)
  com_token <- comment2$nextPageToken

  repeat{
    comment1_sub <- ytcol::yt.GetComments(filter=c(video_id = video_id), page_token = com_token)
    comment2_sub <- ytcol::yt.GetComments(filter=c(video_id = video_id), page_token = com_token,
                                          simplify=FALSE)
    comment1 <- gtools::smartbind(comment1, comment1_sub)
    com_token <- comment2_sub$nextPageToken
    if(is.null(com_token)){
      break
    }
  }
  comment1$pullDate <- Sys.time()
  date <- format(Sys.time(),"%Y%m%d_%H%M")
  write.csv(comment1, file=paste("./yt_collection/","comments_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)

  return(comment1)

}
