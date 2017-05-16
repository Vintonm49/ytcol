#' Get statistics for a video on YouTube
#' This function takes a YouTube video ID and returns in a dataframe
#' statistics like views, likes, and dislikes.
#' @param l video_id  String.
#' @return A dataframe.
#' @export
getVideoStatsDF <- function(video_id){
  stats <- as.data.frame(get_stats(video_id))
  stats$pullDate <- Sys.time()
  return(stats)
}

#' Get details for a video on YouTube
#' This function takes a YouTube video ID and returns details in a dataframe
#' like date published, channel ID, title, description, channel title, category ID, and tags.
#' @param l video_id  String.
#' @return A dataframe.
#' @export
getVideoDetailsDF <- function(video_id){
  details <- get_video_details(video_id)
  return(data.frame(t(unlist(details))))
}

#' Get Statistics and Details for a Single Video on YouTube
#'
#' This function combines the stats and details functions
#' and returns a dataframe with both sets of information for
#' a single video.  Requires the video ID.
#'
#' @param video_id  String.  Video ID from YouTube.
#' @return Dataframe with the following variables: videoID, viewCount, like Count, dislikeCount,
#' favoriteCount, commentCount, pullDate, dateTime, channelID, title, description, tags,
#' liveBroadcastContent, localized.title, localized.description
#' @export
#'

yt.SingleVideoInfo <- function(video_id=NULL){
  vidStat <- ytcol::getVideoStatsDF(video_id)
  vidDetail <- ytcol::getVideoDetailsDF(video_id)
  vidDetail <- vidDetail[,-c(6:17)]
  vidDetail <- vidDetail[,!names(vidDetail) %in% c("thumbnails.maxres.url",
                                                   "thumbnails.maxres.width",
                                                   "thumbnails.maxres.height")]
  vidInfo <- cbind(vidStat,vidDetail)
  vidInfo <- vidInfo[,-c(8)]
  names(vidInfo)[1]<- c("videoID")
  names(vidInfo)[8]<- c("dateTime")
  #date <- format(Sys.time(),"%Y%m%d_%H%M")
  #write.csv(vidInfo, file=paste("./yt_collection/","videoInfo_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)

  return(vidInfo)
}


#' Get Related Videos
#'
#' Given a video ID, get videos that are related.
#'
#' @param video_id  String.  Video ID from YouTube.
#' @return Dataframe with the following variables: video_ID, dateTime, channel_ID, title, description,
#' channel_title, and related_to
#' @export
yt.Related <- function(video_id){
  df <- ytcol::yt.GetRelated(video_id = video_id)
  dff <- dataframeFromJSON(df$items)
  rel_token <- df$nextPageToken
  if(is.null(rel_token)){  #less than 50 related videos
    dff <- dff[,-c(1,2,3,9:17,19)]
    names(dff) <- c("video_ID","dateTime","channel_ID","title","description","channel_title")
    dff$related_to <- video_id
    #date <- format(Sys.time(),"%Y%m%d_%H%M")
    #write.csv(dff, file=paste("./yt_collection/","related_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
    return(dff)
  } else {  #more then 50 related videos
    repeat{
      df2 <- ytcol::yt.GetRelated(video_id = video_id, page_token = rel_token)
      df22 <- dataframeFromJSON(df2$items)
      if(nrow(df22)==0){
        break
      }
      dff <- gtools::smartbind(dff, df22)
      rel_token <- df2$nextPageToken
      if(is.null(rel_token)){
        break
      }
    }
    dff <- dff[,-c(1,2,3,9:17,19)]
    names(dff) <- c("video_ID","dateTime","channel_ID","title","description","channel_title")
    dff$related_to <- video_id
    #date <- format(Sys.time(),"%Y%m%d_%H%M")
    #write.csv(dff, file=paste("./yt_collection/","related_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
    return(dff)
  }
}

#' Get Function for Collecting Related Videos from a YouTube Video
#'
#' Basic function, adapted from tuber package, get_related_videos()
#' Used by other related videos functions in the ytcol package.  Getting related videos
#' is considered a search to the YouTube API.
#'
#' @param video_id string; Required.  Video ID from YouTube.
#' @param max_results  Maximum number of items that should be returned. Integer. Optional. Can be between 1 and 50. Default is 50.
#' @param page_token  Specific page in the result set that should be returned. Optional.
#' @param region_code  string.  Return search results for a specified country.  ISO 3166-1 alpha-2 country code.  Optional.
#' @param safe_search  Character. Optional. Takes one of three values: \code{'moderate'}, \code{'none'} (default) or \code{'strict'}
#' If none, search result not filtered.  If moderate, search result filtered for content restricted within your locale.
#' If strict, search result filtered to exclude all restricted content from the search result set.
#' @param \dots Additional arguments passed to \code{\link{tuber_GET}}.
#' @return Nested named list.
#' @export
yt.GetRelated <- function (video_id = NULL, max_results = 50, page_token = NULL, region_code = NULL, safe_search = "none", ...){
  if (!is.character(video_id)) stop("Must specify a video ID.")
  if (max_results < 0 | max_results > 50) stop("max_results only takes a value between 0 and 50.")

  querylist <- list(part="snippet", relatedToVideoId = video_id, type="video",
                    maxResults=max_results, safeSearch = safe_search,
                    pageToken = page_token, regionCode = region_code)

  res <- ytcol::yt_GET("search", querylist, ...)
  res
}


#' Get the Comments from Related Videos
#'
#' Given a video ID, get the comments from that video and videos that are related.
#'
#' @param video_id  String.  Video ID from YouTube.
#' @return Dataframe with the following variables describing comments.
#' @export
yt.RelatedVideoComments <- function(video_id){
  rel_vids <- ytcol::yt.Related(video_id = video_id)
  list_of_video_ids <- as.character(rel_vids$video_ID)
  list_of_video_ids <- c(list_of_video_ids,video_id)
  comdf<-data.frame()
  for (i in list_of_video_ids) {
    comm <- try(ytcol::yt.SimpleVideoComments(i))
    comdf <- gtools::smartbind(comdf,comm)
  }
  comdf$related_to <- video_id
  comdf <- comdf[,c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                    "text_original","dateTime", "updated_dateTime", "reply_count", "parent_comment_ID",
                    "parent_author_display_name","parent_author_channel_ID","pullDate","related_to")]
  comdf <- comdf[!is.na(comdf$comment_ID),]
  comdf <- dplyr::distinct(comdf, comment_ID, .keep_all = TRUE)
  #date <- format(Sys.time(),"%Y%m%d_%H%M")
  #write.csv(comdf, file=paste("./yt_collection/","related_video_comments_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
  print(paste0("Number of related videos: ",nrow(rel_vids)))
  return(comdf)
}







