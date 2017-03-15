#' Create a dataframe from JSON
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

#' Get statistics for a video on YouTube
#' This function takes a YouTube video ID and returns in a dataframe
#' statistics like views, likes, and dislikes.
#' @param l video_id  String.
#' @return A dataframe.
#' @export
#' @example getVideoStatsDF(video_id)
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
#' @examples
#' getVideoDetailsDF(video_id)
getVideoDetailsDF <- function(video_id){
  details <- get_video_details(video_id)
  return(data.frame(t(unlist(details))))
}

#' Get list of videos from a YouTube Channel
#'
#' This function gets a list of videos from a YouTube channel.  It requires the channel ID
#' and allows the user to set a date range, including before a certain date, after
#' a certain date, and between two dates.  In the output, the dateTime is when the video was posted
#' and the pullDate is when the function was executed.
#'
#' @param channel_id  String.  The YouTube channel ID.  Cannot be the vanity URL name.
#' @param published_before Date.  RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @param published_after  Date.  RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @return A dataframe with multiple variables:  videoID, dateTime, channelID, title, description, channelTitle, pullDate,
#' viewCount, likeCount, dislikeCount, favoriteCount, commentCount, tags, categoryID, liveBroadcastContent,
#' defaultLanguage, localized.title, localized.description, defaultAudioLanguage
#' @export
#' @examples
#' yt.AllChannelVideos(channel_id, published_before, published_after)
yt.AllChannelVideos <- function(channel_id=NULL, published_before=NULL, published_after=NULL){

  channelAct <- tuber::list_channel_activities(filter=c(channel_id = channel_id) ,part = "contentDetails",
                                        published_before = published_before,
                                        published_after = published_after)
  channelList <- tuber::list_channel_activities(filter=c(channel_id = channel_id) ,
                                         published_before = published_before,
                                         published_after = published_after)
  df <- ytcol::dataframeFromJSON(channelAct$items)

  token <- channelAct$nextPageToken

  repeat{
    channelActSub <- tuber::list_channel_activities(filter=c(channel_id = channel_id), part = "contentDetails",
                                             published_before = published_before,
                                             published_after = published_after,
                                             page_token = token)
    channelListSub <- tuber::list_channel_activities(filter=c(channel_id = channel_id),
                                              published_before = published_before,
                                              published_after = published_after,
                                              page_token = token)
    dff <- ytcol::dataframeFromJSON(channelActSub$items)
    df <- gtools::smartbind(df, dff)
    channelList <- gtools::smartbind(channelList,channelListSub)

    print(channelActSub$nextPageToken)
    token <- channelActSub$nextPageToken
    if(is.null(token)){
      break
    }

  }
  df<-cbind(df,channelList)
  df<-df[,c(4,5,6,7,8,21)]
  names(df)[1:2] <- c("videoID", "dateTime")
  df$pullDate <- Sys.time()

  list_of_video_ids <- as.character(df$videoID)
  allVideoStats <- plyr::ldply(list_of_video_ids, ytcol::getVideoStatsDF)
  allVideoDetails <- plyr::ldply(list_of_video_ids, .fun = ytcol::getVideoDetailsDF)
  allVideoDetails <- allVideoDetails[,-c(6:17)]
  allVideoDetails <- allVideoDetails[,!names(allVideoDetails) %in% c("thumbnails.maxres.url",
                                                                     "thumbnails.maxres.width",
                                                                     "thumbnails.maxres.height")]


  allVideoInfo <- cbind(allVideoStats,allVideoDetails)
  df <- cbind(df, allVideoInfo)
  df <- df[,-c(8,14:19)]
  return(df)
}




