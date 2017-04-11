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

yt.singleVideoInfo <- function(video_id=NULL){
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
  date <- format(Sys.time(),"%Y%m%d_%H%M")
  write.csv(df, file=paste("./yt_collection/","videoInfo_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)

  return(vidInfo)
}


#' Get Related Videos
#'
#' Given a video ID, get up to 50 videos that are related.
#'
#' @param video_id  String.  Video ID from YouTube.
#' @return Dataframe with the following variables: videoID, dateTime, channelID, title, description,
#' channelTitle, and liveBroadcastContent
#' @export
yt.related <- function(video_id){
  df <- tuber::get_related_videos(video_id = video_id)
  df <- df[,-c(6:14)]
  names(df)[2] <- "dateTime"
  date <- format(Sys.time(),"%Y%m%d_%H%M")
  write.csv(df, file=paste("./yt_collection/","related_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)

  return(df)

}













