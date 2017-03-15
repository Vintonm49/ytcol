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
  return(vidInfo)
}
