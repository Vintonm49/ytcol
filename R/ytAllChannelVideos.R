


#' Get list of videos from a YouTube Channel
#'
#' This function gets a list of videos from a YouTube channel.  It requires the channel ID
#' and allows the user to set a date range, including before a certain date, after
#' a certain date, and between two dates.  In the output, the dateTime is when the video was posted
#' and the pullDate is when the function was executed.
#'
#' @param channel_id  Character.  The YouTube channel ID.  Cannot be the vanity URL name. Required.
#' @param published_before Date.  RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @param published_after  Date.  RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @return A dataframe with multiple variables: \code{videoID, dateTime, channelID, title, description, channelTitle, pullDate,
#' viewCount, likeCount, dislikeCount, favoriteCount, commentCount, tags, categoryID, liveBroadcastContent,
#' defaultLanguage, localized.title, localized.description, defaultAudioLanguage}
#' @export

yt.AllChannelVideos <- function(channel_id=NULL, published_before=NULL, published_after=NULL){

  channelAct <- tuber::list_channel_activities(filter=c(channel_id = channel_id) ,part = "contentDetails",
                                               published_before = published_before,
                                               published_after = published_after)
  channelList <- tuber::list_channel_activities(filter=c(channel_id = channel_id) ,
                                                published_before = published_before,
                                                published_after = published_after)
  df <- ytcol::dataframeFromJSON(channelAct$items)
  if(ncol(df) > 4){
    x <- grep(pattern = "videoid", ignore.case = T, x = names(df))
    x3<- df[,x, drop = FALSE]
    ncol(x3)
    x3$videoID <- "NA"
    for (j in 1:nrow(x3)){
      for (i in 1:ncol(x3)){
        if (is.na(x3[j,i])=="FALSE"){
          x3$videoID[j] <- as.character(x3[j,i])
        }
      }
    }
    df <- x3[,"videoID", drop = FALSE]

  } else {
    df <- df[,4, drop = FALSE]
    colnames(df)[which(colnames(df)=='contentDetails.upload.videoId')] <- "videoID"
  }

  token <- channelAct$nextPageToken

  if(nrow(channelList) < 50){
    df<-cbind(df,channelList)
    colnames(df)[which(colnames(df)=='contentDetails.upload.videoId')] <- "videoID"
    df<-df[,c("videoID","publishedAt","channelId","title","description","channelTitle")]
    df <- dplyr::distinct(df, videoID, .keep_all = TRUE)
    df <- na.omit(df)
    names(df) <- c("video_ID", "video_dateTime","channel_ID","video_title","video_description","channel_title")
    df$pullDate <- Sys.time()
    list_of_video_ids <- as.character(df$video_ID)
    allVideoStats <- plyr::ldply(list_of_video_ids, ytcol::getVideoStatsDF)
    allVideoDetails <- plyr::ldply(list_of_video_ids, .fun = ytcol::getVideoDetailsDF)
    allVideoStats <- allVideoStats[,!names(allVideoStats) %in% c("pullDate")]

    allVideoInfo <- cbind(allVideoStats,allVideoDetails)
    df <- cbind(df, allVideoInfo)
    cols.dont.want <- c("channelId","channelTitle","liveBroadcastContent","localized.title","localized.description","thumbnails.standard.url",
                        "thumbnails.standard.width","thumbnails.standard.height","publishedAt","title","description",
                        "id","thumbnails.maxres.url","thumbnails.maxres.width","thumbnails.maxres.height",
                        "thumbnails.default.url","thumbnails.default.width","thumbnails.default.height",
                        "thumbnails.medium.url","thumbnails.medium.width","thumbnails.medium.height",
                        "thumbnails.high.url","thumbnails.high.width","thumbnails.high.height")
    df <- df[,! names(df) %in% cols.dont.want, drop=F]
    #date <- format(Sys.time(),"%Y%m%d_%H%M")
    #write.csv(df, file=paste("./yt_collection/","channel_",channel_id,"_!_",date,".csv", sep = ""), row.names = FALSE)

    return(df)
    break
  }

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
    if(ncol(dff) > 4){
      x <- grep(pattern = "videoid", ignore.case = T, x = names(dff))
      x3<- dff[,x, drop = FALSE]
      ncol(x3)
      x3$videoID <- "NA"
      for (j in 1:nrow(x3)){
        for (i in 1:ncol(x3)){
          if (is.na(x3[j,i])=="FALSE"){
            x3$videoID[j] <- as.character(x3[j,i])
          }
        }
      }
      dff <- x3[,"videoID", drop = FALSE]
      #dff <- unique(dff)

    } else {
      dff <- dff[,4, drop = FALSE]
      colnames(df)[which(colnames(df)=='contentDetails.upload.videoId')] <- "videoID"
    }
    df <- gtools::smartbind(df, dff)
    channelList <- gtools::smartbind(channelList,channelListSub)

    token <- channelActSub$nextPageToken
    if(is.null(token)){
      break
    }

  }
  df<-cbind(df,channelList)
  colnames(df)[which(colnames(df)=='contentDetails.upload.videoId')] <- "videoID"
  df<-df[,c("videoID","publishedAt","channelId","title","description","channelTitle")]
  df <- dplyr::distinct(df, videoID, .keep_all = TRUE)
  df <- na.omit(df)
  names(df) <- c("video_ID", "video_dateTime","channel_ID","video_title","video_description","channel_title")
  df$pullDate <- Sys.time()

  list_of_video_ids <- as.character(df$video_ID)
  allVideoStats <- plyr::ldply(list_of_video_ids, ytcol::getVideoStatsDF)
  allVideoDetails <- plyr::ldply(list_of_video_ids, .fun = ytcol::getVideoDetailsDF)
  allVideoStats <- allVideoStats[,!names(allVideoStats) %in% c("pullDate")]

  allVideoInfo <- cbind(allVideoStats,allVideoDetails)
  df <- cbind(df, allVideoInfo)
  cols.dont.want <- c("channelId","channelTitle","liveBroadcastContent","localized.title","localized.description","thumbnails.standard.url",
                      "thumbnails.standard.width","thumbnails.standard.height","publishedAt","title","description",
                      "id","thumbnails.maxres.url","thumbnails.maxres.width","thumbnails.maxres.height",
                      "thumbnails.default.url","thumbnails.default.width","thumbnails.default.height",
                      "thumbnails.medium.url","thumbnails.medium.width","thumbnails.medium.height",
                      "thumbnails.high.url","thumbnails.high.width","thumbnails.high.height")
  df <- df[,! names(df) %in% cols.dont.want, drop=F]
  #date <- format(Sys.time(),"%Y%m%d_%H%M")
  #write.csv(df, file=paste("./yt_collection/","channel_",channel_id,"_!_",date,".csv", sep = ""), row.names = FALSE)

  return(df)
}

