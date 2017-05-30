#' Underlying Function to Get Comments on a Video on YouTube
#'
#'
#' Basic function, adapted from tuber package, get_comment_threads().
#' Used by other comment functions in the ytcol package.
#'
#' @param filter Character; Required.
#' named vector of length 1.
#' Potential names of the entry in the vector:
#' \code{video_id}: video ID.
#' \code{channel_id}: channel ID.
#' \code{thread_id}: comma-separated list of comment thread IDs.
#' \code{threads_related_to_channel}: channel ID.
#'
#' @param part  Character. Comment resource requested. Required. Comma separated list of one or more of the
#' following: \code{id, snippet}. e.g., \code{"id, snippet"}, \code{"id"}, etc. Default: \code{snippet}.
#' @param max_results  Integer. Maximum number of items that should be returned. Integer. Optional. Can be between 20 and 100. Default is 100.
#' @param page_token  Character. Specific page in the result set that should be returned. Optional.
#' @param text_format Character. Default is \code{"html"}. Only takes \code{"html"} or \code{"plainText"}. Optional.
#' @param simplify Boolean. Default is \code{TRUE}. If \code{TRUE}, the function returns a data frame. Else a list with all the information returned.
#' @param \dots Additional arguments passed to \code{\link{yt_GET}}.
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
#' @param video_id Character.  Video ID from YouTube. Required.
#' @return Dataframe with variables including: \code{comment_ID, video_ID, author_display_name, author_channel_ID,
#' text_display, text_original, dateTime, updated_dateTime, reply_count, parent_comment_ID, parent_author_display_name,
#' parent_author_channel_ID, pullDate}
#' @export
yt.VideoComments <- function(video_id = NULL){
  comment1 <- ytcol::yt.GetComments(filter=c(video_id = video_id))
  comment2 <- ytcol::yt.GetComments(filter=c(video_id = video_id), simplify=FALSE)
  total <- comment2$pageInfo$totalResults
  if(total==0){
    print("No comments on this video")
    return(NA)

  }
  com_token <- comment2$nextPageToken
  if(is.null(com_token)){
    ##Get replies to comments if they exist
    comment22 <- ytcol::dataframeFromJSON(comment2$items)
    comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
    if(sum(comment22$snippet.totalReplyCount) > 0){
      reply<- subset(comment22, snippet.totalReplyCount > 0)  ## set of comments that have replies
      reply <- reply[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                        "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                        "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                        "snippet.totalReplyCount")]
      names(reply) <- c("parent_comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                        "text_original","parent_dateTime", "parent_updated_dateTime", "reply_count")
      list_of_parent_ids <- as.character(reply$parent_comment_ID)
      replydf<-data.frame()
      for (i in list_of_parent_ids) {  #get replies to comments that have replies
        comreply <- try(ytcol::yt.GetCommentReply(filter = c(parent_ID = i)))  ##max results is 100, get pageToken (check with sum(reply$reply_count))
        comreply <- ytcol::dataframeFromJSON(comreply$items)
        replydf <- rbind(replydf, comreply)
      }
      replydf <- replydf[,c("id","snippet.authorDisplayName","snippet.authorChannelId.value",
                            "snippet.textDisplay","snippet.textOriginal","snippet.parentId",
                            "snippet.publishedAt","snippet.updatedAt")]
      names(replydf) <- c("reply_comment_ID","author_display_name","author_channel_ID","text_display",
                          "text_original", "parent_comment_ID", "reply_dateTime","reply_updated_dateTime")
      replydf_join <- merge(x = replydf, y = reply, by = "parent_comment_ID", all.x = TRUE)
      drop_cols <- c("text_display.y","text_original.y","parent_dateTime","parent_updated_dateTime","reply_count")
      replydf_join <- replydf_join[,! names(replydf_join) %in% drop_cols, drop=F]
      names(replydf_join) <- c("parent_comment_ID" ,"comment_ID","author_display_name","author_channel_ID","text_display",
                               "text_original", "dateTime","updated_dateTime","video_ID",
                               "parent_author_display_name","parent_author_channel_ID")
      replydf_join$reply_count <- 0  #add the reply_count column
      comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                                 "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                                 "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                                 "snippet.totalReplyCount")]
      names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                             "text_original","dateTime", "updated_dateTime", "reply_count")
      comments_combo <- gtools::smartbind(comment222,replydf_join)
      comments_combo$pullDate <- Sys.time()
      comments_combo$text_original <- gsub('\n'," ", comments_combo$text_original)  #replace breaklines with space.
      comments_combo$text_display <- gsub('\n'," ", comments_combo$text_display)
      comments_combo$text_original <- gsub('<br />'," ", comments_combo$text_original)  #replace breaklines with space.
      comments_combo$text_display <- gsub('<br />'," ", comments_combo$text_display)
      #date <- format(Sys.time(),"%Y%m%d_%H%M")
      #write.csv(comments_combo, file=paste("./yt_collection/","comments_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
      return(comments_combo)
    }else{  #number of replies is zero and token is NULL
      comment22 <- ytcol::dataframeFromJSON(comment2$items)
      comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
      comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                                 "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                                 "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                                 "snippet.totalReplyCount")]
      names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                             "text_original","dateTime", "updated_dateTime", "reply_count")
      comment222$pullDate <- Sys.time()
      comment222$text_original <- gsub('\n'," ", comment222$text_original)  #replace breaklines with space.
      comment222$text_display <- gsub('\n'," ", comment222$text_display)
      comment222$text_original <- gsub('<br />'," ", comment222$text_original)  #replace breaklines with space.
      comment222$text_display <- gsub('<br />'," ", comment222$text_display)
      #date <- format(Sys.time(),"%Y%m%d_%H%M")
      #write.csv(comment222, file=paste("./yt_collection/","comments_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
      return(comment222)
    }

  }else {
    repeat{
      comment1_sub <- ytcol::yt.GetComments(filter=c(video_id = video_id), page_token = com_token)
      comment2_sub <- ytcol::yt.GetComments(filter=c(video_id = video_id), page_token = com_token,
                                            simplify=FALSE)
      comment22 <- ytcol::dataframeFromJSON(comment2$items)
      comment22_sub <- ytcol::dataframeFromJSON(comment2_sub$items)
      comment1 <- gtools::smartbind(comment1, comment1_sub)
      comment22 <- gtools::smartbind(comment22, comment22_sub)
      com_token <- comment2_sub$nextPageToken
      if(is.null(com_token)){
        break
      }
    }
  }
  ##Get replies to comments if they exist
  comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
  if(sum(comment22$snippet.totalReplyCount) > 0){
    reply<- subset(comment22, snippet.totalReplyCount > 0)  ## set of comments that have replies
    reply <- reply[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                      "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                      "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                      "snippet.totalReplyCount")]
    names(reply) <- c("parent_comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                      "text_original","parent_dateTime", "parent_updated_dateTime", "reply_count")
    list_of_parent_ids <- as.character(reply$parent_comment_ID)
    replydf<-data.frame()
    for (i in list_of_parent_ids) {
      comreply <- try(ytcol::yt.GetCommentReply(filter = c(parent_ID = i)))  ##max results is 100, get pageToken (check with sum(reply$reply_count))
      comreply <- ytcol::dataframeFromJSON(comreply$items)
      replydf <- rbind(replydf, comreply)
    }
    replydf <- replydf[,c("id","snippet.authorDisplayName","snippet.authorChannelId.value",
                          "snippet.textDisplay","snippet.textOriginal","snippet.parentId",
                          "snippet.publishedAt","snippet.updatedAt")]
    names(replydf) <- c("reply_comment_ID","author_display_name","author_channel_ID","text_display",
                        "text_original", "parent_comment_ID", "reply_dateTime","reply_updated_dateTime")
    replydf_join <- merge(x = replydf, y = reply, by = "parent_comment_ID", all.x = TRUE)
    drop_cols <- c("text_display.y","text_original.y","parent_dateTime","parent_updated_dateTime","reply_count")
    replydf_join <- replydf_join[,! names(replydf_join) %in% drop_cols, drop=F]
    names(replydf_join) <- c("parent_comment_ID" ,"comment_ID","author_display_name","author_channel_ID","text_display",
                             "text_original", "dateTime","updated_dateTime","video_ID",
                             "parent_author_display_name","parent_author_channel_ID")
    replydf_join$reply_count <- 0  #add the reply_count column
    comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                               "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                               "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                               "snippet.totalReplyCount")]
    names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                           "text_original","dateTime", "updated_dateTime", "reply_count")
    comments_combo <- gtools::smartbind(comment222,replydf_join)
    comments_combo$pullDate <- Sys.time()
    comments_combo$text_original <- gsub('\n'," ", comments_combo$text_original)  #replace breaklines with space.
    comments_combo$text_display <- gsub('\n'," ", comments_combo$text_display)
    comments_combo$text_original <- gsub('<br />'," ", comments_combo$text_original)  #replace breaklines with space.
    comments_combo$text_display <- gsub('<br />'," ", comments_combo$text_display)

    #date <- format(Sys.time(),"%Y%m%d_%H%M")
    #write.csv(comments_combo, file=paste("./yt_collection/","comments_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
    return(comments_combo)
  }else{
    comment22 <- ytcol::dataframeFromJSON(comment2$items)
    comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
    comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                               "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                               "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                               "snippet.totalReplyCount")]
    names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                           "text_original","dateTime", "updated_dateTime", "reply_count")
    comment222$pullDate <- Sys.time()
    comment222$text_original <- gsub('\n'," ", comment222$text_original)  #replace breaklines with space.
    comment222$text_display <- gsub('\n'," ", comment222$text_display)
    comment222$text_original <- gsub('<br />'," ", comment222$text_original)  #replace breaklines with space.
    comment222$text_display <- gsub('<br />'," ", comment222$text_display)
    #date <- format(Sys.time(),"%Y%m%d_%H%M")
    #write.csv(comment222, file=paste("./yt_collection/","comments_",video_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
    return(comment222)
  }

}


#' Utility for Getting Video Comments
#'
#' This function is a utility to be used in other functions to get comments for multiple
#' videos.  Internal use.  Eliminates the save to CSV portion (now eliminated) of the yt.VideoComments function.
#' @param video_id  Character.  Video ID from YouTube. Required.
#' @export
#'
yt.SimpleVideoComments <- function(video_id = NULL){
  comment1 <- ytcol::yt.GetComments(filter=c(video_id = video_id))
  comment2 <- ytcol::yt.GetComments(filter=c(video_id = video_id), simplify=FALSE)
  total <- comment2$pageInfo$totalResults
  if(total==0){
    print("No comments on this video")
    return(NA)

  }
  com_token <- comment2$nextPageToken
  if(is.null(com_token)){
    ##Get replies to comments if they exist
    comment22 <- ytcol::dataframeFromJSON(comment2$items)
    comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
    if(sum(comment22$snippet.totalReplyCount) > 0){
      reply<- subset(comment22, snippet.totalReplyCount > 0)  ## set of comments that have replies
      reply <- reply[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                        "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                        "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                        "snippet.totalReplyCount")]
      names(reply) <- c("parent_comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                        "text_original","parent_dateTime", "parent_updated_dateTime", "reply_count")
      list_of_parent_ids <- as.character(reply$parent_comment_ID)
      replydf<-data.frame()
      for (i in list_of_parent_ids) {  #get replies to comments that have replies
        comreply <- try(ytcol::yt.GetCommentReply(filter = c(parent_ID = i)))  ##max results is 100, get pageToken (check with sum(reply$reply_count))
        comreply <- ytcol::dataframeFromJSON(comreply$items)
        replydf <- rbind(replydf, comreply)
      }
      replydf <- replydf[,c("id","snippet.authorDisplayName","snippet.authorChannelId.value",
                            "snippet.textDisplay","snippet.textOriginal","snippet.parentId",
                            "snippet.publishedAt","snippet.updatedAt")]
      names(replydf) <- c("reply_comment_ID","author_display_name","author_channel_ID","text_display",
                          "text_original", "parent_comment_ID", "reply_dateTime","reply_updated_dateTime")
      replydf_join <- merge(x = replydf, y = reply, by = "parent_comment_ID", all.x = TRUE)
      drop_cols <- c("text_display.y","text_original.y","parent_dateTime","parent_updated_dateTime","reply_count")
      replydf_join <- replydf_join[,! names(replydf_join) %in% drop_cols, drop=F]
      names(replydf_join) <- c("parent_comment_ID" ,"comment_ID","author_display_name","author_channel_ID","text_display",
                               "text_original", "dateTime","updated_dateTime","video_ID",
                               "parent_author_display_name","parent_author_channel_ID")
      replydf_join$reply_count <- 0  #add the reply_count column
      comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                                 "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                                 "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                                 "snippet.totalReplyCount")]
      names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                             "text_original","dateTime", "updated_dateTime", "reply_count")
      comments_combo <- gtools::smartbind(comment222,replydf_join)
      comments_combo$pullDate <- Sys.time()
      comments_combo$text_original <- gsub('\n'," ", comments_combo$text_original)  #replace breaklines with space.
      comments_combo$text_display <- gsub('\n'," ", comments_combo$text_display)
      comments_combo$text_original <- gsub('<br />'," ", comments_combo$text_original)  #replace breaklines with space.
      comments_combo$text_display <- gsub('<br />'," ", comments_combo$text_display)
      return(comments_combo)
    }else{  #number of replies is zero and token is NULL
      comment22 <- ytcol::dataframeFromJSON(comment2$items)
      comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
      comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                                 "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                                 "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                                 "snippet.totalReplyCount")]
      names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                             "text_original","dateTime", "updated_dateTime", "reply_count")
      comment222$pullDate <- Sys.time()
      comment222$text_original <- gsub('\n'," ", comment222$text_original)  #replace breaklines with space.
      comment222$text_display <- gsub('\n'," ", comment222$text_display)
      comment222$text_original <- gsub('<br />'," ", comment222$text_original)  #replace breaklines with space.
      comment222$text_display <- gsub('<br />'," ", comment222$text_display)
      return(comment222)
    }

  }else {
    repeat{
      comment1_sub <- ytcol::yt.GetComments(filter=c(video_id = video_id), page_token = com_token)
      comment2_sub <- ytcol::yt.GetComments(filter=c(video_id = video_id), page_token = com_token,
                                            simplify=FALSE)
      comment22 <- ytcol::dataframeFromJSON(comment2$items)
      comment22_sub <- ytcol::dataframeFromJSON(comment2_sub$items)
      comment1 <- gtools::smartbind(comment1, comment1_sub)
      comment22 <- gtools::smartbind(comment22, comment22_sub)
      com_token <- comment2_sub$nextPageToken
      if(is.null(com_token)){
        break
      }
    }
  }
  ##Get replies to comments if they exist
  comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
  if(sum(comment22$snippet.totalReplyCount) > 0){
    reply<- subset(comment22, snippet.totalReplyCount > 0)  ## set of comments that have replies
    reply <- reply[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                      "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                      "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                      "snippet.totalReplyCount")]
    names(reply) <- c("parent_comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                      "text_original","parent_dateTime", "parent_updated_dateTime", "reply_count")
    list_of_parent_ids <- as.character(reply$parent_comment_ID)
    replydf<-data.frame()
    for (i in list_of_parent_ids) {
      comreply <- try(ytcol::yt.GetCommentReply(filter = c(parent_ID = i)))  ##max results is 100, get pageToken (check with sum(reply$reply_count))
      comreply <- ytcol::dataframeFromJSON(comreply$items)
      replydf <- rbind(replydf, comreply)
    }
    replydf <- replydf[,c("id","snippet.authorDisplayName","snippet.authorChannelId.value",
                          "snippet.textDisplay","snippet.textOriginal","snippet.parentId",
                          "snippet.publishedAt","snippet.updatedAt")]
    names(replydf) <- c("reply_comment_ID","author_display_name","author_channel_ID","text_display",
                        "text_original", "parent_comment_ID", "reply_dateTime","reply_updated_dateTime")
    replydf_join <- merge(x = replydf, y = reply, by = "parent_comment_ID", all.x = TRUE)
    drop_cols <- c("text_display.y","text_original.y","parent_dateTime","parent_updated_dateTime","reply_count")
    replydf_join <- replydf_join[,! names(replydf_join) %in% drop_cols, drop=F]
    names(replydf_join) <- c("parent_comment_ID" ,"comment_ID","author_display_name","author_channel_ID","text_display",
                             "text_original", "dateTime","updated_dateTime","video_ID",
                             "parent_author_display_name","parent_author_channel_ID")
    replydf_join$reply_count <- 0  #add the reply_count column
    comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                               "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                               "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                               "snippet.totalReplyCount")]
    names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                           "text_original","dateTime", "updated_dateTime", "reply_count")
    comments_combo <- gtools::smartbind(comment222,replydf_join)
    comments_combo$pullDate <- Sys.time()
    comments_combo$text_original <- gsub('\n'," ", comments_combo$text_original)  #replace breaklines with space.
    comments_combo$text_display <- gsub('\n'," ", comments_combo$text_display)
    comments_combo$text_original <- gsub('<br />'," ", comments_combo$text_original)  #replace breaklines with space.
    comments_combo$text_display <- gsub('<br />'," ", comments_combo$text_display)
    return(comments_combo)
  }else{
    comment22 <- ytcol::dataframeFromJSON(comment2$items)
    comment22$snippet.totalReplyCount <- as.numeric(levels(comment22$snippet.totalReplyCount))[comment22$snippet.totalReplyCount]
    comment222 <- comment22[,c("id","snippet.videoId","snippet.topLevelComment.snippet.authorDisplayName","snippet.topLevelComment.snippet.authorChannelId.value",
                               "snippet.topLevelComment.snippet.textDisplay","snippet.topLevelComment.snippet.textOriginal",
                               "snippet.topLevelComment.snippet.publishedAt","snippet.topLevelComment.snippet.updatedAt",
                               "snippet.totalReplyCount")]
    names(comment222) <- c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                           "text_original","dateTime", "updated_dateTime", "reply_count")
    comment222$pullDate <- Sys.time()
    comment222$text_original <- gsub('\n'," ", comment222$text_original)  #replace breaklines with space.
    comment222$text_display <- gsub('\n'," ", comment222$text_display)
    comment222$text_original <- gsub('<br />'," ", comment222$text_original)  #replace breaklines with space.
    comment222$text_display <- gsub('<br />'," ", comment222$text_display)
    return(comment222)
  }

}

#' Get Comments from All Videos on a Channel
#'
#' This function collects all the comments from all the videos on a channel,
#' within a time frame if set.
#'
#'@param channel_id  Character.  The YouTube channel ID.  Cannot be the vanity URL name. Required.
#'@param published_before Date.  Optional. RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#'@param published_after  Date.  Optional. RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#'@export
yt.ChannelComments <- function(channel_id=NULL, published_before=NULL, published_after=NULL){
  channelAct <- tuber::list_channel_activities(filter=c(channel_id = channel_id) ,part = "contentDetails",
                                               published_after = published_after,
                                               published_before = published_before)
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

  if(nrow(df) < 50){  # if equal to 50, there are more videos on the channel (max return hit)
    colnames(df)[which(colnames(df)=='contentDetails.upload.videoId')] <- "videoID"
    df <- dplyr::distinct(df, videoID, .keep_all = TRUE)
    df <- na.omit(df)
    list_of_video_ids <- as.character(df$videoID)
    comdf<-data.frame()
    for (i in list_of_video_ids) {
      comm <- try(ytcol::yt.SimpleVideoComments(i))
      comdf <- gtools::smartbind(comdf,comm)
    }
    comdf <- comdf[,c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                      "text_original","dateTime", "updated_dateTime", "reply_count", "parent_comment_ID",
                      "parent_author_display_name","parent_author_channel_ID","pullDate")]
    comdf <- comdf[!is.na(comdf$comment_ID),]
    #date <- format(Sys.time(),"%Y%m%d_%H%M")
    #write.csv(comdf, file=paste("./yt_collection/","channel_comments_",channel_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
    print(paste0("Number of videos: ",nrow(df)))
    return(comdf)
    break
  }
  repeat{  #get the rest of the video IDs
    channelActSub <- tuber::list_channel_activities(filter=c(channel_id = channel_id), part = "contentDetails",
                                                    published_after = published_after,
                                                    published_before = published_before,
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

    } else {
      dff <- dff[,4, drop = FALSE]
      colnames(dff)[which(colnames(dff)=='contentDetails.upload.videoId')] <- "videoID" #only need if col=4
    }
    df <- gtools::smartbind(df, dff)
    token <- channelActSub$nextPageToken
    if(is.null(token)){
      break
    }
  }
  colnames(df)[which(colnames(df)=='contentDetails.upload.videoId')] <- "videoID" #only need if col=4
  df <- dplyr::distinct(df,videoID)
  list_of_video_ids <- as.character(df$videoID)
  comdf<-data.frame()
  for (i in list_of_video_ids) {
    comm <- try(ytcol::yt.SimpleVideoComments(i))
    comdf <- gtools::smartbind(comdf,comm)
  }
  comdf <- comdf[,c("comment_ID", "video_ID", "author_display_name","author_channel_ID","text_display",
                    "text_original","dateTime", "updated_dateTime", "reply_count", "parent_comment_ID",
                    "parent_author_display_name","parent_author_channel_ID","pullDate")]
  comdf <- comdf[!is.na(comdf$comment_ID),]
  #date <- format(Sys.time(),"%Y%m%d_%H%M")
  #write.csv(comdf, file=paste("./yt_collection/","channel_comments_",channel_id,"_!_",date,".csv", sep = ""), row.names = FALSE)
  print(paste0("Number of videos: ",nrow(df)))
  return(comdf)
}




#' Get Function for Collecting Comment Replies from a YouTube Video
#'
#' Basic function, adapted from tuber package, get_comments().
#' Used by other comment functions in the ytcol package.
#'
#' @param filter Character; Required.
#' named vector of length 1.
#' Potential names of the entry in the vector:
#' \code{comment_ID}: comment ID.
#' \code{parent_ID}: parent comment ID.
#' @param part  Character. Comment resource requested. Required. Comma separated list of one or more of the
#' following: \code{id, snippet}. e.g., \code{"id, snippet"}, \code{"id"}, etc. Default: \code{snippet}.
#' @param max_results  Integer. Maximum number of items that should be returned. Integer. Optional. Can be between 20 and 100. Default is 100.
#' @param page_token  Character. Specific page in the result set that should be returned. Optional.
#' @param text_format Character. Default is \code{"html"}. Only takes \code{"html"} or \code{"plainText"}. Optional.
#' @param \dots Additional arguments passed to \code{\link{yt_GET}}.
#' @return Nested named list.
#' @export
#'
yt.GetCommentReply <- function (filter=NULL, part="snippet", text_format="html", max_results=100, page_token = NULL, ...) {

  if (max_results < 20 | max_results > 100) stop("max_results only takes a value between 20 and 100.")
  if (text_format != "html" & text_format !="plainText") stop("Provide a legitimate value of textFormat.")

  if (!(names(filter) %in% c("comment_ID", "parent_ID"))) stop("filter can only take one of values: comment_ID or parent_ID")
  if ( length(filter) != 1) stop("filter must be a vector of length 1.")

  translate_filter   <- c(parent_ID = 'parentId', comment_ID ='id')
  yt_filter_name     <- as.vector(translate_filter[match(names(filter), names(translate_filter))])
  names(filter)      <- yt_filter_name

  querylist <- list(part=part, maxResults=max_results, textFormat=text_format, pageToken=page_token)
  querylist <- c(querylist, filter)

  res <- ytcol::yt_GET("comments", querylist, ...)

  # if (simplify==TRUE & part=="snippet") {
  #   simple_res  <- lapply(res$items, function(x) unlist(x$snippet$topLevelComment$snippet))
  #   simpler_res <- plyr::ldply(simple_res, rbind)
  #   return(simpler_res)
  #}

  res

}




