#' Network Diagram for Comments on Related Videos
#'
#'
#' This function takes the dataframe and generates a network diagram with the nodes being comment authors and videos.
#' The dataframe must have columns named "comment_ID", "author_display_name", and "video_ID".
#' The dataframes created with the yt.ChannelComments() and yt.RelatedVideoComments() functions
#' work well in this function.  The size of the nodes for comment authors indicates the relative number of videos
#' that author has commented on in the original set of related videos.  The function subsets the
#' related video comments dataframe by authors that have commented on at least the minimum
#' number of videos, so as to focus on authors who have commented on multiple videos as more
#' interesting entities.
#'
#' @param relatedComments  Dataframe object created with the yt.RelatedVideoComments function.  Must have the variables
#' "author_display_name" and "video_ID".
#' @param minVideos  Numeric for the minimum number of videos the author has commented on in the original set of related
#' videos.  Default is 1. Subsets authors with equal to or greater than the minVideos numeric.
#' @return Plots a network diagram with nodes being comment authors and video IDs.
#' @example \dontrun{yt.Network(relatedComments = df, minVideos = 3)}
#' @export
yt.Network <- function(relatedComments = NULL, minVideos = 1){
  edge <- relatedComments[,c("author_display_name","video_ID")]
  edge_counts <- dplyr::summarise(dplyr::group_by(edge, author_display_name, video_ID), count = n())  # number of comments by user on single video
  SumByDisplayName <- dplyr::summarise(dplyr::group_by(relatedComments,author_display_name),
                                numComments = n_distinct(comment_ID), numVids = n_distinct(video_ID))
  SumByDisplayName <- dplyr::arrange(SumByDisplayName, desc(numVids))
  if(minVideos > max(SumByDisplayName$numVids)) {
    stop(paste0("minVideos parameter is too large.  Must be less than ", max(SumByDisplayName$numVids), sep=" "))
  }
  multi_vids <- subset(SumByDisplayName, numVids >= minVideos)  #select only users with comments on at least x videos
  multi_edge <- dplyr::semi_join(edge,multi_vids, by = "author_display_name")  #pair subset of authors with videos they comment on
  multi_edge_distinct <- dplyr::distinct(multi_edge)
  multi_edge_count <- dplyr::summarise(dplyr::group_by(multi_edge,author_display_name), countVids = n())
  my_nodes <- as.character(unique(multi_edge_distinct$author_display_name))
  my_vids <- as.character(unique(multi_edge_distinct$video_ID))
  nodes <- data.frame(name = unique(c(my_nodes, my_vids)), stringsAsFactors = FALSE)
  nodes$id <- 0:(nrow(nodes) - 1)
  nodes <- dplyr::full_join(nodes, multi_edge_count, by = c("name" = "author_display_name"))  #add data for node strength based on number of videos author has commented on
  nodes[is.na(nodes)] <- 1  # data for node strength for videos
  nodes <- dplyr::rename(nodes, nodesize = countVids)
  link <- dplyr::left_join(multi_edge_distinct,nodes, by = c("author_display_name" = "name"))
  link <- link[,2:3]
  link <- dplyr::rename(link, source = id)
  link <- dplyr::left_join(link,nodes, by = c("video_ID" = "name"))
  link <- link[,2:3]
  link <- dplyr::rename(link, target = id)
  link$width <- 1
  nodes$group <- ifelse(nodes$name %in% my_nodes,"author","video")
  networkD3::forceNetwork(Links = link, Nodes = nodes,
               Source = "source",
               Target = "target",
               NodeID ="name",
               Group = "group",
               Value = "width",
               opacity = 0.9,
               zoom = TRUE,
               Nodesize = "nodesize",
               legend = TRUE,
               fontSize = 16)

}


#' Graphs of Comments on a Video Over Time
#'
#' This function produces two charts related to the temporal distribution
#' of comments on a video over time.  The input to the function is the dataframe
#' from the yt.VideoComments() function.  The first chart is a daily representation
#' of number of comments each day, starting with the date of the first comment and ending
#' on the date of the most recent comment.  The second chart is the cumulative number of
#' comments on the video over time.
#' @param videoComments  Dataframe object created with the yt.VideoComments() function.
#' Must have the variables "author_display_name" and "dateTime".
#' @param note  A string to put a note on the chart, such as the video ID or some other reference.
#' @param breakBy  String.  Set the breaks in the date sequence on the x-axis.  Takes one of five values:
#' \code{'day','week','month','quarter','year'}
#' @return Plots two charts, a histogram showing comment density over time and a cumulative line for total
#' comments over time.
#' @example \dontrun{yt.CommentsOverTime(videoComments = df, note = "video name: abc", breakBy = "week")}
#' @export
yt.CommentsOverTime <- function(videoComments = NULL, note = "", breakBy = "day"){

  is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
  if (is.installed(mypkg = "ggplot2")==FALSE) {
    stop("ggplot2 package is not installed")
  }
  require(ggplot2)

  if(!(breakBy %in% c("day","week","month","quarter","year"))){
    stop("breakBy can only take the values: day, week, month, quarter, or year")
  }

  comDate <- videoComments[,c("author_display_name", "dateTime")]
  comDate$dateTime <- as.Date(comDate$dateTime)
  minDate <- min(comDate$dateTime)
  maxDate <- max(comDate$dateTime)
  allDate <- data.frame(days = seq(as.Date(minDate), as.Date(maxDate),by = "day"), stringsAsFactors = FALSE)
  commentNums <- count(comDate, dateTime)
  maxCom <- max(commentNums$n)
  allDate <- dplyr::left_join(allDate,commentNums,by = c("days" ="dateTime"))
  allDate[is.na(allDate)] <- 0
  allDate$total <- cumsum(allDate$n)  #cumulative sum of comments over time
  totalMax <- max(allDate$total)
  datebreaks <- seq(as.Date(minDate),as.Date(maxDate), by=breakBy)
  p1<- ggplot2::ggplot(allDate,aes(x=days, y= n))+
    geom_bar(color = 'blue', fill = 'blue', stat='identity') +
    scale_x_date(breaks = datebreaks,labels = scales::date_format("%d %b %Y")) +
    theme(axis.text.x = element_text(angle=90)) +
    ggtitle("Comments Over Time") +
    xlab("Date") +
    ylab('# of Comments')+
    annotate("text",x = minDate, y = maxCom-1, label = note, hjust = -.2)

  #Plot cumulative count of comments over time
  p2 <- ggplot2::ggplot(allDate,aes(x=days, y= total))+
    geom_line() +
    scale_x_date(breaks = datebreaks,labels = scales::date_format("%d %b %Y")) +
    theme(axis.text.x = element_text(angle=90)) +
    ggtitle("Cumulative Comments Over Time") +
    xlab("Date") +
    ylab('Total # of Comments') +
    annotate("text",x = minDate, y = totalMax-1, label = note, hjust = -.2)

  return(list(p1,p2))
}
