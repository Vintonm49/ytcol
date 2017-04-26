#' Network Diagram for Comments on Related Videos
#' This function takes the dataframe created with the yt.RelatedVideoComments() function
#' and generates a network diagram with the nodes being comment authors and videos.
#' The size of the nodes for comment authors indicates the relative number of videos
#' that author has commented on in the original set of related videos.  The function subsets the
#' related video comments dataframe by authors that have commented on at least the minimum
#' number of videos, so as to focus on authors who have commented on multiple videos as more
#' interesting entities.
#' @param relatedComments  Dataframe object created with the yt.RelatedVideoComments function.  Must have the variables
#' "author_display_name" and "video_ID".
#' @param minVideos  Numeric for the minimum number of videos the author has commented on in the original set of related
#' videos.  Default is 1. Subsets authors with greater than the minVideos numeric.
#' @export
yt.Network <- function(relatedComments = NULL, minVideos = 1){
  edge <- relatedComments[,c("author_display_name","video_ID")]
  edge_counts <- dplyr::summarise(group_by(edge, author_display_name, video_ID), count = n())  # number of comments by user on single video
  SumByDisplayName <- dplyr::summarise(group_by(relatedComments,author_display_name),
                                numComments = n_distinct(comment_ID), numVids = n_distinct(video_ID))
  SumByDisplayName <- dplyr::arrange(SumByDisplayName, desc(numVids))
  multi_vids <- subset(SumByDisplayName, numVids > minVideos)  #select only users with comments on at least x videos
  multi_edge <- dplyr::semi_join(edge,multi_vids, by = "author_display_name")  #pair subset of authors with videos they comment on
  multi_edge_distinct <- dplyr::distinct(multi_edge)
  multi_edge_count <- dplyr::summarise(group_by(multi_edge,author_display_name), countVids = n())
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
