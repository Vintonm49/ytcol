#' Search YouTube for a key term with a date range
#' This function makes use of the yt_search function in the tuber package.
#' and pulls all videos for the set term within the set date range.
#'
#' @param term  String.
#' @param published_before Date.  RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @param published_after  Date.  RFC 339 Format.  Example, "1970-01-01T00:00:00Z"
#' @return A dataframe with x variables:
#' @export


yt.search <- function(term=NULL, published_before=NULL, published_after=NULL){

  search_vids <- tuber::yt_search(term = term, published_before = published_before, published_after = published_after)
  search_list <- tuber::yt_search(term = term, published_before = published_before, published_after = published_after,
                           simplify = FALSE)
  token<-search_list$nextPageToken
  df <- ytcol::dataframeFromJSON(search_list$items)

  repeat{
    search_vids_sub <- tuber::yt_search(term = term,
                                 published_before=published_before,
                                 published_after = published_after,
                                 page_token = token)
    search_list_sub <- tuber::yt_search(term = term, published_before = published_before,
                                 published_after = published_after,
                                 simplify = FALSE, page_token = token)
    token <- search_list_sub$nextPageToken
    dff<- ytcol::dataframeFromJSON(search_list_sub$items)
    df <- gtools::smartbind(df, dff)
    search_vids <- gtools::smartbind(search_vids,search_vids_sub)
    if(is.null(token)){
      break
    }
  }
  df<-cbind(df,search_vids)
  df<-df[,c(4,20,21,22,23,33)]
  names(df)[1:2] <- c("videoID", "dateTime")
  date <- format(Sys.time(),"%Y%m%d_%H%M")
  write.csv(df, file=paste("./yt_collection/","search_",term,"_!_",date,".csv", sep = ""), row.names = FALSE)

  return(df)
}


#' Set up YouTube Authorization
#'
#' This function uses the yt_oauth() function in the tuber package to
#' launch a browser that will allow you to authorize the application.
#'
#' @param client  String.  Client ID from YouTube OAuth 2.0
#' @param secret  String.  Client Secret from YouTube OAuth 2.0
#' @export

yt.oauth <- function(client, secret){
  tuber::yt_oauth(client, secret)
}
