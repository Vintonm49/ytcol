% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ytVideos.R
\name{getVideoDetailsDF}
\alias{getVideoDetailsDF}
\title{Get details for a video on YouTube}
\usage{
getVideoDetailsDF(video_id)
}
\arguments{
\item{l}{video_id  Character.  Video ID from YouTube.  Required.}
}
\value{
A dataframe with multiple variables:\code{id, publishedAt, channelId, title, description,
thumbnail data, tags, categoryId, liveBroadcastContent, localized.title, localized.description}
}
\description{
This function takes a YouTube video ID and returns details in a dataframe
including date published, channel ID, title, description, channel title, category ID, and tags.
It is used by other video related functions in the package.
}
