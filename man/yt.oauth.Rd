% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ytSearch.R
\name{yt.oauth}
\alias{yt.oauth}
\title{Set up YouTube Authorization}
\usage{
yt.oauth(client, secret)
}
\arguments{
\item{client}{String.  Client ID from YouTube OAuth 2.0}

\item{secret}{String.  Client Secret from YouTube OAuth 2.0}
}
\description{
This function uses the yt_oauth() function in the tuber package to
launch a browser that will allow you to authorize the application.
}