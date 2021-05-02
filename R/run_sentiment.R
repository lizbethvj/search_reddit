#' Run Sentiment Analysis
#'
#' @param search chr string
#' @param max_results numeric
#' @param sort_by chr string
#' @param posts_from chr string
#'
#' @import dplyr
#' @return list of data frames
#' @export
#'
run_sentiment <- function(search,
                          max_results = 50,
                          sort_by = "relevance",
                          posts_from = "all"){
  message("1/4 Searching for posts")
  posts <- search_reddit(search, max_results, sort_by, posts_from)
  message("2/4 Extracting comments")
  comments <- extract_comments(posts$url)
  message("3/4 Processing comments")
  p.comments <- process_comments(comments)
  message("4/4 Calculating net sentiment")
  sentiments <- calc_net_sentiment(p.comments, comments)
  
  list(posts = posts,
       comments = comments,
       processed.comments = p.comments,
       sentiments = sentiments)
  
}