#' Search Reddit
#' 
#' @param search character string, search query passed to reddit
#' @param max_results numeric, maximum number of reddit results, max is 100
#' @param sort_by character string, one of reddit's sorting algorithms e.g 'new', 'comments', 'relevance', 'hot', 'top'
#' @param posts_from character string, filter post based on when they were posted, one of: "hour", "day", "week", "month", "year", "all"
#'
#' @return data frame with post title, url, number of comments
#'
#'@importFrom assertthat assert_that 
#'@importFrom glue glue
#'@importFrom jsonlite fromJSON
#'@importFrom dplyr as_tibble mutate select
#'@export
search_reddit <- function (search,  
                           max_results = 50, 
                           sort_by = "relevance",
                           posts_from = "all"){
  
  #Check User Input
  match.arg(sort_by, c("new", "comments", "relevance", "hot", "top"))
  match.arg(posts_from, c("hour", "day", "week", "month", "year", "all"))
  assert_that(is.character(search) & length(search) == 1)
  assert_that(max_results %in% c(1:100),
              msg = "Enter a number between 1 and 100. Reddit allows up to 100 posts through JSON API.")
  #Build Search Query
  reddit.url <- "https://www.reddit.com"
  search.enc <- URLencode(search)
  search.url <- glue("{reddit.url}/search.json?q={search.enc}&sort={sort_by}&t={posts_from}&limit={max_results}")
  
  #Execute Search Query
  search.result <- readLines(search.url, warn = FALSE)
  result.json <- fromJSON(search.result)
  
  #Tidy Results Table
  result.df <- result.json[["data"]][["children"]][["data"]] %>%
    as_tibble() %>%
    mutate(url = paste0(reddit.url,permalink)) %>%
    select(title, subreddit, num_comments, url)
  
  return(result.df)
}
