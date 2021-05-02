#' Extract Comments
#'
#' @param urls urls of reddit posts
#'
#' @return data frame with comments
#' @export
#'
#' @importFrom purrr map_dfr quietly 
#' @importFrom RedditExtractoR reddit_content
#' @import dplyr
#' 
extract_comments <- function(urls){
  
  quiet_reddit_content <- function(url){
    #Remove progress bar from rmarkdown output
    quiet_reddit <- quietly(reddit_content)
    
    quiet_reddit(url)[["result"]] %>%
      #force character type on all columns
      mutate(across(.fns = as.character))
  }
  
  map_dfr(urls, quiet_reddit_content, .id = "post_id") %>% 
    rename(comment_id = id) %>%
    as_tibble()
}