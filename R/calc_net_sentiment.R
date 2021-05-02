#' Calculate Net Sentiment
#'
#' @param processed_comments data frame, result of `process_comments`
#' @param comments data frame, result of 'extract_comments'
#'
#' @return data frame
#' @export
#'
#' @import dplyr
#' @importFrom tidyr spread
#' @import tidytext
#' @importFrom rlang .data
#'
calc_net_sentiment <- function(processed_comments, comments){
  
  assert_that(is.data.frame(processed_comments) & is.data.frame(comments))
  processed_comments_cols <- c('post_id', 'comment_id', 'word')
  assert_that(all(processed_comments_cols %in% names(processed_comments)),
              msg = paste("processed_comments data frame is expected to have following columns: ", paste(processed_comments_cols, collapse =", ")))
  
  comments_cols <- c('post_id', 'title')
  assert_that(all(comments_cols %in% names(comments)),
              msg = paste("comments data frame is expected to have following columns: ", paste(comments_cols, collapse =", ")))
  
  
  
  lexicon_type = "nrc"
  lexicon <- get_sentiments(lexicon_type)
  lexicon_posneg <- filter(lexicon, .data$sentiment %in% c("positive", "negative"))
  
  post_titles <- select(comments, all_of(comments_cols)) %>%
    mutate("post_id" = as.numeric(.data$post_id))
  
  sentiments <- processed_comments %>%
    inner_join(lexicon_posneg, by = "word") %>%
    count(.data$post_id, .data$comment_id, .data$sentiment) %>%
    spread(.data$sentiment, .data$n, fill = 0) %>%
    mutate('net_sentiment' = .data$positive - .data$negative) %>%
    left_join(post_titles, by = "post_id") %>%
    select(all_of(c("title","post_id","comment_id","net_sentiment"))) %>%
    distinct()
  
  return(sentiments)
}
