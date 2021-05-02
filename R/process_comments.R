#' Process Comments for tidytext
#'
#' @param comments data frame, result of `extract_comments` function
#' @param other_stop_words character vector of words to be removed, eg. for Reddit its 'gt', an artifact from inline HTML in the comments, for multiple use c("word1", "word2")
#'
#' @return data frame
#' @export
#'
#' @import dplyr
#' @import tidytext
#' @importFrom assertthat assert_that
#' @importFrom rlang .data
#'
process_comments <- function(comments, other_stop_words = "gt"){
  
  assert_that(is.data.frame(comments))
  expected_cols <- c('post_id', 'comment_id', 'comment')
  assert_that(all(expected_cols %in% names(comments)),
              msg = paste("comments data frame is expected to have following columns: ", paste(expected_cols, collapse =", ")))
  
  
  pcomments <- comments %>%
    select(all_of(expected_cols)) %>%
    mutate(
      post_id = as.numeric(.data$post_id),
      comment_id = as.numeric(.data$comment_id),
      id = .data$post_id*100+.data$comment_id) %>%
    group_by(across(c('post_id','comment_id'))) %>%
    unnest_tokens("word", comment) %>%
    anti_join(tidytext::stop_words, by = "word") %>%
    ungroup()
  
  other_stop_words_match <- pcomments$word %in% other_stop_words
  
  pcomments <- pcomments[!other_stop_words_match,]
  
  return(pcomments)
  
}