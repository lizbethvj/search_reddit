#' Plot Top Words in Processed Comments
#'
#' @param comments data frame, result of `process_comments`
#' @param type chr string, one of: "col", "wordcloud"
#'
#' @return ggplot object
#' @export
#'
#' @import dplyr
#' @importFrom wordcloud wordcloud
#' @importFrom stats reorder
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 ggplot aes geom_col labs
#' @importFrom rlang .data
plot_comments <- function(comments, type){
  
  assert_that(is.data.frame(comments))
  expected_cols <- c('post_id', 'comment_id', 'word')
  assert_that(all(expected_cols %in% names(comments)),
              msg = paste("comments data frame is expected to have following columns: ", paste(expected_cols, collapse =", ")))
  match.arg(type, c("col","wordcloud"))
  
  switch(type,
         col = {
           comments %>%
             count(.data$word, sort = TRUE) %>%
             filter(.data$n > quantile(.data$n, probs = 0.99)) %>%
             mutate('word' = reorder(.data$word, n)) %>%
             ggplot(aes(.data$n, .data$word)) +
             geom_col() +
             labs(y = NULL, title = "Top 1% Words")
         },
         wordcloud = {
           
           lexicon_type = "nrc"
           lexicon <- get_sentiments(lexicon_type)
           lexicon_posneg <- filter(lexicon, .data$sentiment %in% c("positive", "negative"))
           
           comments %>%
             inner_join(lexicon_posneg) %>%
             count(.data$word) %>%
             with(wordcloud(word, n, max.words = 100))
         }
  )
  
}