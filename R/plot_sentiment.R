#' Plot Sentiments
#'
#' @param sentiments data frame, result of `calc_net_sentiment`
#'
#' @return ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot geom_col aes facet_wrap
#' @importFrom stringr str_wrap
#' @importFrom rlang .data
plot_sentiments <- function(sentiments){
  
  assert_that(is.data.frame(sentiments))
  sentiments_cols <- c('comment_id', 'net_sentiment','title')
  assert_that(all(sentiments_cols %in% names(sentiments)),
              msg = paste("comments data frame is expected to have following columns: ", paste(sentiments_cols, collapse =", ")))
  
  ggplot(sentiments, aes(.data$comment_id, .data$net_sentiment, fill = .data$title)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~str_wrap(.data$title), ncol = 2, scales = "free")
  
}