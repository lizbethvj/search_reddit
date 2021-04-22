#load information needed 

library(tidyverse)

library(assertthat)

library(glue)

library(jsonlite)

#load reddit library

library(RedditExtractoR)

# create reddit_urls_mod (based on RedditExtractoR::reddit_urls) function to allow all reddit post sorting algorithms

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

#extract posts
search_query <- "domestic abuse"
title_must_contain <- "abuse"
sort_types <- c("relevance","top")
max_results <- 100
min_comments <- 1

#For each sort type execute separate search 
df_raw <- map_dfr(.x = sort_types, 
                  .f = function(x) search_reddit(search_query,
                                                 max_results = max_results,
                                                 sort_by = x), 
                  .id = "sort_id")
df_raw <- df_raw %>%
  mutate(sort_id = as.numeric(sort_id)) %>%
  left_join(tibble(sort_id = seq_along(sort_types), sort = sort_types)) %>%
  as_tibble() %>%
  filter(num_comments >= min_comments)

df_raw

top_posts <- 5

df_filtered <- df_raw %>% 
  group_by(title) %>% 
  summarize(sort_n = n(),
            num_comments = num_comments[1],
            url = URL[1]) %>% 
  ungroup() %>%
  filter(sort_n == 2) %>%
  arrange(desc(num_comments)) %>%
  top_n(top_posts)

df_filtered

