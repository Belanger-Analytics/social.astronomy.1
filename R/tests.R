# testing things, not really unit tests yet

# test <- get_reddit_comments(q = "coffee maker", size = 250)
# keurig <- get_reddit_comments(q = "keurig", size = 1000)
# varsol <- get_reddit_comments(q = "varsol", size = 1000)
# reddit_comments <- get_reddit_comments(q = '"coffee maker"', size = 25, fields ="id,body,subreddit")
# reddit_comments <- get_reddit_comments(q = 'plant rocket glasses', size = 100, fields ="id,body,subreddit,created_utc")
# top subreddits
# test %>% group_by(subreddit) %>% count() %>% arrange(desc(n))

test_functions <- function(){
library(tidyverse)
library(tidytext)
library(widyr)

# time to....?
mentions <- c(5, 10, 50, 100, 250, 500, 1000)

# only want the ones we can get
mentions <- mentions[times < nrow(test)]


reddit_comments %>%
  slice(mentions) %>%
  mutate(created_date = as.POSIXct(created_utc, origin = "1970-01-01") ) %>%
  mutate(time_to_mentions = difftime(Sys.time(), created_date)) %>%
  mutate(mentions = mentions) %>%
  select(mentions, time_to_mentions)


reddit_comments$created_utc %>% as.POSIXct(origin = "1970-01-01")


# https://www.tidytextmining.com/nasa.html

num_words <- 200

reddit_body <- reddit_comments %>%
  select(id, body) %>%
  tidytext::unnest_tokens(word, body) %>%
  dplyr::anti_join(bind_rows(tidytext::stop_words, dplyr::tibble(word = c(1:10, "it's"))))

word_counts <- reddit_body %>%
  dplyr::group_by(word) %>%
  dplyr::count(sort = TRUE) %>%
  dplyr::ungroup() %>%
  dplyr::slice_head(n = num_words)

body_trim <- reddit_body %>%
  dplyr::inner_join(word_counts, by = "word") %>%
  dplyr::rename(count = n)

word_pairs <- body_trim %>%
  widyr::pairwise_count(word, id, sort = TRUE, upper = FALSE) %>%
  dplyr::left_join(dplyr::select(body_trim, -id), by = c("item1" = "word")) %>%
  dplyr::distinct()


library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)

#vert <- word_pairs %>%


word_pairs_forplot <- word_pairs %>%
  filter(n >= 50)

word_counts_forplot <- word_counts %>%
  dplyr::filter(word %in% c(word_pairs_forplot$item1, word_pairs_forplot$item2))

word_pairs_forplot %>%
  graph_from_data_frame(vertices = word_counts_forplot) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  #geom_node_point(size = 5) +
  geom_node_point(aes(size = n, alpha = n)) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() +
  scale_size(range = c(2,10))
}
