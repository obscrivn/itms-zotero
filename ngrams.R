#library(dplyr)
#library(tidytext)
#library(janeaustenr)

#load('rubase.rda')
#austen_bigrams <- austen_books() %>%
  #data_frame(title = title, txt = text) %>%
 # unnest_tokens(unigram, text,token="ngrams", n=1) %>%
 # filter(!unigram %in% stop_words$word) %>%
#dplyr::count(book, unigram)
#d <- data.frame(text = c("da ma ai","da ma","a aa ma"), stringsAsFactors = FALSE)
#for (i  in 1:nrow(d)) {
 # w <- unlist(strsplit(d$text[1], " "))
 # df <- data.frame(table(w))
 # colnames(df) <- c("word","count")
  #nrow(df)
#  title <- data.frame(rep("2017",nrow(df)))
 # colnames(title) <- "year"
 # cbind(title, df)
#}
#tail(austen_bigrams)

# ngramBuilder2 <- function(date, text) {
#   d <- data.frame(date = date, text = text)
#   for (i  in 1:length(text)) {
#     w <- unlist(text[i], " ")
#     df <- data.frame(table(w))
#     colnames(df) <- c("word","count")
#     #nrow(df)
#    # title <- data.frame(rep(d$date[i],nrow(df)))
#   #  colnames(title) <- "year"
#    # tab <- cbind(title, df)
#   }
#  # d %>%
#    # unnest_tokens(unigram, txt,token="ngrams", n=1) %>%
#    # filter(!unigram %in% stop_words$word) %>%
#   #  count(unigram, sort=TRUE) #%>%
#     #dplyr::count(date, unigram)
# return(df)
# }

#title <- rubase$titles[[1]]
#d = data.frame(txt = text)
#d %>% unnest_tokens(word, txt)
#text <- unlist(rubase$preprocessingSteps$lda.format)[[1]]

#d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 2) %>% count(ngram, sort =TRUE)

#d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 1) %>% count(ngram, sort =TRUE)

#d1 <- d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 1) %>% count(ngram, sort =TRUE)
#unnest_tokens(bigram, text, token="ngrams", n=2)

#ng = ngramBuilder(title,text)
#ng[[1]]
ngramBuilder <- function(title, text) {
#require(dplyr)
#require(tidytext)
#require(janeaustenr)
#require(tidyverse)
  d <- data_frame(title = title, txt = text)
#d <- data_frame(title = extract[[3]], txt = extract[[2]])

d %>%
  unnest_tokens(word, txt)
d_bigrams <- d %>%
  unnest_tokens(bigram, txt, token="ngrams", n=2)

d_unigrams <- d %>%
  unnest_tokens(unigram, txt, token="ngrams", n=1)

d_unigrams %>%
  dplyr::count(unigram, sort = TRUE)

d_bigrams %>%
  dplyr::count(bigram, sort = TRUE)

bigrams_separated <- d_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

addwords <- c("page","blank")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% addwords,
       !word2 %in% addwords)

unigram_filtered <- d_unigrams %>%
  filter(!unigram %in% stop_words$word) %>%
  filter(!unigram %in% addwords)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  dplyr::count(word1, word2, sort = TRUE)

unigram_counts <- unigram_filtered %>% 
  dplyr::count(unigram, sort = TRUE)

bigrams_counts_united <- bigrams_united %>%
  dplyr::count(bigram, sort = TRUE) 
### tf - idf
bigram_tf_idf <- bigrams_united %>%
  dplyr::count(title, bigram) %>%
  bind_tf_idf(bigram, title, n) %>%
  arrange(desc(tf_idf))
table_bigram <- bigram_tf_idf[,c(2,3)]
#table_bigram$tf_idf <- table_bigram$tf_idf *100
colnames(table_bigram) <- c("bigram","count")

bigram_tf_idf  %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  group_by(title) %>%
  top_n(5) %>%
  ungroup 
  # ggplot(aes(bigram, tf_idf, fill = title)) +
  # geom_col(show.legend = FALSE) +
  # labs(x = NULL, y = "tf-idf") +
  # facet_wrap(~title, ncol = 1, scales = "free") +
  # coord_flip()
# d_trigrams <- d %>%
#   unnest_tokens(trigram, txt, token = "ngrams", n = 3) %>%
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
#   filter(!word1 %in% stop_words$word,
#          !word2 %in% stop_words$word,
#          !word3 %in% stop_words$word) %>%
#   dplyr::count(word1, word2, word3, sort = TRUE)
info <- list(bigrams_counts_united, d, bigram_tf_idf, table_bigram, unigram_counts,bigrams_counts_united, bigram_counts,bigrams_separated)
return(info)
} 

#p <- ngramBuilder(mydata)

