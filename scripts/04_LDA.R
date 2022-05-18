library(tidyverse)
library(data.table)
# install.packages("topicmodels")
library(topicmodels)
library(tidytext)
library("textstem")
library(ggpubr)

all_revs_dt <-  data.table(readRDS(file = "./02_text_pre_out.rds"))

# Documents are reviews

# Split into words

tidy_revs <- all_revs_dt |> 
  select(review_text,category, rating, review_id , book_id) |> 
  unnest_tokens(word, review_text)

tidy_revs <- tidy_revs |> 
  anti_join(stop_words, by = "word")

# lemmatizing

tidy_revs <- tidy_revs |> 
  mutate(word = lemmatize_words(word))

# count occurrence on book level

word_counts <- tidy_revs |> 
  count(book_id , word, sort = TRUE)|>
  ungroup()

# create a matrix on review document level

books_dtm <- word_counts|>
  cast_dtm(book_id, word, n)

# Four topics: bio-history-mistery-thriller

books_lda <- LDA(books_dtm, k = 4, control = list(seed = 20220515))
# revs_lda <- LDA(reviews_dtm, k = 4, control = list(seed = 20220515))

# get model topics

# rev_topics <- tidy(revs_lda , matrix = "beta" )
book_rev_topics <- tidy(books_lda , matrix = "beta" )

# saveRDS(rev_topics, file = "lda_topics.rds") #temp save as detection is long process
saveRDS(book_rev_topics, file = "lda_book_topics.rds")

# top_terms <- rev_topics|>
#   group_by(topic)|>
#   top_n(10, beta)|>
#   ungroup()|>
#   arrange(topic, -beta)
# 
# top_terms|>
#   mutate(term = reorder(term, beta))|>
#   ggplot(aes(term, beta, fill = factor(topic))) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~ topic, scales = "free") +
#   coord_flip()


top_terms_b <- book_rev_topics|>
  group_by(topic)|>
  top_n(20, beta)|>
  ungroup()|>
  arrange(topic, -beta)

top_terms_b|>
  mutate(term = reorder(term, beta))|>
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Find books in topics

books_gamma <- tidy(books_lda, matrix = "gamma")

# Load book ids


all_books_dt <-  data.table(readRDS(file = "./all_books_dt.rds"))

all_books_dt <- all_books_dt |> select(book_id, title , popular_shelves)

avail_books <- all_books_dt$book_id

books_gamma <- books_gamma |> filter(document %in% avail_books)


topic1_top10 <- data.table(books_gamma)[topic == 1, .(document, gamma)][order(-gamma)][1:10]
topic2_top10 <- data.table(books_gamma)[topic == 2, .(document, gamma)][order(-gamma)][1:10]
topic3_top10 <- data.table(books_gamma)[topic == 3, .(document, gamma)][order(-gamma)][1:10]
topic4_top10 <- data.table(books_gamma)[topic == 4, .(document, gamma)][order(-gamma)][1:10]

topic1_top10 <- inner_join(topic1_top10, all_books_dt , by=c("document" = "book_id"), all.x = TRUE)
topic2_top10 <- inner_join(topic2_top10, all_books_dt , by=c("document" = "book_id"), all.x = TRUE)
topic3_top10 <- inner_join(topic3_top10, all_books_dt , by=c("document" = "book_id"), all.x = TRUE)
topic4_top10 <- inner_join(topic4_top10, all_books_dt , by=c("document" = "book_id"), all.x = TRUE)



dt_counts <- data.table(counts = NULL, shelves=NULL)

for (i in 1:10) {
  counts <- sapply(topic1_top10$popular_shelves,'[[',1)[[i]]
  shelves <- sapply(topic1_top10$popular_shelves,'[[',2)[[i]]
  dt_counts <- rbind(dt_counts , data.table(counts = counts, shelves=shelves))
}

topic1_books <-  dt_counts  |> 
  mutate(counts = as.numeric(counts))
        
         
bar_1 <- topic1_books |> group_by(shelves) |> summarise(sum = sum(counts)) |> arrange(-sum) |> head(10)

dt_counts <- data.table(counts = NULL, shelves=NULL)

for (i in 1:10) {
  counts <- sapply(topic2_top10$popular_shelves,'[[',1)[[i]]
  shelves <- sapply(topic2_top10$popular_shelves,'[[',2)[[i]]
  dt_counts <- rbind(dt_counts , data.table(counts = counts, shelves=shelves))
}

topic2_books <-  dt_counts  |> 
  mutate(counts = as.numeric(counts))


bar_2 <- topic2_books |> group_by(shelves) |> summarise(sum = sum(counts)) |> arrange(-sum) |> head(10)

dt_counts <- data.table(counts = NULL, shelves=NULL)

for (i in 1:10) {
  counts <- sapply(topic3_top10$popular_shelves,'[[',1)[[i]]
  shelves <- sapply(topic3_top10$popular_shelves,'[[',2)[[i]]
  dt_counts <- rbind(dt_counts , data.table(counts = counts, shelves=shelves))
}

topic3_books <-  dt_counts  |> 
  mutate(counts = as.numeric(counts))


bar_3 <- topic3_books |> group_by(shelves) |> summarise(sum = sum(counts)) |> arrange(-sum) |> head(10)

dt_counts <- data.table(counts = NULL, shelves=NULL)

for (i in 1:10) {
  counts <- sapply(topic4_top10$popular_shelves,'[[',1)[[i]]
  shelves <- sapply(topic4_top10$popular_shelves,'[[',2)[[i]]
  dt_counts <- rbind(dt_counts , data.table(counts = counts, shelves=shelves))
}

topic4_books <-  dt_counts  |> 
  mutate(counts = as.numeric(counts))


bar_4 <- topic4_books |> group_by(shelves) |> summarise(sum = sum(counts)) |> arrange(-sum) |> head(10)


g1 <- ggplot(data=bar_1, aes(x=reorder(shelves,-sum), y=sum)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("") +
  ylab("")+
  ggtitle("Topic 1") +
  theme_minimal()

g2 <- ggplot(data=bar_2, aes(x=reorder(shelves,-sum), y=sum)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("") +
  ylab("")+
  ggtitle("Topic 2") +
  theme_minimal()

g3 <- ggplot(data=bar_3, aes(x=reorder(shelves,-sum), y=sum)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("") +
  ylab("")+
  ggtitle("Topic 3") +
  theme_minimal()

g4 <- ggplot(data=bar_4, aes(x=reorder(shelves,-sum), y=sum)) +
  geom_bar(stat="identity", fill="steelblue")+
  xlab("") +
  ylab("")+
  ggtitle("Topic 4") +
  theme_minimal()


figure <- ggarrange(g1, g2 , g3 , g4,
          hjust = -0.6,
          ncol = 2, nrow = 2)

library(grid)

annotate_figure(figure, left = textGrob("Number of selections", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
                bottom = textGrob("Shelves selected by users", gp = gpar(cex = 1.3)))

