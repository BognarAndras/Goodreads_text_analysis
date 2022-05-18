library(data.table)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(modelsummary)
library("SnowballC")
# install.packages("textstem")
# install.packages("textdata")
# install.packages("ggwordcloud")
library(ggwordcloud)
library("textdata")
library("textstem")
library(ggpubr)
data("stop_words")
library(ggwordcloud)

all_revs_dt <-  data.table(readRDS(file = "./02_text_pre_out.rds"))

# 1. EDA 

Missing <- name <- function(x) {sum(is.na(x))}
P95 <- function(x){quantile(x,0.95,na.rm=T)}
P05 <- function(x){quantile(x,0.05,na.rm=T)}

datasummary(  
                (`Number of characters` = rev_len) *
                  (`Category` = factor(category)) * (`Rating` = factor(rating)) +
                  (`Number of words` = rev_num_words) *
                  (`Category` = factor(category)) * (`Rating` = factor(rating)) + 
                  (`Avg. word length` = rev_mean_word_len) *
                  (`Category` = factor(category)) * (`Rating` = factor(rating)) +
                  (`Number of unique words` = rev_unq_word) *
                  (`Category` = factor(category)) * (`Rating` = factor(rating)) ~ 
                N + Mean + Min + Max + P05 + P95 ,
              data = all_revs_dt ,
              fmt = 1 ,
              title = '<center><b>Table 2. Descriptive statistics<b></center>') |>  
              kable_material_dark(c("striped", "hover")) |> 
              kable_styling(latex_options = c("HOLD_position","scale_down"))

# 2. Word Occurrence

tidy_revs <- all_revs_dt |> 
  select(review_text,category, rating) |> 
  unnest_tokens(word, review_text)

tidy_revs <- tidy_revs |> 
  anti_join(stop_words, by = "word")

# lemmatizing

tidy_revs <- tidy_revs |> 
  mutate(word = lemmatize_words(word))

# book related words

generic_words <- c("book" , "author" , "read" , "character" , "story" , "page" , "write" , "series" , "review")

top_mistery <- tidy_revs |> 
  filter(category == "mistery", rating == 5) |> 
  count(word, sort = T) |> 
  head(15)|> 
  mutate(generic_word = word %in% generic_words )

bot_mistery <- tidy_revs |> 
  filter(category == "mistery", rating == 1) |> 
  count(word, sort = T) |> 
  head(15) |> 
  mutate(generic_word = word %in% generic_words)

top_bio <- tidy_revs |> 
  filter(category == "history", rating == 5) |> 
  count(word, sort = T) |> 
  head(15)|> 
  mutate(generic_word = word %in% generic_words )

bot_bio <- tidy_revs |> 
  filter(category == "history", rating == 1) |> 
  count(word, sort = T) |> 
  head(15) |> 
  mutate(generic_word = word %in% generic_words)


g1 <- ggplot(top_bio   , aes(reorder(word,n) , n, fill = generic_word)) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))
g2 <- ggplot(top_mistery   , aes(reorder(word,n) , n, fill = generic_word)) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))
g3 <- ggplot(bot_bio   , aes(reorder(word,n) , n, fill = generic_word)) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))
g4 <- ggplot(bot_mistery   , aes(reorder(word,n) , n, fill = generic_word)) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))

g1 <- g1 + ggtitle("History 5 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))
g2 <- g2 + ggtitle("Mistery 5 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))
g3 <- g3 + ggtitle("History 1 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))
g4 <- g4 + ggtitle("Mistery 1 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))

figure <- ggarrange(g1, g2 , g3 , g4,
          hjust = -0.6,
          ncol = 2, nrow = 2, common.legend = TRUE, legend="top") 

annotate_figure(figure, 
                bottom = textGrob("Word count", gp = gpar(cex = 1.3)))
  

# Without generic words

tidy_revs_nogen <- tidy_revs |> 
  mutate(generic_word = word %in% generic_words) |> 
  filter(generic_word == FALSE)


top_mistery2 <- tidy_revs_nogen |> 
  filter(category == "mistery", rating == 5) |> 
  count(word, sort = T) |> 
  head(15)

bot_mistery2 <- tidy_revs_nogen |> 
  filter(category == "mistery", rating == 1) |> 
  count(word, sort = T) |> 
  head(15)

top_bio2 <- tidy_revs_nogen |> 
  filter(category == "history", rating == 5) |> 
  count(word, sort = T) |> 
  head(15)

bot_bio2 <- tidy_revs_nogen |> 
  filter(category == "history", rating == 1) |> 
  count(word, sort = T) |> 
  head(15)


gg1 <- ggplot(top_bio2   , aes(reorder(word,n) , n, fill = "orange")) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))
gg2 <- ggplot(top_mistery2   , aes(reorder(word,n) , n, fill = "orange")) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))
gg3 <- ggplot(bot_bio2   , aes(reorder(word,n) , n, fill = "orange")) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))
gg4 <- ggplot(bot_mistery2   , aes(reorder(word,n) , n, fill = "orange")) + geom_col() + xlab(NULL) + coord_flip() + guides(fill=guide_legend(title="Generic Word about books"))

gg1 <- gg1 + ggtitle("History 5 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))
gg2 <- gg2 + ggtitle("Mistery 5 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))
gg3 <- gg3 + ggtitle("History 1 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))
gg4 <- gg4 + ggtitle("Mistery 1 star") + rremove("xlab") +
  theme(plot.title = element_text(hjust = 0.5))

figure2 <- ggarrange(gg1, gg2 , gg3 , gg4,
                    hjust = -0.6,
                    ncol = 2, nrow = 2, common.legend = TRUE, legend="top") 

annotate_figure(figure2, 
                bottom = textGrob("Word count", gp = gpar(cex = 1.3)))

# 3. Sentiment Analysis

bing_sentiments <- get_sentiments("bing")

tidy_revs_nogen |> inner_join(get_sentiments("bing") , by = "word") |> 
  filter(sentiment %in% c("positive" , "negative")) |> 
  mutate(method = "NRC", n = row_number()) |> 
  spread(n, sentiment , fill = 0 ) |> 
  mutate(sentiment = positive - negative)


books_categ_dt <- tidy_revs_nogen |> 
  group_by(category) |> 
  mutate(linenumber = row_number()) |> 
  ungroup() |> 
  unnest_tokens(word, review_text)


nrcpos <- get_sentiments("nrc") |> 
  filter(sentiment == "positive")

nrcneg <- get_sentiments("nrc") |> 
  filter(sentiment == "negative")

mistery_nrc_pos <- books_categ_dt |> 
  filter(category == "mistery" , rating == 5) |> 
  inner_join(nrcpos, by = "word") |> 
  count(word, sort = TRUE) |>  head(20)
  
history_nrc_pos <- books_categ_dt |> 
  filter(category == "history" , rating == 5) |> 
  inner_join(nrcpos, by = "word") |> 
  count(word, sort = TRUE) |>  head(20)


mistery_nrc_neg <- books_categ_dt |> 
  filter(category == "mistery" , rating == 1) |> 
  inner_join(nrcneg, by = "word") |> 
  count(word, sort = TRUE) |>  head(20)


history_nrc_neg <- books_categ_dt |> 
  filter(category == "mistery" , rating == 1) |> 
  inner_join(nrcneg, by = "word") |> 
  count(word, sort = TRUE) |>  head(20)


create_wc <- function(dt, title) {
  dt |>  ggplot(aes(label = word, size = n,
                                color = factor(sample.int(10, nrow(dt), replace = TRUE)))) +
                      geom_text_wordcloud() +
                      scale_size_area(max_size = 24) +
                      ggtitle(title) +
                      theme_minimal() +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      theme(plot.title = element_text(size=22))
  
}



w1 <- create_wc(mistery_nrc_pos, "History 5 star")
w2 <- create_wc(history_nrc_pos, "Mistery 5 star")
w3 <- create_wc(history_nrc_neg, "History 1 star")
w4 <- create_wc(mistery_nrc_neg, "Mistery 1 star")
                

ggarrange(w1, w2 , w3 , w4,
                    hjust = -0.6,
                    ncol = 2, nrow = 2) 

saveRDS(all_revs_dt, file = "03_cleaned_out.rds") #temp save as detection is long process