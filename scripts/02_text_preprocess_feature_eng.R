library(data.table)
# install.packages("textcat")
library("textcat")
if (!require("pacman")) install.packages("pacman") # for cld2, cld3
pacman::p_load("cld2")
pacman::p_load("cld3")
library(kableExtra)
# install.packages("tidytext")
library(tidytext)

mist_thrill_reviews_dt <-  data.table(readRDS(file = "./mist_thrill_reviews_dt.rds"))
hist_bio_reviews_dt <- data.table(readRDS(file = "./hist_bio_reviews_dt.rds"))
hist_bio_books_dt <- data.table(readRDS(file = "./hist_bio_books_dt.rds"))
mist_thrill_books_dt <- data.table(readRDS(file = "./mist_thrill_books_dt.rds"))


# Join to Single Table

# First mark

mist_thrill_reviews_dt[ , "category" := ("mistery")]
mist_thrill_books_dt[ , "category" := ("mistery")]
hist_bio_reviews_dt[ , "category" := ("history")]
hist_bio_books_dt[ , "category" := ("history")]

# Then merge

all_books_dt <- rbind(mist_thrill_books_dt,hist_bio_books_dt)
all_revs_dt <- rbind(mist_thrill_reviews_dt,hist_bio_reviews_dt)

rm(mist_thrill_reviews_dt , mist_thrill_books_dt, hist_bio_reviews_dt, hist_bio_books_dt)

# Remove Duplicates from both categories

# Mark duplicate rows

all_books_dt[, duplicate := .N > 1, by = "book_id"]

# Remove

nrow(all_books_dt) # 3806
all_books_dt <- all_books_dt[!(duplicate)]
nrow(all_books_dt) # 3704

# Drop col

all_books_dt[,duplicate:=NULL]

# Same for reviews

all_revs_dt[, duplicate := .N > 1, by = "review_id"]


nrow(all_revs_dt) # 65768
all_revs_dt <- all_revs_dt[!(duplicate)]
nrow(all_revs_dt) # 63680

all_revs_dt[,duplicate:=NULL]

# Cleaning text

# all_revs_dt$review_text[1]

# Remove links

# url_table <- all_revs_dt[grepl( 'http', review_text, fixed = TRUE) , .(review_id, review_text)]
# nrow(url_table) #1513
# 
# # Experiment with removing just the URL
# url_table$review_text[133]
# gsub('http\\S+\\s*', '', url_table$review_text[321])
# gsub('http\\S+\\s*', '', url_table$review_text[133])
# 
# rm(url_table)

all_revs_dt[, review_text := gsub('http\\S+\\s*', '', review_text) ]

# Remove Missing

all_revs_dt <- all_revs_dt[!(review_text == "")]
nrow(all_revs_dt) # 63603

# Remove non-English

# First, detech language with package



all_revs_dt[, language := textcat(review_text) ]
all_revs_dt[, language2 := cld2::detect_language(text = review_text , plain_text = FALSE) ]
all_revs_dt[, language3 := cld3::detect_language(text = review_text) ]


# saveRDS(all_revs_dt, file = "all_revs_dt.rds") #temp save as detection is long process
# saveRDS(all_books_dt, file = "all_books_dt.rds") 
# all_revs_dt <-  data.table(readRDS(file = "./all_revs_dt.rds"))

foreign_dt <- all_revs_dt[language != "english"]
textcat_non_en <- nrow(foreign_dt) #9949

foreign_dt2 <- all_revs_dt[language2 != "en"]
cld2_non_en <- nrow(foreign_dt2) #1984

foreign_dt3 <- all_revs_dt[language3 != "en"]
cld3_non_en <- nrow(foreign_dt3) # 4252

num_foreign <- c(textcat_non_en,cld2_non_en,cld3_non_en)

textcat_missing <- all_revs_dt[, .(sum(is.na(language)))]
cld2_missing <- all_revs_dt[, .(sum(is.na(language2)))]
cld3_missing <- all_revs_dt[, .(sum(is.na(language3)))]
missing <- c(textcat_missing,cld2_missing,cld3_missing)

random <- foreign_dt[sample(nrow(foreign_dt), 20), .(review_text , language)]
random2 <- foreign_dt2[sample(nrow(foreign_dt2), 20), .(review_text , language2)]
random3 <- foreign_dt3[sample(nrow(foreign_dt3), 20), .(review_text , language3)]

textcat_acc <- 0.5
cld2_acc <- 0
cld3_acc <- 0.2
mislabel <- c(textcat_acc,cld2_acc,cld3_acc)

lang_detect_dt <- data.table(package=c("textcat","cld2","cld3"),
                          num_foreign=num_foreign,missing=missing,misslabel=mislabel)

kbl(lang_detect_dt , align=rep('c', 4) ,
    caption = "<center><b>Table 1. Language Detection Packages<b></center>") |>  
    kable_material_dark(c("striped", "hover"))

# Choose cld2

nrow(all_revs_dt) # 63603
all_revs_dt <- all_revs_dt[(language2 == "en") & (!(is.na(language2)))]
nrow(all_revs_dt) # 58696

all_revs_dt[, c("language","language2","language3"):=NULL]

# Feature Engineering

rev_mean_word_len <- function(text) {
  rev_mean_word_len = mean(nchar(unlist(strsplit(text, '\\s+'))))
  return(rev_mean_word_len)
}

rev_unq_word <- function(text) {
  rev_unq_word = length(unique(unlist(strsplit(text, " "))))
  return(rev_unq_word)
}

all_revs_dt[, ":=" (rev_len=nchar(review_text), rev_num_words=lengths(strsplit(review_text, "\\s+")), 
                    rev_mean_word_len= mapply(rev_mean_word_len, review_text), 
                    rev_unq_word     = mapply(rev_unq_word, review_text))]

all_revs_dt[,min(rev_num_words)] #1
all_revs_dt[,mean(rev_num_words)] #128
all_revs_dt[,max(rev_num_words)] #3338
P95 <- function(x){quantile(x,0.95,na.rm=T)}
all_revs_dt[,P95(rev_num_words)] #456
P99 <- function(x){quantile(x,0.99,na.rm=T)}
all_revs_dt[,P99(rev_num_words)] #966

saveRDS(all_revs_dt, file = "02_text_pre_out.rds") #temp save as detection is long process

