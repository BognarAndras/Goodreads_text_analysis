# Libraries

library(jsonlite)
library(data.table)
library(skimr)

# Load Data, filenames

hist_bio_books_file <- "./goodreads_books_history_biography.json"
hist_bio_reviews_file <- "./goodreads_reviews_history_biography.json"
mist_thrill_books_file <- "./goodreads_books_mystery_thriller_crime.json"
mist_thrill_reviews_file <- "./goodreads_reviews_mystery_thriller_crime.json"


# dt <- data.table(title = NULL , id = NULL)
# for(i in 1:10000) {
#   tmp <- paste(scan(file=hist_bio_books_file, what="complex", sep="\n", nlines=1, skip=(i-1)*1, quiet=TRUE),collapse=" ")
#   dt <- rbindlist(list(dt, list(fromJSON(tmp)$title, fromJSON(tmp)$book_id)))
# }


# Large JSON files, get first  25k-100k results

hist_bio_books_dt <- jsonlite::stream_in(textConnection(readLines(hist_bio_books_file,n=25000)),verbose=F)
hist_bio_reviews_dt <- jsonlite::stream_in(textConnection(readLines(hist_bio_reviews_file,n=100000)),verbose=F)
mist_thrill_books_dt <- jsonlite::stream_in(textConnection(readLines(mist_thrill_books_file,n=25000)),verbose=F)
mist_thrill_reviews_dt <- jsonlite::stream_in(textConnection(readLines(mist_thrill_reviews_file,n=100000)),verbose=F)

# save as data table for performance

hist_bio_books_dt <- data.table(hist_bio_books_dt)
hist_bio_reviews_dt <- data.table(hist_bio_reviews_dt)
mist_thrill_books_dt <- data.table(mist_thrill_books_dt)
mist_thrill_reviews_dt <- data.table(mist_thrill_reviews_dt)

# check variables and select relevant ones

skim(mist_thrill_reviews_dt)

mist_thrill_reviews_dt <- mist_thrill_reviews_dt[rating == 1 |rating == 5 , .(review_id, book_id , rating, review_text, date_added)]
hist_bio_reviews_dt <- hist_bio_reviews_dt[rating == 1 |rating == 5 , .(review_id, book_id , rating, review_text, date_added)]
relevant_books_hist <-  hist_bio_books_dt[hist_bio_books_dt$book_id %in% hist_bio_reviews_dt$book_id , book_id]
relevant_books_thrill <-  mist_thrill_books_dt[mist_thrill_books_dt$book_id %in% mist_thrill_reviews_dt$book_id, book_id]

skim(relevant_books_hist)

hist_bio_books_dt <- hist_bio_books_dt[book_id %in% relevant_books_hist , .(book_id , title ,  text_reviews_count , language_code,
                                                       average_rating, format, popular_shelves)] 

mist_thrill_books_dt <- mist_thrill_books_dt[book_id %in% relevant_books_thrill , .(book_id , title ,  text_reviews_count , language_code,
                                                                            average_rating, format, popular_shelves)]

# save as RDS file for performance

saveRDS(mist_thrill_reviews_dt, file = "mist_thrill_reviews_dt.rds")
saveRDS(hist_bio_reviews_dt, file = "hist_bio_reviews_dt.rds")
saveRDS(hist_bio_books_dt, file = "hist_bio_books_dt.rds")
saveRDS(mist_thrill_books_dt, file = "mist_thrill_books_dt.rds")
