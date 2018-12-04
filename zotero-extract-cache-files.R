### Export CSV file (no text, no notes)
### Load csv
#install.packages("readr", "qdapRegex", "tm", "stringr", "stringi", "qdap", "qdapRegex", "tibble", "mallet", "dplyr", "tidytext")
library(readr)
library(qdapRegex)
library(tm)
library(stringr)
library(stringi)
library(qdap)
library(qdapRegex)
library(tibble)
library(mallet)
library(dplyr)
library(tidytext)

mypath <- file.choose()

mydata <- read_csv(mypath)  
shortdata <- mydata[,c("Key","Publication Year","Author","Title","Abstract Note","File Attachments")]

#filter out the records without attachments
shortdata <- shortdata[!(is.na(shortdata$`File Attachments`) | shortdata$`File Attachments`==" "), ]
#coerce all characters into ASCII
shortdata <- as.data.frame(apply(shortdata, c(1, 2), stri_trans_general, "Latin-ASCII"), stringsAsFactors=FALSE)

## loading text
#texts=vector()
id = vector()
lda.format <- vector()  
terms <-list()
for (i in 1:nrow(shortdata)) {
  path = shortdata['File Attachments'][[1]][i]
  dotpath = strsplit(path, "/") # REPLACE by \ for PC - Let's hope there is not title with this symbol
  paths = unlist(dotpath)[length(unlist(dotpath))]
  
  uris.name = gsub(paths,".zotero-ft-cache", path, perl = FALSE, fixed = TRUE)  # replace pdf file name by cache file
  if (file.exists(uris.name)) {
    text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)  
    data=enc2utf8(text.scan)
    loc.a <- grep("References|REFERENCES", data)  # removing last section with references from text
    if (length(data[loc.a])>1){
      text <- data[1:(loc.a[length(loc.a)]-1)]
    }
    else if (length(data[loc.a])=1) {
      text <- data[1:(loc.a-1)]
    }
    else {
      text <- data
    }
    text <- paste(text, collapse = " ")
    text <-gsub("-\\s+", "", text) 
    text.punct <- rm_email(text)
    text.punct <- rm_url(text.punct)
    text.punct <- rm_twitter_url(text.punct)
    text.punct <-rm_citation(text.punct)
    text.punct <-rm_citation(text.punct, pattern="@rm_citation3")
    text.punct <-rm_citation(text.punct, pattern="@rm_citation2")
    text.punct <-rm_round(text.punct)
    text.punct <-rm_curly(text.punct)
    text.punct <-rm_square(text.punct)
    text.punct <- rm_bracket(text.punct)
    text.punct <- strip(text.punct, char.keep="-",digit.remove = TRUE, apostrophe.remove = FALSE,
                        lower.case = TRUE)
    text.punct <- removePunctuation(text.punct,
                                    preserve_intra_word_contractions = TRUE,
                                    preserve_intra_word_dashes = TRUE)
    text.punct <- removeWords(text.punct, stopwords("en"))
    text.punct <- gsub("\\s\\s+"," ",text.punct)
    
    lda.format[i] <- text.punct
    id[i] <- i
    
    d <- data_frame(txt = text.punct)
    freq <- d %>%  unnest_tokens(word, txt)  %>%
      count(word) %>%
      ungroup()
    terms[[i]] <- freq
  }
  else { # cache file does not exist
    lda.format[i] <- 'NA'
    terms[[i]] <-'NA'
    
  }
  
  
  
  titles <- mydata$Title
  abstracts <- mydata$`Abstract Note`
  authors <- mydata$Author
  datetimes <- mydata$`Publication Year`
  
  collection <- list(id = id, titles=titles, abstracts=abstracts, authors=authors,datetimes=datetimes,
                     terms=terms, text = lda.format)
  
  save_collection <- saveRDS(collection, "collection.rds")
