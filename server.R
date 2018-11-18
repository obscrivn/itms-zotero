#options(shiny.reactlog=TRUE)
#command+F3
library(shiny)
source("words.R")
#load("stopwords.txt")
library(plyr)
library(dplyr)
library(ggthemes)
library(graphics)
library(tidyverse)
library(DT)
library(RcppArmadillo)
library(ggplot2)
#library(ggplot2movies)
library(lda)
library(corpus)
library(qdap)
library(qdapRegex)
library(quanteda)
library(reshape2)
library(RTextTools)
library(RColorBrewer)
library(tibble)
library(mallet)
#library(slam)
#library(SnowballC)
#require(stm)
library(stm)
library(stringi)
library(devtools)
library(htmlwidgets)
#devtools::install_github("lchiffon/wordcloud2")
#install.packages("glmnet", repos = "http://cran.us.r-project.org")
#devtools::install_github("hrbrmstr/streamgraph")
#showReactlog
library(streamgraph)
library(stringr)
library(tidyr)
library(tidytext)
library(tm)
library(topicmodels)
library(wordcloud)
#require(wordcloud2)
library(wordcloud2)
library(rJava)

import <- as_tibble(readRDS('collection.rds'))
import2 <- as_tibble(readRDS('collectionNP.rds'))

shinyServer(function(input, output) {

 
output$print_length <- renderUI({
 
 len <-  length(zoteroData()[[1]])

  
  HTML(paste("Corpus Size Total: ", len, sep=" ", collapse="<br/>"))
})

######### zoteroData function #######
zoteroData <- reactive ({
  if(input$corpus =='np-corpus'){ 
    texts <- import2
  }
 #   else if (input$corpus =='russian') {
 #   texts <- russian
 #   }
    else if (input$corpus =='zot') {
      texts <- import
  }
  
  # list of extracted articles with sublist of titles, abstracts, authors, datetimes, text.extracts
  withProgress(message = 'Loading Corpus',
               detail = 'Almost done...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  return(texts)
})

output$zotero_term <-renderUI ({
  list(textInput('zotero_term', "Enter one query, or Enter two queries separated by space", placeholder = "major cost"),
  actionButton("zotero_submit", "Submit") )
})

#output$zotero_one_term <-renderUI ({
#  list(textInput('zotero_one_term', "Enter one query", placeholder = "russia"),
 #      actionButton("zotero_submit1", "Submit") )
#})



zotero_keywords_submitted <- eventReactive(input$zotero_submit, {
  input$zotero_term
})
#zotero_keywords_one_submitted <- eventReactive(input$zotero_submit1, {
#  input$zotero_one_term
#})

output$choose_kwic_num <- renderUI({
  selectizeInput("choose_kwic_num", label = "Select or Type number of words between two terms if your condition is AND", 
                 choices = c(2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

#### Sliders ####
output$zotero_slider <- renderUI({
  sliderInput("zotero_slider", "Window Length (left and right context):",
              min = 0, max = 100, value = 5)
})
output$ngram_slider <- renderUI({
  sliderInput("ngram_slider", "Ngram Filter",
              min = 0, max = 1000, value = 70)
})
#######extractZoteroTerm function ####
extractZoteroTerm <- reactive ({
 
  if (!is.null(zotero_keywords_submitted())){
    query <- gsub("\\s+", " ", tolower(zotero_keywords_submitted()))
    if (length(unlist(strsplit(query," "))) > 1) {
    query1  <- unlist(strsplit(query," "))[1]
    query2 <- unlist(strsplit(query," "))[2]

    #condition <- unlist(strsplit(query," "))[2]
  len <- as.integer(input$zotero_slider)
  between <- as.integer(input$choose_kwic_num)
  withProgress(message = 'Extracting Two Terms',
               detail = 'Might take a while...', value = 0, {
                 for (i in 1:35) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  lines <- list()
  text.extract <- list()
  w = 1
  titles <- vector()
  authors <- vector()
  datetimes <- vector()
  abstracts <- vector()
  ids <- vector()
 # contents <- list()
  texts <- vector()
  terms <- list()
  lines.merge = NULL
  num <- length(zoteroData()$titles)
  for (i in 1:num) {
    content <- zoteroData()$text[i]
    lda.list <- unlist(strsplit(as.character(content), "\\s+"))
   # lda.list <- strsplit(as.character(content), "\\s+")
    loc1 <- grep(query1, lda.list,perl=TRUE)
    loc2 <- grep(query2, lda.list,perl=TRUE)
    if ((length(loc1) > 0) & length(loc2)>0) {
      ### choose the smallest
      z=0
      # ### Add between window
      for (k in 1:length(loc1)) {
        strings <- lda.list[(loc1[k]):(loc1[k]+between)]
        if (query2 %in% strings) {
          z=z+1
          ### add left and right context
          if ((loc1[k]-between-len)<1 ){
            match.string <- lda.list[(loc1[k]):(loc1[k]+between+len)]
          }
          else if ((loc1[k]+between+len)>loc1[length(loc1)]){
            match.string <- lda.list[(loc1[k]-between-len):(loc1[k])]
          }
          else {
            match.string <- lda.list[(loc1[k]-between-len):(loc1[k]+between+len)]
          }
          line <- paste(match.string, collapse=" ")
          line <- gsub("\\s\\s+"," ",line)
          lines[[z]] <- line
           lines.merge <- paste(unlist(lines), collapse=" ")
        }
        #else {
         # lines.merge = NULL}
      }
      if (!is.null(lines.merge)) {
        title <- zoteroData()$titles[i]
        datetime <- zoteroData()$datetimes[i]
        abstract <- zoteroData()$abstracts[i]
        text <- zoteroData()$text[i]
        term <- zoteroData()$terms[i]
        id <- zoteroData()$id[i]
        name <- zoteroData()$authors[i]#zoteroData()$text.extract[[1]][i]
        text.extract[[w]] <- lines.merge
        titles[w] <- title
        authors[w] <- name
        datetimes[w] <- datetime
        abstracts[w] <- abstract
        ids[w] <- id
       # contents[w] <- text
        terms[w] <- term
        texts[w] <- text
        w=w+1
      }
    }
  }
  }
 # else if (!is.null(zotero_keywords_one_submitted())){
    else {
    query <- gsub("\\s+", "", tolower(zotero_keywords_submitted()))
    len <- as.integer(input$zotero_slider)
    withProgress(message = 'Extracting One Term',
                 detail = 'Might take a while...', value = 0, {
                   for (i in 1:25) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    lines <- list()
    text.extract <- list()
    w = 1
    titles <- vector()
    authors <- vector()
    datetimes <- vector()
    abstracts <- vector()
    ids <- vector()
   # contents <- list()
    texts <- vector()
    terms <- list()
    lines.merge = NULL
    num <- length(zoteroData()$titles)
    for (i in 1:num) {
      content <- zoteroData()$text[i]
      lda.list <- unlist(strsplit(as.character(content), "\\s+"))
      loc1 <- grep(query, lda.list,perl=TRUE)
      if (length(loc1) > 0){
        ### choose the smallest
        z=0
        # ### Add between window
        for (k in 1:length(loc1)) {
          z=z+1
          ### add left and right context
          if ((loc1[k]-len)<1 ){
            match.string <- lda.list[(loc1[k]):(loc1[k]+len)]
          }
          else if ((loc1[k]+len)>loc1[length(loc1)]){
            match.string <- lda.list[(loc1[k]-len):(loc1[k])]
          }
          else {
            match.string <- lda.list[(loc1[k]-len):(loc1[k]+len)]
          }
          line <- paste(match.string, collapse=" ")
          line <- gsub("\\s\\s+"," ",line)
          lines[[z]] <- line
          lines.merge <- paste(unlist(lines), collapse=" ")
        }
        #else {
        # lines.merge = NULL}
      }
     
      if (!is.null(lines.merge)) {
        title <- zoteroData()$titles[i]
        datetime <- zoteroData()$datetimes[i]
        abstract <- zoteroData()$abstracts[i]
        text <- zoteroData()$text[i]
        term <- zoteroData()$terms[i]
        name <- zoteroData()$authors[i]
        id <- zoteroData()$id[i]
        text.extract[[w]] <- lines.merge
        titles[w] <- title
        authors[w] <- name
        datetimes[w] <- datetime
        abstracts[w] <- abstract
        ids[w] <- id
        terms[w] <- term
        texts[w] <- text
        w=w+1
      }
    }
    }
  }
  text.extract <- unlist(text.extract) 
  extract <- list(texts=texts,text.extract=text.extract,titles=titles,datetimes=datetimes, authors=authors, abstracts=abstracts, terms=terms, ids=ids)
  return(extract) # list of 5: 1.texts (processed), 2.text.extract, 3.titles, 4.datetimes,5.authors, 6.abstracts, 7. terms - word and freq
})


########print_content_rdf ######
output$print_content_rdf <- renderUI({
  if ((is.null(input$zotero_submit))) { return() }
  #txt.lines <- zoteroData()$preprocessingSteps$lda.format
 txt.lines <- extractZoteroTerm()[[3]]#zoteroData()$preprocessingSteps$lda.format[[1]]
 # data <- extractZoteroTerm()[[4]]
 # df = as.data.frame(table(data))
 # colnames(df) = c("Years","Count")
  # txt.lines <- extractZoteroTerm()[[3]]#$titles#zoteroData()$dir.list #directory()
 #data
   HTML(paste("<br/>", "Num of Extracted documents: ",length(txt.lines), sep="<br/>"))
}) 
##########zotero_content function ########
zotero_content <- reactive({
  if ((is.null(input$zotero_submit))) { return() }
 # txt.lines <- zoteroData()$preprocessingSteps$lda.format[[1]]
  txt.lines <-extractZoteroTerm()$text.extract#extractZoteroTerm()[[3]]#zoteroData()$preprocessingSteps$lda.format[[1]]
    #extractZoteroTerm()[[1]]#$texts
   # zoteroData()[[1]]
  #extractZoteroTerm()$text.extract# zoteroData()$texts[[5]]#extractZoteroTerm()
  return(txt.lines)
})
## Test ####
output$matrix <- renderUI({
  stmAnalysis()
})
  
### print zotero viewer ####
output$print_zotero <- renderUI({
  if ((is.null(input$zotero_submit))) { return() }
  txt.titles <- extractZoteroTerm()$titles
  txt.lines <- unlist(extractZoteroTerm()$text.extract) #zotero_content() #extractZoteroTerm()#zotero_content()
  #txt.lines <- zoteroData()[[2]]
  #content <- zoteroData()$preprocessingSteps$lda.format[[i]]
  #txt.lines <- unlist(strsplit(as.character(content), "\\s+"))
 # txt.lines <- zoteroData()$titles
  
 # HTML(paste("<br/>", "Document: ",txt.lines, sep="<br/>"))
  HTML(paste("<br/>", 'Title:', txt.titles, 'Extract:', txt.lines, sep="<br/>"))
})

output$choose_length <- renderUI({
  selectizeInput("len",
                 "Context Length:",
                 choices = c(1,2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
})

####### ListTerms function #####
ListTerms <- reactive({
 
    corpus.lda <- extractZoteroTerm()[[2]]#[[1]]#[[2]]

 clusters <- listTerms(corpus.lda)
 info <- list(d = clusters$d, corpus.lda = clusters$corpus.lda, 
              len = clusters$len, clean = clusters$d.clean)
return(info)
 })  

output$term_print <- renderUI ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.rdf)) && (is.null(input$file.tag))) { return() }
  len <- input$len
  term <- input$choose_term
  HTML(paste("Your Term: ", term, "Your Context Length: ", len,  sep=" ", collapse="\n"))
})
######Abstract function ###########
# Abstract <- reactive ({
#   if (is.null(input$file.article))  { return() } 
#   if (input$article_content=="Abstract") {
#     extractAbstract(x=ExtractRawContentPDF(), y=input$file.article)
#   }
# })



## Reading Metadata File from csv
######### fileData metafunction #########
 fileData <- reactive({ # loading data
   a <- extractZoteroTerm()$authors#[[4]]#$authors
   t <- extractZoteroTerm()$titles#[[2]]#$titles
   dt <- extractZoteroTerm()$datetimes#[[3]]#$datetimes
   
   my_data <- data.frame(date=dt, title=t, author = a)

# 
   return(my_data)
 })
#########zotero_metadata_table #######
output$zotero_metadata_table <- renderDataTable({
   a <- extractZoteroTerm()$authors#[[4]]#$authors
   t <- extractZoteroTerm()$titles#[[2]]#$titles
   dt <- extractZoteroTerm()$datetimes#[[3]]#$datetimes

  my_data <- data.frame(date=dt, title=t, author = a)
my_data

})
### Display Metadata from CSV


output$print_preprocessed <- renderUI({
  # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&& (is.null(input$file.rdf))) { return() }
  # if (input$preprocessing=="No Changes") {
  #   pdf.lines <- "No pre-processing steps are applied"
  # }
 # else if (input$preprocessing=="Apply Steps") {
    # withProgress(message = 'Preprocessing',
    #              detail = 'Almost done...', value = 0, {
    #                for (i in 1:60) {
    #                  incProgress(1/15)
    #                  Sys.sleep(0.25)
    #                }
    #              })
    pdf.lines <- extractZoteroTerm[[2]]#[[1]]#PreprocessingSteps()[[3]]#$lda.format#PreprocessingSteps()[6]#$lda.format#window.one()$lda.format
     pdf.lines <- unlist(pdf.lines)
 # }
  HTML(paste("<br/>", "Document: ",pdf.lines, sep="<br/>"))
}) 

##### STOP WORDS 
#############stopWordsTxt function ######
# stopWordsTxt <- reactive ({
#   stop_words<-vector()
#   if (input$stops=="english") {
#     stop_words <- stopwords("SMART")
#   }
#   if (input$stops=="russian"){
#     stop_words <- stopwords("russian")
#   }
#   if (input$stops=="Upload") {
#     uris.name <- input$stopwords.txt$datapath
#     text.scan <- scan(uris.name, what="character", sep="\n",blank.lines.skip = FALSE)
#     x=enc2utf8(text.scan)
#     text.punct <- str_c(x)#text.punct)
#     text.punct<- str_trim(text.punct)
#     text.punct <- gsub("\\s\\s+", " ", text.punct)   
#     stops<-strsplit(text.punct, " ")
#     stop_words<-unlist(stops)
#   }
#   return(stop_words)
# })
# 
# output$print_stopwords <- renderPrint({
#   if (input$stops=="None") {"Stopwords are not selected"}
#   #else{ 
#    # stopWordsTxt()}
# })
#########RemoveWordsStepOne #########
#Allows for interactive instant word removals from user
# RemoveWordsStepOne <- reactive({
#  # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag)) &&(is.null(input$file.rdf))) { return() }
#   #cutoff.lower=0#input$cutoff_lower
#   #cutoff.high=input$cutoff_high
#   mycorpus <- unlist(extractZoteroTerm()[[2]])
#   # mycorpus <- unlist(zoteroData()$preprocessingSteps$lda.format)#PreprocessingSteps()[[3]])#$lda.format#[[3]]#[[5]]
#   if  (input$stops=="None") {    
#     doc.vect <- VectorSource(mycorpus)
#     corpus.tm <-VCorpus(doc.vect)
#     corpus.tm <- tm_map(corpus.tm, stripWhitespace)
#     corpus.tm <- tm_map(corpus.tm, removePunctuation)
#     corpus <- list()
#     for (i in 1:length(corpus.tm)) {
#       doc <-corpus.tm[[i]]$content
#       corpus[[i]] <- doc
#     }
#     lda.corpus <- corpus
#     # corpus <- mycorpus
#     corpus <- unlist(corpus)
#     corpus.paste <-paste(mycorpus, sep=" ")
#     corpus.paste <-paste(corpus, sep=" ")
#     corpus.paste <- str_c(corpus.paste)
#     corpus.paste<- str_trim(corpus.paste)
#     corpus.paste <- gsub("[^[:alnum:] ]", "", corpus.paste) 
#     corpus.list <- strsplit(corpus.paste, "\\s+")
#     terms <- table(unlist(corpus.list))
#     terms.sorted <- sort(terms, decreasing = TRUE)
#     terms.matrix<-as.matrix(terms.sorted)
#     d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
#     d$word <- row.names(d)
#     agg_freq <- aggregate(frequency ~ word, data = d, sum)
#     #d <- d[order(d$frequency, decreasing = T), ]
#     d <- d[order(d$word), ]
#     # words.list <- as.list(d$word)
#     
#   }
#   else if  ((input$stops=="english")||(input$stops=="Upload") || (input$stops=="russian")) {
#     remove_word <- stopWordsTxt()
#     doc.vect <- VectorSource(mycorpus)
#     corpus.tm <-Corpus(doc.vect)
#     corpus.tm <- tm_map(corpus.tm,removeWords,stopWordsTxt())
#     corpus.tm <- tm_map(corpus.tm, stripWhitespace)
#     corpus.tm <- tm_map(corpus.tm, removePunctuation)
#     corpus <- list()
#     for (i in 1:length(corpus.tm)) {
#       doc <-corpus.tm[[i]]$content
#       corpus[[i]] <- doc
#     }
#     lda.corpus <- corpus
#     corpus <-unlist(corpus)
#     # corpus.paste <-paste(mycorpus, sep=" ")
#     corpus.paste <-paste(corpus, sep=" ")
#     corpus.paste <- str_c(corpus.paste)
#     corpus.paste<- str_trim(corpus.paste)
#     corpus.list <- strsplit(corpus.paste, "\\s+")
#     terms <- table(unlist(corpus.list))
#     remove_word <- stopWordsTxt()
#     del <- names(terms) %in% remove_word #| terms < cutoff.lower
#     terms <- terms[!del]
#     terms.sorted <- sort(terms, decreasing = TRUE)
#     terms.matrix<-as.matrix(terms.sorted)
#     d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
#     d$word <- row.names(d)
#     agg_freq <- aggregate(frequency ~ word, data = d, sum)
#    # d <- d[order(d$frequency, decreasing = T), ]
#     d <- d[order(d$word), ]
#     # words.list <- as.list(d$word)
#   }
#   info <- list(corpus=corpus,d=d,lda.corpus=lda.corpus)
#   return(info)
# })
# #######RemoveWordsStepTwo########
# RemoveWordsStepTwo <-reactive({
#  # if ((is.null(input$file.article)) & (is.null(input$file.article.txt)) & (is.null(structured_data()))& (is.null(input$file.tag))&(is.null(input$file.rdf))) { return() }
#   if (is.null(input$remove_words)) {
#     corpus <-RemoveWordsStepOne()$corpus
#     lda.corpus<-RemoveWordsStepOne()$lda.corpus
#     d <-RemoveWordsStepOne()$d
#     # words.list <-RemoveWordsStepOne()$words.list
#   }
#   else {
#     mycorpus <-RemoveWordsStepOne()$corpus
#     doc.vect <- VectorSource(mycorpus)
#     corpus.tm <-VCorpus(doc.vect)
#     # corpus.tm <- removeWords(corpus.tm, c(input$remove_words))
#     corpus.tm <- tm_map(corpus.tm,removeWords,c(input$remove_words))
#     corpus.tm <- tm_map(corpus.tm, stripWhitespace)
#     corpus <- list()
#     for (i in 1:length(corpus.tm)) {
#       doc <-corpus.tm[[i]]$content
#       corpus[[i]] <- doc
#     }
#     # corpus <-unlist(corpus)
#     corpus.paste <- paste(corpus, sep=" ")
#     corpus.paste <- str_c(corpus.paste)
#     corpus.paste <- str_trim(corpus.paste)
#     corpus.paste <- gsub("[^[:alnum:] ]", " ", corpus.paste) 
#     corpus.list <- strsplit(corpus.paste, "\\s+")
#     terms <- table(unlist(corpus.list))
#     terms.sorted <- sort(terms, decreasing = TRUE)
#     # remove_word <- stopWordsTxt()
#     # del <- names(terms) %in% remove_word #| terms < cutoff.lower
#     # terms <- terms[!del]
#     terms.matrix<-as.matrix(terms)
#     d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
#     d$word <- row.names(d)
#     agg_freq <- aggregate(frequency ~ word, data = d, sum)
#     d <- d[order(d$frequency, decreasing = T), ] 
#     #words.list <- as.list(d$word)
#   }
#   info <- list(corpus=corpus,d=d)#,words.list=words.list)
#   return(info)
# })
######RemoveWordsStepThree #########
  RemoveWordsStepThree <-reactive({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  corpus <- extractZoteroTerm()$text.extract#stemming()
  doc.vect <- VectorSource(corpus)
  docs <-VCorpus(doc.vect)
  tdm <- TermDocumentMatrix(docs)
  tdm <- removeSparseTerms(tdm,0.99)
  dtm <- DocumentTermMatrix(docs)
  term.matrix <- as.matrix(tdm)
    file.names <- extractZoteroTerm()$ids#zoteroData()$texts[[1]]#extractZoteroTerm()$titles
 # }
  colnames(term.matrix) <- file.names
  corpus.paste <-paste(corpus, sep=" ")
  corpus.paste <- str_c(corpus.paste)
  corpus.paste<- str_trim(corpus.paste)
  corpus.list <- strsplit(corpus.paste, "\\s+")
  terms <- table(unlist(corpus.list))
  terms.sorted <- sort(terms, decreasing = TRUE)
  terms.matrix<-as.matrix(terms)
  d <- data.frame(frequency = sort(rowSums(terms.matrix), decreasing = TRUE))
  d$word <- row.names(d)
  agg_freq <- aggregate(frequency ~ word, data = d, sum)
  d <- d[order(d$frequency, decreasing = T), ]
  info <- list(d=d,corpus=corpus,tdm=tdm,terms.matrix=terms.matrix,dtm=dtm, term.matrix=term.matrix)
  return(info)
})

output$print_apply_stops <- renderUI({
  if (input$stopwords=="None") {"No changes are made. You need to select stopwords first"}
  if (input$stopwords=="Apply Stopwords") {
    HTML(paste0(RemoveWordsStepThree()$corpus))
  }
})
######### stemming#######
# stemming <- reactive ({
#  # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
#  # if(input$stopwords=="None") {
#  #   corpus <-RemoveWordsStepOne()$corpus
#     # d <-RemoveWordsStepOne()$d
#  # }
# #  else if(input$stopwords=="Apply Stopwords") {
#  #   corpus <-RemoveWordsStepTwo()$corpus
#     #   d <-RemoveWordsStepTwo()$d
# #  }
#   
#   if (input$language=="none") {
#    # corpus <-RemoveWordsStepTwo()$corpus
#     corpus <- extractZoteroTerm()[[2]]
#     # text.punct="Select language"
#   }
#   else if ((input$language=="russian") || (input$language=="english")) {
#     corpus <- extractZoteroTerm()[[2]]
#     doc.vect <- VectorSource(corpus)
#     docs <-VCorpus(doc.vect)
#     corpus <- list()
#     for (i in 1:length(docs)) {
#       doc <-docs[[i]]$content
#       text.split <- unlist(strsplit(doc, " "))
#       text.stem <- paste(wordStem(text.split, language = input$language),collapse = " ")
#       #t <- text_tokens(text.split, stemmer = "ru")
#      # text.stem <- paste(unlist(t) ,collapse = " ")
#      # text.stem <- paste(wordStem(text.split, language = input$language),collapse = " ")
#       text.stem <- str_c(text.stem)
#       text.stem<- str_trim(text.stem)
#       corpus[[i]] <- text.stem
#     }
#     corpus <-unlist(corpus)
#   }
#   return(corpus)
# })

#output$print_stemmer <- renderUI({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  # if (is.null(input$remove_manual)) { return() }
 # HTML(paste0(stemming()))
#})
#########rawfrequency###########
rawFrequency <- reactive ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if(!is.null(input$file.article)) {
 #   x <-ExtractRawContentPDF()
 # }
 # else if(!is.null(input$file.article.txt)) {
 #   x<- ExtractRawContentTXT()
 # }
#  else if(!is.null(input$file.tag)) {
#    x <-selectPos()
#  }
 # else if(!is.null(input$file.rdf)) {
    x<-zotero_content()#extractZoteroTerm()$text.extract
 # }
 # else if(!is.null(is.null(structured_data()))) {
 #   x <-structured_data()$corpus
 # }
  d <-frequencyTable(x)
  return(d)
})

output$freq <- renderDataTable ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  d<-rawFrequency()$dataf#ListTerms()$d#rawFrequency()$dataf
  return(d)#,options=list(lengthMenu = c(5, 10, 15), pageLength = 5))
})

output$zipf <- renderPlot ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  tdm <- ListTerms()$tdm
  dtm <- ListTerms()$dtm
  p<- Zipf_plot(dtm, type="l")
  return(p)
})

#heaps not working
#output$heaps <- renderPlot ({
# if ((is.null(input$file.article)) && (is.null(input$file.article.txt))) { return() }
#if((is.null(input$show_freq)) || (input$show_freq=="NULL")){ return() }
# if (input$show_freq=="Frequency") {
# dtm <-ListTerms()$dtm  #RemoveWords()$tdm
#})


output$choose_text <- renderUI({
#  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if (!is.null(input$file.article)) {
 #   names <- input$file.article$name
 # }
 # if (!is.null(input$file.article.txt)) {
 #   names <- input$file.article.txt$name
 # }
#  if (!is.null(input$file.tag)) {
 #   names <-input$file.tag$name
 # }
#  if (!is.null(structured_data())) {
#  names <- structured_data()$titles
#  }
  #if (!is.null(input$file.rdf)) {
    names <- extractZoteroTerm()$titles
 # }
  selectizeInput("show_text", label = "Select which document to analyze", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE) 
  
})



#########ExtractRawContentTXT#######
# ExtractRawContentTXT <- reactive ({
#   if (is.null(input$file.article.txt)) { return() }
#   extractContentTxt(input$file.article.txt)
# })

output$choose_top <- renderUI({
  selectizeInput("top", label = "Select a number for top frequent words (ex. 10 top frequent words)", 
                 choices = c(10,20,30,40,50,60,70,80,90,100),
                 options = list(create = TRUE),
                 selected=10,
                 multiple = FALSE) 
}) 

output$choose_remove <- renderUI({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data())) && (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  word <- RemoveWordsStepThree()$d[[2]]
  selectizeInput("remove_words", label = "Select words to be removed", 
                 choices = word,
                 options = list(create = TRUE),
                 selected = NULL,
                 multiple = TRUE) 
  # }
})

output$printWords <- renderUI({
  #if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  # if (is.null(input$remove_manual)) { return() }
  HTML(paste0(RemoveWordsStepThree()$d[[2]]))
  # HTML(paste0(RemoveWordsStepTwo()$words.list))#PreprocessingSteps()$lda.format))# RemoveWordsNew()$corpus.lda))
})

output$word_count <- renderPlot({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  x <- input$top
  #  word <- RemoveWordsNew()$d[[2]][1:x]
  #  frequency <- RemoveWordsNew()$d[[1]][1:x]
  word <-  RemoveWordsStepThree()$d[[2]][1:x]
  # word <- RemoveWordsFinal()$d[[2]][1:x]
  frequency <- RemoveWordsStepThree()$d[[1]][1:x]
  plot <-barplot(frequency, names.arg=word, las=2,cex.names=0.8)
  return(plot)
})

output$choose_min_frequency <- renderUI({
  selectizeInput("min",
                 "Minimum Frequency:",
                 choices = c(1,2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=1,
                 multiple = FALSE) 
})

output$choose_max_words <- renderUI({
  selectizeInput("max",
                 "Maximum Words per Plot:",
                 choices = c(100,150,200),
                 options = list(create = TRUE),
                 selected=150,
                 multiple = FALSE) 
})
#########print_cloud########
output$print_cloud2 <-renderWordcloud2({
  #if ((input$multicloud=="Word Cloud"))
 # {
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&& (is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # wordcloud_rep <- repeatable(wordcloud)
  data = data.frame(RemoveWordsStepThree()$d$word, RemoveWordsStepThree()$d$freq)
  font <- input$font
  
  if (input$pal=="black"){
    pal="black"
  } else if (input$pal=="green"){
    pal <- brewer.pal(9, "BuGn")
    pal <- pal[-(1:2)]
  } else if (input$pal=="multi") {
    
    pal <- brewer.pal(8,"Dark2")
  }
 # if (input$multicloud=="Word Cloud") {
    wordcloud2::wordcloud2(data, minSize = 0,
               fontFamily=font,
               color=pal)
 # }
 # }else
 # { return() }
})
  


  
output$print_cloud <-renderPlot({
  wordcloud_rep <- repeatable(wordcloud)
  font <- input$font
  par(mar = rep(0, 4))
  if (input$pal=="black"){
    pal="black"
  } else if (input$pal=="green"){
    pal <- brewer.pal(9, "BuGn")
    pal <- pal[-(1:2)]
  } else if (input$pal=="multi") {
    
    pal <- brewer.pal(8,"Dark2")
  }
if (input$multicloud=="Commonality Cloud") {
    d = RemoveWordsStepThree()$term.matrix
    commonality.cloud(d, random.order=FALSE, #max.words=input$max,
                      ordered.colors=F,#ordered.colors=T,
                      rot.per=.15,#c(8,0.3),
                      #scale=c(5, .2),
                      scale=c(4, .5),
                        #min.freq = input$min,# 
                      #vfont=c("sans serif","plain"),
                      #vfont=c("script","plain"),
                      vfont=c(font,"plain"),
                      #  random.order=FALSE,
                      #  max.words=input$max,#100,#input$freq, max.words=input$max,
                      #colors=brewer.pal(6, "Dark2"))
                      colors=pal )#"black")
  }
  else if (input$multicloud=="Comparison Cloud") {
    d <- RemoveWordsStepThree()$term.matrix
    #d = data.frame(RemoveWordsStepThree()$d$freq,RemoveWordsStepThree()$d$word)
    comparison.cloud(d,random.order=FALSE, #max.words=input$max,
                     title.size=1.3,
                     scale=c(4,0.5))
  }
  # wordcloud(d$word, d$freq)
  #  max.words =100,min.freq=3,scale=c(4,.5), 
  # random.order = FALSE,rot.per=.5,vfont=c("sans serif","plain"),colors=palette())
})



##### LDA Analysis
output$choose_topic_num <- renderUI({
  selectizeInput("num", label = "Select or Type Number of Topics", 
                 choices = c(2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

output$choose_word_num <- renderUI({
  selectizeInput("word", label = "Select or Type Number of Words per Topic", 
                 choices = c(1,2,3,4,5,6,7,8,9,10),
                 options = list(create = TRUE),
                 selected=3,
                 multiple = FALSE) 
}) 

output$iter <- renderUI({
  names <- c(500,1000)
  selectizeInput("iter", label = "Select or Type a number for iterations", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=500,
                 multiple = FALSE) 
}) 
output$alpha <- renderUI({
  names <- c(0.01,0.02,0.05,0.1)
  selectizeInput("alpha", label = "Select or Type a number for apha", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=0.02,
                 multiple = FALSE) 
}) 
output$eta <- renderUI({
  names <- c(0.01,0.02,0.05,0.1)
  selectizeInput("eta", label = "Select or Type a number for eta", 
                 choices = names,
                 options = list(create = TRUE),
                 selected=0.02,
                 multiple = FALSE) 
}) 

#######chronology########
chronology <- reactive ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  #if( input$metadata_source == "None") {return()}
 # if((is.null(input$chronology)) ||(input$chronology=="None")) { return() }
  #remove.words.file <- stopWordsTxt()
  corpus.lda <-  RemoveWordsStepThree()$corpus
  dates<-  fileData()$date

    #corpus.lda <-  RemoveWordsStepOne()$corpus
 # corpus.lda <- removeWords(corpus.lda, remove.words.file)
 # corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
 # corpus.lda <- gsub("\\s+"," ",corpus.lda)
#  corpus.lda <- str_c(corpus.lda)
#  corpus.lda<- str_trim(corpus.lda)
  set.seed(2013)
  my.corpus <- VCorpus(VectorSource(corpus.lda))
  # my.corpus <- tm_map(my.corpus,removeWords,stopwords("english"))
 # language=input$language
  dtm <- DocumentTermMatrix(my.corpus)
  # if (!is.null(input$metadata_pdf)) {
  #dates<-  fileData()$date
  # dates <- metadataPdf()$datetimes
  # }
  #if (!is.null(input$metadata_csv)) {
  #  dates <- newData()[[2]]
  # }
  for (i in 1:length(my.corpus)){
    meta(my.corpus[[i]], tag = "datetimestamp") <- dates[i]
  }
  dtm <- DocumentTermMatrix(my.corpus)
  n.topics <- as.numeric(input$num)
  lda.model <- LDA(dtm, n.topics,method="Gibbs")
  n.words <- as.numeric(input$word)
  term <- terms(lda.model,n.words)  
  df <- data.frame(id=names(topics(lda.model)), 
                   date=unlist(meta(my.corpus, type="local",tag="datetimestamp"),as.character)#, "%Y-%m-%d %H:%M:%S"))))#2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"   
  )

 # dfw <- cbind(df,posterior(lda.model)$topics)
  dft <- cbind(df,posterior(lda.model)$topics)
  # dft2 < cbind(df,term)
  M <-melt(dft,id.vars=c("id","date")) 
 # M2 <-melt(dft,id.vars="date")
  l<-length(my.corpus)
  info <- list(dft=dft, term=term,lda.model=lda.model,M=M,l=l)#,datetimestamp=datetimestamp)
  return(info)
})

output$chronology_top <- renderPrint ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((input$chronology=="None") || (is.null(input$chronology))) { return() }
  chronology()$term
 # chronology$dft
})
output$chronology_plot <- renderPlot ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
#  if((input$chronology=="None") || (is.null(input$chronology))) { return() }
  dft<-chronology()$M
  #  terms <- chronology()$term
  g <- ggplot(dft,aes(x= dft[,2],y=dft[,4],color=dft[,3]))+xlab("Time Period") + ylab("Posterior") + geom_point(aes(size = dft[,4]))+ geom_density2d(alpha=.2) #+ geom_text(aes(label=terms))
  # p <- MyPlot(M[2:3],grouped.by=M[1])
  # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line() xlab("Time Period") + ylab("Posterior")  +
  return(g)
})
output$chronology_table <- renderTable ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((input$chronology=="NULL") || (is.null(input$chronology))) { return() }
  dft<-chronology()$M
  # g <- ggplot(dft,aes(x= dft[,2],y=dft[,3]),color=dft[,1])+geom_point()  + geom_density2d(alpha=.2)
  # p <- MyPlot(M[2:3],grouped.by=M[1])
  # p<-  ggplot(data=M, aes(y=value, x=date, color=variable))+geom_point() + geom_line()
  return(dft)
})
#######BestK#######
BestK <- reactive ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
 # remove.words.file <- stopWordsTxt()
  cutoff.lower=0#input$cutoff_lower
  # cutoff.high=input$cutoff_high
 # if(!is.null(input$file.article)) {
    novel.vector <- PreprocessingSteps()[[3]]#[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
    #  novel.vector <- ExtractContentPDF()$lda.format
    # num.documents <- length(ExtractContentPDF()$lda.format)
    num.documents <- length(PreprocessingSteps()[[3]])#[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
    file.names <- extractZoteroTerm()[[1]]#input$file.article$name  
 # }
 # if(!is.null(input$file.article.txt)) {
  #  novel.vector <- PreprocessingSteps()[[3]]#[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
    # novel.vector <- ExtractContentTXT()$lda.format #novel.list
  #  num.documents <- length(PreprocessingSteps()[[3]])#[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
  #  file.names <- input$file.article.txt$name  
 # }
 # if(!is.null(input$file.tag)) {
 #   novel.vector <- PreprocessingSteps()[[3]]#[6]#PreprocessingSteps()$lda.format#window.one()$lda.format 
    # novel.vector <- ExtractContentTXT()$lda.format #novel.list
 #   num.documents <- length(PreprocessingSteps()[[3]])#[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
 #   file.names <- input$file.tag$name  
 # }
 # novel.vector <- removeWords(novel.vector, remove.words.file)
 # novel.vector <- removeWords(novel.vector, c(input$remove_words))
  pdf.corpus <- lexicalize(novel.vector, lower=TRUE)
  if (input$language=="None") {
    pdf.corpus$vocab <- pdf.corpus$vocab
  }
  else {
    pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language = input$language)
  }
  wc <- word.counts(pdf.corpus$documents)
  to.remove <- as.numeric(names(wc)[wc<=cutoff.lower])
  pdf.corpus$documents <- filter.words(pdf.corpus$documents , to.remove)
  if (!is.null(input$metadata_pdf)) {
    file.names <- metadataPdf()$names
  }
  if (!is.null(input$metadata_csv)) {
    file.names <- metadataPdf()$names
  }
  matrix <- create_matrix(cbind(as.vector(file.names),as.vector(novel.vector)), 
                          language="english", removeNumbers=TRUE, stemWords=TRUE, weighting=weightTf)
  best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(matrix, d)})
  best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
  best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
  
  p <- ggplot(best.model.logLik.df, aes(x=topics, y=LL)) +   
    xlab("Number of topics") + ylab("Log likelihood of the model") +   
    geom_line() +   theme_bw()
  
  k <- best.model.logLik.df[which.max(best.model.logLik.df$LL),]
  info <- list(p=p,k=k,best.model.logLik=best.model.logLik,best.model=best.model)
  return(info)
})
output$best_k <- renderTable ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Calculation in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:35) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  best.model <- BestK()$best.model
  best.model.logLik<-BestK()$best.model.logLik
  best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
  best.model.logLik.df[which.max(best.model.logLik.df$LL),]
})
output$best_k_plot <- renderPlot ({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$best_num)) || (input$best_num=="NULL")) { return() }
  withProgress(message = 'Plotting in progress',
               detail = 'This may take a while...', value = 0, {
                 for (i in 1:35) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  BestK()$p
})
#########LdaAnalysis########
LdaAnalysis <- reactive({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  set.seed(2013)
 # remove.words.file <- stopWordsTxt()
  cutoff.lower=0#input$cutoff_lower
  #cutoff.high=input$cutoff_high
 # if(!is.null(input$file.article)) {
 #   corpus.lda <-  RemoveWordsStepOne()$corpus #PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
    #   num.documents <- length(RemoveWordsStepOne()$lda.corpus)#PreprocessingSteps()[6])#PreprocessingSteps()$lda.format)#window.one()$lda.format)
 #   n.docs <- as.numeric(length(input$file.article$name))  
 # }
 # if(!is.null(input$file.article.txt)) {
  #  corpus.lda <-  RemoveWordsStepOne()$corpus
    # num.documents <- length(RemoveWordsStepOne()$lda.corpus)
    #corpus.lda <-  window.one()$lda.format
    # num.documents <- length(window.one()$lda.format)
 #   n.docs <- as.numeric(length(input$file.article.txt$name))
 # }
 # if(!is.null(input$file.tag)) {
 #   corpus.lda <-  RemoveWordsStepOne()$corpus
    # num.documents <- length(RemoveWordsStepOne()$lda.corpus)
    #corpus.lda <-  window.one()$lda.format
    # num.documents <- length(window.one()$lda.format)
 #   n.docs <- as.numeric(length(input$file.tag$name))
 # }
 # if(!is.null(input$file.rdf)) {
   # corpus.lda <-  RemoveWordsStepOne()$corpus
  corpus.lda <-  RemoveWordsStepThree()$corpus
    n.docs <- as.numeric(length(extractZoteroTerm()$titles))#zoteroData()$texts[[1]]))#extractZoteroTerm()$titles))
  #}
 # if(!is.null(structured_data())) {
 #   corpus.lda <-  RemoveWordsStepOne()$corpus
 #   n.docs <- as.numeric(length(structured_data()$titles))
 # }
 # corpus.lda <- removeWords(corpus.lda, remove.words.file)
 # corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
 # corpus.lda <- gsub("\\s+"," ",corpus.lda)
 # corpus.lda <- str_c(corpus.lda)
 # corpus.lda<- str_trim(corpus.lda)
  # empty.string <- lapply(corpus.lda, function(x) gsub(" +", " ", x))
  # pdf.corpus <- lexicalize(empty.string, lower=TRUE)
  
  # corpus <- Corpus(VectorSource(corpus.lda))
  # corpus <- tm_map(corpus,removeWords,remove.words.file)
  # newtext <-tm_map(corpus,removeWords,input$remove_words)
  
  # pdf.corpus <- lexicalize(newtext, lower=TRUE)
  pdf.corpus <- lexicalize(corpus.lda, lower=TRUE)
  
 # if (input$language=="none") {
 #   pdf.corpus$vocab <- pdf.corpus$vocab
 # }
 # else {
   # language=input$language
    pdf.corpus$vocab <- wordStem(pdf.corpus$vocab, language = "english")
 # }
  wc <- word.counts(pdf.corpus$documents)
  to.remove <- as.numeric(names(wc)[wc<=cutoff.lower])
  pdf.corpus$documents <- filter.words(pdf.corpus$documents , to.remove)
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  K <- as.numeric(input$num)
  alphaK <-as.numeric(input$alpha)
  etaK<-as.numeric(input$eta)
  num.words <- as.numeric(input$word)
  iterK <-as.numeric(input$iter)
  pdf.lda <-
    lda.collapsed.gibbs.sampler(pdf.corpus$documents,K,pdf.corpus$vocab,iterK, alpha=alphaK, eta=etaK, compute.log.likelihood=TRUE)
  topics <- top.topic.words(pdf.lda$topics, num.words, by.score = T)
  docs <- top.topic.documents(pdf.lda$document_sums,n.docs)# num.documents)
  p_topic <- as.vector(pdf.lda$topic_sums / sum(pdf.lda$topic_sums))
  lda.coordinates <- mat.or.vec(n.docs,K)
  # for (i in 1:n.docs){
  #   for (j in 1:K){
  #     lda.coordinates[i,j] <-
  #       sum(pdf.lda$assignments[[i]]==(j-1))/length(pdf.lda$assignments[[i]])
  #   }
  # }
  info<-list(p_topic=p_topic,topics=topics, docs=docs,lda.coordinates=lda.coordinates)
  return(info)
})
########topics########
output$topics <- renderTable({ #renderUI({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  LdaAnalysis()$topics
})
#######docs#########
output$docs <- renderPrint({
#  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  LdaAnalysis()$docs
})

output$docsNames <- renderUI({
#  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) ||  (input$lda=="None")) { return() }
 
    k <- length(extractZoteroTerm()$titles)
    n <- as.list(rep(1:k,1))
    HTML(paste("Document ",n, ":", extractZoteroTerm()$titles,  sep=" ", collapse="<br/>"))

})
########printCoordinates########
output$printCoordinates <-renderPlot({
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if((is.null(input$lda)) || (input$lda=="None")) { return() }
  
  distance <- LdaAnalysis()$lda.coordinates
  d<-dist(distance)
  # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  # fit # view results
  # plot solution 
  x <- fit$points[,1]
  y <- fit$points[,2]
  p <- plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
            main="Metric  MDS",	type="n")
  text(x, y,  cex=.9)  
  return(p)
})

output$best_topic_num <-renderUI({
  names <- c("NULL","Calculate")
  selectizeInput("best_num", label = "Select Calculate to find the best topic number (Log Likelihood) - It may take a long time", 
                 choices = names,
                 selected=FALSE,
                 multiple = FALSE) 
}) 
########stmAnalys############
stmAnalysis <- reactive ({
#  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  #novel.vector <- as.list(RemoveWordsStepTwo()$d$word)
 # novel.vector <- as.list(RemoveWordsStepThree()$d$word)
  novel.vector <- RemoveWordsStepThree()$corpus
  corpus <- Corpus(VectorSource(novel.vector))
  tdm <-DocumentTermMatrix(corpus)   
 # dm <- RemoveWordsStepThree()$dtm
  dtm <- RemoveWordsStepThree()$dtm
 # out <- readCorpus(tdm, type="dtm")
  out <- readCorpus(dtm, type="dtm")
  
  documents <- out$documents
  vocab <- out$vocab
  n.topics <- as.numeric(input$num)
  stmmodel <- stm(documents, vocab, n.topics, verbose=FALSE)
 # return(dm)
  return(stmmodel)
})
### Print Topics

#### Print Documents for each topic
#Example of documents associated with topics
output$association <- renderPlot ({
  if (input$stm=="None") { return() }
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  novel.vector <-RemoveWordsStepThree()$corpus#PreprocessingSteps()[6]#PreprocessingSteps()$lda.format#window.one()$lda.format
  K=input$num
  #par(mfrow = c(3, 3),mar = c(.5, .5, 1, .5))
  for (i in 1:K){
    thoughts <- findThoughts(stmmodel, text=novel.vector,topic=i,n = 1)
    #thoughts <- paste(strsplit(as.character(thoughts3$docs[1]), " ")," ")
    #thoughts5 <- findThoughts(stmmodel, text=novel.vector,topic=5,n = 1)
    plotQuote(thoughts, width = 30, main = paste("Topic",i, " "))
  }
})
### Summary
# Expected Topic Proportion
#########proportion#########
output$proportion <- renderPlot ({
  if (input$stm=="None") { return() }
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  #par(mfrow=c(1,1),mar=c(5,5,5,5))
  plot.STM(stmmodel, type = "summary", xlim = c(0, .9))
})

########## perspective########
output$perspectives <- renderPlot ({
#  if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if (input$stm=="None") { return() }
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
  par(mfrow=c(1,1),mar=c(1,1,1,1))
  plot.STM(stmmodel, type = "labels")#, xlim = c(0, .9))#,xlim = c(1, 5))
})
##########cloud_stm########
output$cloud_stm <- renderPlot({
  if (input$stm=="None") { return() }
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((is.null(input$stm))|| (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  K=as.integer(input$num)
  cloud(stmmodel, topic = K, scale = c(5,.25))
})

### graphical display of topic correlation
#########topic corelation#########
output$corelation <-renderPlot({
  if (input$stm=="None") { return() }
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
 # if((is.null(input$stm)) || (input$stm=="None")) { return() }
  stmmodel<-  stmAnalysis()
  modoutcorr <- topicCorr(stmmodel)
  #modoutcorr$cor
  plot.topicCorr(modoutcorr,vertex.label.cex = 1.0)
})

output$topics_stm <- renderPrint({
  #if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  if (input$stm=="None") { return() }
  stmmodel<-  stmAnalysis()
  labelTopics(stmmodel)
  # stmmodel$documents
})



######## MALLET######

output$malletprint <- renderTable({
  
  extractZoteroTerm()$ids
})

mallets <- reactive({
mallet.instances <- mallet.import(as.character(extractZoteroTerm()$ids), as.character(extractZoteroTerm()$texts),"stopwords.txt",FALSE, token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
## Create a topic trainer object.
topic.model <- MalletLDA(num.topics=10)
seed <- 42 # choose any integer you like
topic.model$model$setRandomSeed(as.integer(seed))
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
## Optimize hyperparameters every 20 iterations, 
# ##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)
topic.model$train(200)
# 
# ## NEW: run through a few iterations where we pick the best topic for each token, 
# ##  rather than sampling from the posterior distribution.
max <- topic.model$maximize(10) 
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)  

frequency.table <-  mallet.word.freqs(topic.model) 
frequency.table <-  frequency.table[order(-frequency.table$term.freq),]
topic.labels <-   mallet.topic.labels(topic.model, topic.words, num.top.words=3)
#mallet.topic.hclust(doc.topics, topic.words, balance) #balance = A value between 0.0 (use only document-level similarity) and 1.0 (use only word-level similarity).
p = plot(mallet.topic.hclust(doc.topics, topic.words, balance= 0.3), labels=topic.labels)

#  top 10 words
top <-   head(frequency.table, n=10 ) 
colnames(top) = c("word","freq","doc.freq")
nw <- list()
for (i in 1:length(extractZoteroTerm()$ids)) {
  df2 = extractZoteroTerm()$terms[i]
  id = extractZoteroTerm()$id[i]
  #year=as.character(import$datetimes[i])
  df = merge(x = top, y = df2, by = "word", all.x = TRUE)
  droplevels(df)
  #colnames(df) = c("word","freq","doc.freq",year)
  # df_total <- cbind(df[,4])
  freq <-df$n
  
  word <- as.character(df$word)
  year <- as.integer(extractZoteroTerm()$datetimes[i])
  title <- as.character(extractZoteroTerm()$titles[i])
  nw[[i]] <- as.tibble(cbind(word,freq,year,title,id))
}

nm <- lapply(nw, as.matrix)
n <- max(sapply(nm, nrow)) 
longfrequency <- as.tibble(do.call(rbind, lapply(nm, function (x) 
  rbind(x, matrix(, n-nrow(x), ncol(x))))))

colnames(longfrequency) <- c("name","n","year","title","id")
longfrequency$n[is.na(longfrequency$n)] <- 0
longfrequency$n <- as.integer(longfrequency$n)
longfrequency$year <- as.integer(longfrequency$year)
longfrequency <- longfrequency[!is.na(longfrequency$year),]
extract <- list(p=p,top = top,longfrequency=longfrequency,topic.labels=topic.labels,topic.words=topic.words,doc.topics=doc.topics)
return(extract)

})
######## CLUSTER ANALYSIS######

#mallettop <-reactive ({
 # if((is.null(input$cluster)) || (input$cluster=="None")) { return() }
  #method=input$method
 # top = mallets()$top
  #distance = input$distance
  #corpus <- stemming()
  #doc.vect <- VectorSource(corpus)
  #docs <-VCorpus(doc.vect)
 # tdm <- TermDocumentMatrix(docs)
 # dtm <-RemoveWordsStepThree()$dtm#ListTerms()$dtm
 # m <- as.matrix(dtm)
#  m <- as.matrix(tdm)
 # plot(mallet.topic.hclust(doc.topics, topic.words, balance= 0.3), labels=topic.labels)
 # d<-dist(m,distance)
 # fit <-hclust(d,method)
  
 # return(top)
#})

output$mallettop <- renderTable({
  mallets()$topic.labels
})

output$longfrequency <- renderDataTable({
  mallets()$longfrequency
})

output$cluster_plot <- renderPlot({
 # if((is.null(input$cluster)) || (input$cluster=="None")) { return() }
 # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  #par(cex=1.2,mar=c(5, 4, 4, 2))
  #mallets()$p
 plot(mallet.topic.hclust(mallets()$doc.topics, mallets()$topic.words, balance= 0.3), labels=mallets()$topic.labels)
  # fit <- cluster()
  #return(fit)
})


#output$cuttree <-renderUI({
#  punct <- c(0,2,3,4,5,6,7,8,9)
#  selectizeInput("cuttree", label = "Select number of groups", 
 #                choices = punct,
 #                selected = 0,
 #                multiple = FALSE) 
#})

######### streamgraph ###########
output$sg1 <- renderStreamgraph({
  
  #name <- c("populism","security", "populism", "security","populism", "security",
    #        "europe","defense","europe","defense","europe","defense")
  #year <- c(2001,2001,2002,2002, 2003, 2003, 2001,2001, 2002, 2002, 2003, 2003)
  #count <- c(100,50,150, 70, 50, 200, 80,40, 70,70, 100,150)
  longfrequency <- mallets()$longfrequency
   longfrequency %>% 
    group_by(year, name) %>%
    arrange(year) %>%
    tally(wt=n) %>%
    streamgraph("name", "nn", "year", interpolate="cardinal") %>%
    sg_legend(show=TRUE, label="Words: ") %>%
     sg_fill_brewer("PuOr") %>%
    sg_axis_x(1, "year", "%Y") 
 
  })


output$print_data = renderTable({
  
  d<-chronology()$M
  
  name <- as.character(d[,3])
  year <- d[,2]
  count <-d[,4]
  
  myset <- data.frame(name,year,count)
  
})
output$chronology_top2 <- renderPrint ({
  #if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
  # if((input$chronology=="None") || (is.null(input$chronology))) { return() }
  chronology()$term
})
#### NGRAM #####
#ngramFunction <- reactive({
 
 # if (is.null(input$file.rdf))  { return() }
 # text <- unlist(#RemoveWordsStepThree()$corpus)#extractZoteroTerm()
 # title <- extractZoteroTerm()$titles
 # ngram <- ngramBuilder(title, text)
 # return(ngram)
#})

#ngramFunction2 <- reactive({
  # if (is.null(input$file.rdf))  { return() }
 # text <- unlist(RemoveWordsStepThree()$corpus)#extractZoteroTerm()
 # title <- extractZoteroTerm()$titles
 # date <- extractZoteroTerm()$datetimes
 # ngram <- ngramBuilder2(date, text)
 # return(ngram)
#})

#output$ngram1 <- renderPlot({
 # bigrams_counts_united <- ngramFunction()[[1]]
 # ggplot(bigrams_counts_united[1:15,],aes(bigram, n)) +
 #   geom_col(show.legend = FALSE) +
 #   labs(x = NULL, y = "count") +
 #   coord_flip()
#})

#output$ngram2 <- renderPlot({
#  bigram_tf_idf <- ngramFunction()[[3]]
#  ggplot(bigram_tf_idf, aes(bigram, tf_idf, fill = title)) +
 #   geom_col(show.legend = FALSE) +
 #   labs(x = NULL, y = "tf-idf") +
 #   facet_wrap(~title, ncol = 1, scales = "free") +
  #  coord_flip() +
  #  theme_classic()
#plot(bigram_tf_idf)
#})

gram <- reactive({
  
  text = unlist(RemoveWordsStepThree()$corpus)
  d = data.frame(txt = text)
  d1 <- d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 1) %>% dplyr::count(ngram, sort =TRUE)
  d2 <- d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 2) %>% dplyr::count(ngram, sort =TRUE)
  d3 <- d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 3) %>% dplyr::count(ngram, sort =TRUE)
  
  info <- list(d1=d1,d2=d2,d3=d3)
  return(info)
})

output$freq_unigram <- renderDataTable({
  gram()$d1
 # txt = unlist(RemoveWordsStepThree()$corpus)
  
 # d = data.frame(txt = text)
 # d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 1) %>% count(ngram, sort =TRUE)
  #ngramFunction()[[5]]
})

output$freq_bigram <- renderDataTable({
  gram()$d2
 # txt = unlist(RemoveWordsStepThree()$corpus)
  
 # d = data.frame(txt = text)
 # d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 2) %>% count(ngram, sort =TRUE)
 # ngramFunction()[[4]]
})

output$freq_trigram <- renderDataTable({
 # ngramFunction2()
  gram()$d3
 # txt = unlist(RemoveWordsStepThree()$corpus)
  
 # d = data.frame(txt = text)
 # d %>% unnest_tokens(ngram, txt,token = "ngrams", n = 2) %>% count(ngram, sort =TRUE)
})

output$network <- renderPlot({
  if((is.null(input$show)) || (input$show=="None")) { return() }
  set.seed(2017)
  bigrams_separated <- ngramFunction()[[6]]
 bigram_graph <- bigrams_separated %>%
    filter(n > as.integer(input$ngram_slider)) %>%
    graph_from_data_frame()
 
 a <- grid::arrow(type = "closed", length = unit(.6, "inches"))
 
 ggraph(bigram_graph, layout = "fr") +
   geom_edge_link() +
   #geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
    #              arrow = a, end_cap = circle(.07, 'inches')) +
   geom_node_point(color = "lightblue", size = 5) +
   geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
   theme_void()
})

output$distribution <- renderPlot({
  if ((is.null(input$zotero_submit))) { return() }
  data <- data.frame(extractZoteroTerm()[[4]])
  
  df = as.data.frame(table(data))
  colnames(df) = c("Years","Count")
  ggplot(df,aes(Years, Count) ) + 
           geom_bar(stat="identity")+
    theme_classic()
 
})


# #streamdata <- reactive ({
#   # if ((is.null(input$file.article)) && (is.null(input$file.article.txt)) && (is.null(structured_data()))&&(is.null(input$file.tag))&&(is.null(input$file.rdf))) { return() }
#   #if( input$metadata_source == "None") {return()}
#   # if((is.null(input$chronology)) ||(input$chronology=="None")) { return() }
# #  remove.words.file <- stopWordsTxt()
#   
#   corpus.lda <-  RemoveWordsStepOne()$corpus
#   corpus.lda <- removeWords(corpus.lda, remove.words.file)
#   corpus.lda <- removeWords(corpus.lda, c(input$remove_words))
#   corpus.lda <- gsub("\\s+"," ",corpus.lda)
#   corpus.lda <- str_c(corpus.lda)
#   corpus.lda<- str_trim(corpus.lda)
#   set.seed(2013)
#   my.corpus <- VCorpus(VectorSource(corpus.lda))
#   # my.corpus <- tm_map(my.corpus,removeWords,stopwords("english"))
#   language=input$language
#   dtm <- DocumentTermMatrix(my.corpus)
#   # if (!is.null(input$metadata_pdf)) {
#   dates<-  fileData()$date
#   # dates <- metadataPdf()$datetimes
#   # }
#   #if (!is.null(input$metadata_csv)) {
#   #  dates <- newData()[[2]]
#   # }
#   for (i in 1:length(my.corpus)){
#     meta(my.corpus[[i]], tag = "datetimestamp") <- dates[i]
#   }
#   dtm <- DocumentTermMatrix(my.corpus)
#   n.topics <- as.numeric(input$num)
#   lda.model <- LDA(dtm, n.topics,method="Gibbs")
#   n.words <- as.numeric(input$word)
#   term <- terms(lda.model,n.words)  
#   df <- data.frame(id=names(topics(lda.model)), 
#                    date=unlist(meta(my.corpus, type="local",tag="datetimestamp"),as.character)#, "%Y-%m-%d %H:%M:%S"))))#2011-03-27 01:30:00", "%Y-%m-%d %H:%M:%S"   
#   )
#   
#   # dfw <- cbind(df,posterior(lda.model)$topics)
#   dft <- cbind(df,posterior(lda.model)$topics)
#   # dft2 < cbind(df,term)
#   M <-melt(dft,id.vars=c("id","date")) 
#   l<-length(my.corpus)
#   info <- list(dft=dft, term=term,lda.model=lda.model,M=M,l=l)#,datetimestamp=datetimestamp)
#   return(info)
# })

})