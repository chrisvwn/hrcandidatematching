library(tm)
library(text2vec)
library(magrittr)
library(RMariaDB)

rankClientJobs <- function(clientId)
{
  if(missing(clientId))
    return()
  
  tryCatch({
    dbCon <- RMariaDB::dbConnect(RMariaDB::MariaDB(), host=dbHost, user=dbUser, db=dbName)
  
    clientJobs <- getClientJobSearchResults(clientId)

    clientDetails <- clientDetails[which(clientDetails$Property %LIKE% 'Posicao_Cargo_Interesse')]
      
    clientDetails <- getClientDetails(clientId)$Value
    
    if(nrow(clientJobs) < 2)
      return()
    
    clientDetails <- gsub("NA", "", clientDetails)
    
    clientDetails <- paste(clientDetails, collapse=" ")
    
    prep_fun <- tolower
    
    tok_fun <- word_tokenizer
    
    if(all(is.na(clientJobs$DescriptionLong)))
      clientJobDescriptions <- clientJobs$DescriptionShort
    else
      clientJobDescriptions <- clientJobs$DescriptionLong
    
    #the iterator which defines the actions on each doc in this case lowercase
    #then tokenization
    iterClientJobs <-  itoken(clientJobDescriptions,
                        preprocessor = prep_fun,
                        tokenizer = tok_fun,
                        progressbar = TRUE)
    
    iterClientDetails <-  itoken(clientDetails,
                              preprocessor = prep_fun,
                              tokenizer = tok_fun,
                              progressbar = TRUE)
    
    message("vocab - ngrams")
    #create ngrams vocab excluding stopwords
    vocab <-  create_vocabulary(iterClientJobs, stopwords = tm::stopwords(kind = 'portuguese'), ngram = c(1L, 2L))
    
    message("pruning vocabs")
    #remove terms that occur less than 10 times in total and
    #any that appear over 50% in any doc
    vocab <-  vocab %>% prune_vocabulary(term_count_min = 3,
                                         doc_proportion_max = 0.5,
                                         doc_proportion_min = 0.001)
    
    message("creating vectorizer")
    #create function that will vectorize
    bigram_vectorizer = vocab_vectorizer(vocab)
    
    message("creating dtms")
    #create ngram dtm
    dtm_train = create_dtm(iterClientJobs, bigram_vectorizer)
  
    dtm_ClientDetail_tfidf <- create_dtm(iterClientDetails, bigram_vectorizer)
      
    # define tfidf model
    tfidf <- TfIdf$new()
    
    message("creating tfidfs")
    # fit model to train data and transform train data with fitted model
    dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
  
    dtm_ClientDetail_tfidf <- transform(dtm_ClientDetail_tfidf, tfidf)
    
    client_job_sim <- sim2(dtm_ClientDetail_tfidf, dtm_train_tfidf, method = "cosine", norm="l2")
  
    jobRank <- colnames(client_job_sim)[order(client_job_sim, decreasing = T)]
    
    clientJobs$Rank <- as.numeric(jobRank)
    
    for(i in 1:nrow(clientJobs))
      res <- try(RMariaDB::dbExecute(dbCon, paste("UPDATE JobSearchResults SET Rank=",
                             clientJobs[i,"Rank"],
                             " WHERE Id=",
                             clientJobs[i,"Id"])),
                 TRUE)
    
    dbDisconnect(dbCon)
    
    return(TRUE)
  }, error=function(err)
  {
    message(err)
    
    dbDisconnect(dbCon)
    
    return(FALSE)
  })
}
  