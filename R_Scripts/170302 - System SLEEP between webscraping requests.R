N <- length of links to be scraped
for (i in seq_along(reviewLinks)){
  # file name
  fname <- str_c(
    
    # download
    if(!file.exists(fname) & !is.na(reviewLinks[i])){
      message("downloading")
      try(download.file(reviewLinks[i], fname))
      
      # sleep
      sleep <- abs(rnorm(1)) + runif(1, 0, .25)
      message("I have done ", i, " of ", N,
              " - gonna sleep ", round(sleep, 2),
              " seconds.")
      Sys.sleep(sleep)
    }
    
    # size of file info
    message(i, "size: ", file.info(fname)$size/1000, " KB")
}

N <- 23
i <- 5

?try

