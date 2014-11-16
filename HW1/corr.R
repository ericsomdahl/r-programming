source("complete.R")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  ##find the files in the folder so that we can run them through complete
  fnames <- list.files(path = directory, pattern = '*.csv', full.names = F)
  fstrs <- lapply(fnames, function(f) str_match(f, '^([[:digit:]]{3}).csv$'))
  fnums <- lapply(fstrs, function(f) as.numeric(f[2]))
  
  ##figure out how many observations we have
  ldf_complete = complete(directory, unlist(fnums))
  
  ##get stations that meet the threshold
  ldf_min <- subset(ldf_complete, ldf_complete$nobs > threshold)
  
  #if nothing met the threshold return a default 0 value
  if (nrow(ldf_min) < 1) {
    return(c(0))
  }
  
  ##here we go
  li_usable_ids = ldf_min$id
  
  ##figure out which filenames to read
  filenames <- sprintf("./specdata/%03d.csv", li_usable_ids)
  
  ##read them in
  ldf <- lapply(filenames, function(f) read.csv(f, header=TRUE))
  
  answer <- lapply(ldf, function(x) cor(x = x$sulfate, y = x$nitrate, use='pairwise.complete.obs'))
  unlist(answer)
}

filtered <- function(df, t) {
  subset(df, !is.na(df[,t]))
}