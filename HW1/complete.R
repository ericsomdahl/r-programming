complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  ##figure out which filenames to read
  filenames <- sprintf("./specdata/%03d.csv", id)
  
  ##read them in
  ldf <- lapply(filenames, function(f) read.csv(f, header=TRUE))
  
  ##filter out readings that have no sulfate
  ldf_all_sulfate <- lapply(ldf, function(s) filtered(s, 'sulfate'))
  
  ##filter out readings that have no nitrate
  ldf_all_obs <- lapply(ldf_all_sulfate, function(n) filtered(n, 'nitrate'))
  
  ##allocate a dataframe for the output
  df <- data.frame(id=rep.int(NA, 0), nobs=rep.int(NA, 0), stringsAsFactors=FALSE)
  
  ##build an array of the observation values
  l_ob_values <- lapply(ldf_all_obs, function(f) list(id=f[[4]][1], nobs=nrow(f)))
  
  for (i in 1:length(l_ob_values)) {
    df <- rbind(df, l_ob_values[[i]])
  }
  
  rownames(df) <- 1:nrow(df)
  df
}

filtered <- function(df, pollutant) {
  subset(df, !is.na(df[,pollutant]))
}