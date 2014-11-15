pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ##figure out which filenames to read
  filenames <- sprintf("./specdata/%03d.csv", id)
  
  ##read them in
  ldf <- lapply(filenames, function(f) read.csv(f, header=TRUE))
  
  ##create a vector of means for each file
  meanf <- lapply(ldf, function(x) filtered(x, pollutant))

  ##get the mean of the vector
  mean(unlist(meanf))
}

filtered <- function(df, pollutant) {
  non_na <- subset(df, !is.na(df[,pollutant]))
  non_na[,pollutant]
}