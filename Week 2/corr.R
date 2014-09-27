setwd("/Users/jameslondall/Dropbox/DS Course/Week 2/RPrograming/Week 2")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations

  directory <- paste('/Users/jameslondall/Documents/Data/DS Course/',directory,sep="")
  files<-as.character(list.files(directory))
  files_paths<-paste(directory, files,sep="/")
  
  obs <-complete("specdata",1:332)  
  ids <- obs[obs$nobs > threshold,]$id
  
  df_out <- rep(0,length(ids))
  
  j <- 1
  for (i in ids){
    df <- read.csv(files_paths[i])
    df_out[j] <- cor(df$sulfate, df$nitrate, use="complete.obs")
    j <- j + 1
  }
  df_out  

}