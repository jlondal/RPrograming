setwd("/Users/jameslondall/Dropbox/DS Course/R/RPrograming")

complete <- function(directory, id = 1:332) {
  
  directory <- paste('/Users/jameslondall/Documents/Data/DS Course/',directory,sep="")
  files<-as.character(list.files(directory))
  files_paths<-paste(directory, all_files,sep="/")
  
  df_out <- rep(0,length(id))
  j <- 1
  for (i in id){
    df <- read.csv(files_paths[i])
    df_out[j] <- sum(complete.cases(df))
    j <- j + 1
  }
  data.frame(id = id, nobs = df_out)
}