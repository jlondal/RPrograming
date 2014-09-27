setwd("/Users/jameslondall/Dropbox/DS Course/RPrograming/Week 2")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  directory <- paste('/Users/jameslondall/Documents/Data/DS Course/',directory,sep="")
  files<-as.character(list.files(directory))
  files_paths<-paste(directory, files,sep="/")
  
  df2 <- c()
  for(i in id) {
    df <- read.csv(files_paths[i])
    df2 <-   c(df2,df[!is.na(df[pollutant]),][[pollutant]])
  }
  round(mean(df2),3)  
}