# This function will parse a list of WoS export files in Tab-delimited (Win, UTF-8) Format, and convert them
# to a data.table.

library(data.table)

read.wos.tw8 <- function(filefolder = "./files", fields_path = "fields.txt", nrows=10000000L) {
  # reads list of fields
  fields  <- readLines(fields_path)
  files  <- list.files(filefolder)  
  
  # creates empty data.table
  dt <- data.table(x=rep("0",nrows))
  l  <- list(rep("0",length(fields)))
  i  <- 1
  for (field in fields) {
    l[[i]] <- rep("0",nrows)
    i <- i + 1
  }
  dt[, fields := l, with = FALSE]
  dt[,x:=NULL]
  
  i  <- 1L # row counter
  # Iterates through all files in filefolder
  for (file in files) {
    # reads a file, and saves its lines as a character vector
    fullpath  <- paste(filefolder,"/",file, sep="")
    lines  <- readLines(fullpath)
    lines <- lines[2:length(lines)]
    
    for (line in lines) {
      # Splits each row into a character vector, and updates the rows in the data.table
      row <- strsplit(line, "\t")[[1]][1:length(fields)]
      j  <- 1L # column counter
      for (field_value in row) {
        set(dt,i,j,field_value)
        j  <- j + 1L
      }
      i  <- i + 1L
    }
  }
  # deletes unused rows
  dt <- dt[PT != "0"]
  # returns data.table
  dt
}