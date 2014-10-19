# This function will parse a list of WoS export files in Tab-delimited (Win, UTF-8) Format, and convert them
# to a data.frame.

read.wos.tabwin.utf8 <- function(filefolder = "./files", fields_path = "fields.txt") {
  # reads list of fields
  fields  <- readLines(fields_path)
  files  <- list.files(filefolder)
  
  df <- data.frame()
  
  # Iterates through all files in filefolder
  for (file in files) {
    # reads a file, and saves its lines as a character vector
    fullpath  <- paste(filefolder,"/",file, sep="")
    lines  <- readLines(fullpath)
    lines <- lines[2:length(lines)]
    for (line in lines) {
      
      # creates a one-row data.frame and appends it to the output data.frame (df)
      row_matrix <- strsplit(line, "\t")[[1]]
      dim(row_matrix) <- c(1,length(fields))
      row_df <- data.frame(row_matrix)
      df <- rbind(df, row_df)
    }
    
  }
  colnames(df) <- fields
  df
}