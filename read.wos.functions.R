library(data.table)

# This function will parse a list of WoS export files in Tab-delimited (Win, UTF-8) Format, and convert them
# to a data.table.
read.wos.tw8 <- function(filefolder = "./files", fields_path = "fields.txt", nrows=1000000L) {
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

# This function will parse a list of WoS export files in Plain Text Format, and convert them
# to a data.table.
read.wos.plain <- function(filefolder = "./files", fields_path = "fields.txt", nrows=10000000L) {
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
    # Gets a list of the lines where a record end, and sets the beginning of the first record
    end_of_records <- which(grepl("ER", substr(lines,1,2)))
    beginning_of_record  <- 3
    
    # Iterates through all records in the file
    for (end_of_record in end_of_records) {
      # subsets the file to get only the lines containing a single record
      current_record <- lines[beginning_of_record:end_of_record-1]
      # creates an empty vector where the fields will be stored
      row <- vector()
      
      # FIELD SEARCH
      # Iterates through the list of fields that we want to process.
      for (field in fields) {
        
        # Looks for the current field in the current record.
        field_position <- which(grepl(field, substr(current_record,1,2)))
        
        # If it is found, the data is extracted, if not, sets the value to NA
        if (length(field_position) == 1) {
          field_data  <- current_record[field_position]
          new_field_line = field_position + 1
          
          # There are fields that span across more than one line (Authors, etc.)
          # If the following line to the one we are processing now begins with whitespaces,
          # it is still the same field, so we append those lines to the field we are processing.
          while (new_field_line <= length(current_record) && length(which(grepl(substr(current_record[new_field_line],1,2),fields))) != 1) {
            # If the field we are processing is an Author field, we want to separate the names with
            # ";". In the rest of cases, we just want to concatenate the data together with a space.
            if (field == "AU" | field == "AF") {
              sep = ";"
            } else {
              sep = " "
            }
            field_data <- paste(field_data, substring(current_record[new_field_line],4), sep=sep)
            new_field_line  <- new_field_line + 1
          }
          
        } else {
          field_data <- NA
        }
        # Adds each field to the row vector
        row  <- c(row, substring(field_data,4))
        
      }
      # Splits each row into a character vector, and updates the rows in the data.table
      j  <- 1L # column counter
      for (field_value in row) {
        set(dt,i,j,field_value)
        j  <- j + 1L
      }
      i  <- i + 1L
      beginning_of_record <- end_of_record + 2 
    }      
  }
  # deletes unused rows
  dt <- dt[PT != "0"]
  # returns data.table
  dt
}

# Deletes white spaces from the left and right of a string
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# This function splits a field with multiple elements (authors,
# subject categories, etc... and creates a new data.table containing
# each element in a separate row, keeping its relationship to the 
# original record through the ID column.
split.field <- function(idcol, splitcol, delimiter) {
  
  # creates empty data.table
  dt <- data.table(x=rep("0",100000000L))
  l  <- list(rep("0",2))
  i  <- 1
  for (j in 1:2) {
    l[[i]] <- rep("0",100000000L)
    i <- i + 1
  }
  dt[, c("UT","split") := l, with = FALSE]
  dt[,x:=NULL]
  
  i  <- 1L # row counter
  for (docix in 1:length(idcol)) {
    id <- idcol[docix]
    splitfield <- splitcol[docix]
    split_list <- strsplit(splitfield, delimiter)[[1]]
    for (el in split_list) {
      el  <- trim(el)
      if (length(el) > 0) {
        set(dt,i,1L,id)
        set(dt,i,2L,el)
        i  <-  i + 1L
      }
    }
  }  
  # deletes unused rows
  dt <- dt[UT != "0"]
  dt
}