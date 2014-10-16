# This function will parse a list of WoS export files in Plain Text Format, and convert them
# to a data.frame. Optionally, It will also save it to Tab delimited text file.

read.wos.plain <- function(filefolder = "./files", fields_path = "fields.txt") {
  # reads list of fields
  fields  <- readLines(fields_path)
  files  <- list.files(filefolder)
  
  # Connection to the output text file
  #output <- file("wos_tab.txt","w")
  # Writing headers to the file
  #writeLines(paste(fields, collapse="\t"), output)
  
  df <- data.frame()
  
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
      # creates a one row data.frame and appends it to the output data.frame (df)
      row_matrix <- row
      dim(row_matrix) <- c(1,length(fields))
      row_df <- data.frame(row_matrix)
      df <- rbind(df, row_df)
      
      # writes the vector to the file, separating each field with tabs
      #writeLines(paste(row, collapse="\t"), output)
      beginning_of_record <- end_of_record + 2
      
    }      
  }
  
  colnames(df) <- fields
  # Closes the connection
  #close(output)
  
  df
}
  
