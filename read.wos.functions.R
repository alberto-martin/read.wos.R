library(data.table)

fields <- c('PT', 'AU', 'BA', 'BE', 'GP', 'AF', 'BF', 'CA', 'TI', 'SO', 'SE', 'BS', 
            'LA', 'DT', 'CT', 'CY', 'CL', 'SP', 'HO', 'DE', 'ID', 'AB', 'C1', 'RP', 
            'EM', 'RI', 'OI', 'FU', 'FX', 'CR', 'NR', 'TC', 'Z9', 'PU', 'PI', 'PA', 
            'SN', 'EI', 'BN', 'J9', 'JI', 'PD', 'PY', 'VL', 'IS', 'PN', 'SU', 'SI', 
            'MA', 'BP', 'EP', 'AR', 'DI', 'D2', 'PG', 'WC', 'SC', 'GA', 'UT', 'AA', 
            'BB')

# This function will parse a list of WoS export files in Tab-delimited (Win, UTF-8) Format, and convert them
# to a data.table.

read.wos.tw8 <- function(path = './files', nrows=1000000L) {
  # reads list of files
  files  <- list.files(path)  
  
  # creates empty data.table
  dt <- data.table(x=rep('0',nrows))
  l  <- list(rep("0",length(fields)))
  i  <- 1
  for (field in fields) {
    l[[i]] <- rep('0',nrows)
    i <- i + 1
  }
  dt[, fields := l, with = FALSE]
  dt[,x:=NULL]
  
  i  <- 1L # row counter
  # Iterates through all files in path
  for (file in files) {
    # reads a file, and saves its lines as a character vector
    fullpath  <- paste(path,'/',file, sep='')
    lines  <- readLines(fullpath)
    lines <- lines[2:length(lines)]
    
    for (line in lines) {
      # Splits each row into a character vector, and updates the rows in the data.table
      row <- strsplit(line, '\t')[[1]][1:length(fields)]
      j  <- 1L # column counter
      for (field_value in row) {
        # Converts empty fields to NA
        if (field_value == "" | is.na(field_value)) {
          field_value  <- NA
        }
        set(dt,i,j,field_value)
        j  <- j + 1L
      }
      i  <- i + 1L
    }
  }
  # deletes unused rows and columns
  dt <- dt[PT != '0']
  dt <- dt[,AA:=NULL]
  dt <- dt[,BB:=NULL]
  
  # converts some variables to integer: NR (number of cited references),
  #                                     TC (times cited WoS),
  #                                     Z9 (total Times Cited Count (WoS, BCI, and CSCD))
  #                                     PY (publication year)
  
  int_fields <- c('NR', 'TC', 'Z9', 'PY')
  for (int_field in int_fields) {
    class(dt[[int_field]]) <- 'integer'
  }
  
  # returns data.table
  dt
}

###-----------------------------------------------------------------------------------------

# This function is a modified version of strsplit intended to clean the UT field at the end
# of the read.wos.plain function
clean_ut <- function(char_vec, split = " ") {strsplit(char_vec, split = split)[[1]][1]}

###-----------------------------------------------------------------------------------------

# This function will parse a list of WoS export files in Plain Text Format, and convert them
# to a data.table.
read.wos.plain <- function(path = './files', nrows=10000000L) {
  files  <- list.files(path)
  
  # creates empty data.table
  dt <- data.table(x=rep('0',nrows))
  l  <- list(rep('0',length(fields)))
  i  <- 1
  for (field in fields) {
    l[[i]] <- rep('0',nrows)
    i <- i + 1
  }
  dt[, fields := l, with = FALSE]
  dt[,x:=NULL]
  
  i  <- 1L # row counter
  # Iterates through all files in path
  for (file in files) {
    # reads a file, and saves its lines as a character vector
    fullpath  <- paste(path,'/',file, sep='')
    lines  <- readLines(fullpath)
    # Gets a list of the lines where a record end, and sets the beginning of the first record
    end_of_records <- which(grepl('ER', substr(lines,1,2)))
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
  dt <- dt[PT != '0']
  dt <- dt[,AA:=NULL]
  dt <- dt[,BB:=NULL]
  
  # cleans UT field
  dt$UT  <- sapply(dt$UT, clean_ut, split = ' ')
  
  # converts some variables to integer: NR (number of cited references),
  #                                     TC (times cited WoS),
  #                                     Z9 (total Times Cited Count (WoS, BCI, and CSCD))
  #                                     PY (publication year)
  
  int_fields <- c('NR', 'TC', 'Z9', 'PY')
  for (int_field in int_fields) {
    class(dt[[int_field]]) <- 'integer'
  }
  # returns data.table
  dt
}

#------------------------------------------------------------------------------------------

# Deletes white spaces from the left and right of a string
trim <- function (x) gsub('^\\s+|\\s+$', '', x)

#------------------------------------------------------------------------------------------

# This function splits a field with multiple elements (authors,
# subject categories, etc... and creates a new data.table containing
# each element in a separate row, keeping its relationship to the 
# original record through the ID column.
split.simple <- function(source_dt, idcol = 'UT', splitcol, delimiter = ';') {
  
  idcol_name <- idcol
  splitcol_name <- splitcol
  idcol <- source_dt[[idcol]]
  splitcol <- source_dt[[splitcol]]
  
  # creates empty data.table
  dt <- data.table(x=rep('0',100000000L))
  l  <-  list(rep('0',2))
  i  <- 1
  for (j in 1:2) {
    l[[i]] <- rep('0',100000000L)
    i <- i + 1
  }
  dt[, c(idcol_name, splitcol_name) := l, with = FALSE]
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
  dt <- dt[UT != '0']
  dt
}

#------------------------------------------------------------------------------------------

# This function splits the C1 field (authors / addresses) and creates a new data.table
# containing each element in a separate row, keeping its relationship to the 
# original record through the ID column.

split.c1 <- function(source_dt, idcol = 'UT', splitcol = 'C1', delimiter = ';') {
  
  idcol_name <- idcol
  splitcol_name <- splitcol
  idcol <- source_dt[[idcol]]
  splitcol <- source_dt[[splitcol]]
  
  # creates empty data.table
  dt <- data.table(x=rep('0',100000000L))
  l  <-  list(rep('0',3))
  i  <- 1
  for (j in 1:3) {
    l[[i]] <- rep('0',100000000L)
    i <- i + 1
  }
  dt[, c(idcol_name, 'author', 'address') := l, with = FALSE]
  dt[,x:=NULL]
  
  i  <- 1L # row counter
  for (docix in 1:length(idcol)) {
    id <- idcol[docix]
    splitfield <- splitcol[docix]
    
    # Since there are semicolons both inside and outside the square brackets,
    # I replace semicolons inside square brackets by another delimiter, so they
    # don't get affected by the following statement, which is intended to split
    # addresses, not authors.
    splitfield <- gsub(";(?=[^\\[\\]]*\\])", "/", splitfield, perl=T)
    
    split_list <- strsplit(splitfield, delimiter)[[1]]
    for (el in split_list) {
      
      if (grepl('\\[', el) == TRUE) {
        author_list <- strsplit(strsplit(el, '\\]')[[1]][1], '\\/')[[1]]
        #print(author_list)
        address <- trim(strsplit(el, '\\]')[[1]][2])
        for (author in author_list) {
          author <- trim(gsub('\\[', '', author))
          set(dt, i, 1L, id)
          set(dt, i, 2L, author)
          set(dt, i, 3L, address)
          i  <-  i + 1L
        }
        
      } else {
        address <- trim(el)
        if (length(address) > 0) {
          set(dt, i, 1L, id)
          set(dt, i, 2L, NA)
          set(dt, i, 3L, address)
          i  <-  i + 1L
        }
      }
    }
  }  
  # deletes unused rows
  dt <- dt[UT != '0']
  dt
}

#------------------------------------------------------------------------------------------

# This function wraps the read.wos.tw8 and read.wos.plain functions, allowing to select
# the file format through the format parameter

read.wos  <- function(path = './files', format = 'tab_win_utf8') {
  if (format == 'tab_win_utf8') {
    read.wos.tw8(path = path)
  } else if (format == 'plain_text') {
    read.wos.plain(path = path)
  }
}

#------------------------------------------------------------------------------------------

# This function wraps the split.simple and split.c1 functions, calling the latter if 
# splitcol == C1 and the former in the rest of the cases

split.field <- function(source_dt, idcol = 'UT', splitcol, delimiter = ';') {
  if (splitcol == 'C1') {
    split.c1(source_dt = source_dt, idcol = idcol, splitcol = 'C1', delimiter = delimiter)
  } else {
    split.simple(source_dt = source_dt, idcol = idcol, splitcol = splitcol, delimiter = delimiter)
  }
}