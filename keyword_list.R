library(openxlsx)
library(data.table)

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
  dt <- dt[dt[[1]] != '0']
  dt
}

author_list <- read.xlsx('gsc_prof_list_processed.xlsx')

keywords <- split.simple(author_list, idcol = 'Profile_URL', 'Subjects', delimiter = '/')

keyword_freq <- as.data.table(table(tolower(keywords$Subjects)))
setorder(keyword_freq, -N)