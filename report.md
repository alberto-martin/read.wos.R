
# Reading Web of Science Data into R

### Alberto Martín-Martín, Emilio Delgado López-Cózar
#### EC3: Evaluación de la Ciencia y de la Comunicación Científica, Universidad de Granada (Spain)
#### albertomartin@ugr.es



March 09, 2015

##### Cite as:
Martín-Martin, A., & Delgado López.Cózar, E. (2015). Reading Web of Science Data into R. Available at: [url]. Accessed [day, month, year]

### Abstract
This document describes a series of functions designed to read Web of Science (WoS) data exported in tabular or plain text (RIS) format into R. Additionally, functions to process multiple-element fields (authors, addresses, Web of Science categories) are also presented. Lastly, some simple examples of the possible computations that may be executed once the data has been read into R are laid out. The code for these functions is accessible from our [read.wos.R GitHub repository](https://github.com/alberto-martin/read.wos.R).

### Introduction
R has its own function to read tabular data from text files (read.table), and many R packages provide their own functions for this task (i.e. data.table and fread). If this is the case, why are custom functions to import WoS data into R even necessary? The reason is that text files exported from WoS, even in tabular format, don't comply with the specifications set buy these functions, and therefore trying to read WoS data with these functions, at least with the most common parameter combinations, always results in errors or incorrectly read data. 

It may very well be that we've just missed the correct combination of function parameters that allows to read WoS data into R with these base functions, but in any case, in the end we had to resort to writing my own custom functions to be able to perform computations on WoS data with R.

For testing purposes, a set of files comprising the totality of records on the Web of Science on "INFORMATION SCIENCE LIBRARY SCIENCE" published between 1999 and 2014 have been downloaded. This set was downloaded in two stages:

* Articles published between 2007 and 2014 (95,821 records) were downloaded in "Tab-delimited (Win, UTF-8)" format. The search strategy in WoS' advanced search interface was: WC="INFORMATION SCIENCE LIBRARY SCIENCE" AND PY=2007-2014.
* Articles published between 1999 and 2006 (90,939 records) were downloaded in "Plain Text" (RIS) format. The search strategy in WoS' advanced search interface was: WC="INFORMATION SCIENCE LIBRARY SCIENCE" AND PY=1999-2006.

In both cases, the record content selected was "Full Record and Cited References"


```r
# "LIS_2007-2014" and "LIS_1999-2006" are the directories where I downloaded the data
head(list.files("LIS_2007-2014")); tail(list.files("LIS_2007-2014"))
```

```
## [1] "1-500.txt"       "10001-10500.txt" "1001-1500.txt"   "10501-11000.txt"
## [5] "11001-11500.txt" "11501-12000.txt"
```

```
## [1] "93501-94000.txt" "94001-94500.txt" "94501-95000.txt" "95001-95500.txt"
## [5] "9501-10000.txt"  "95501-95821.txt"
```

```r
head(list.files("LIS_1999-2006")); tail(list.files("LIS_1999-2006"))
```

```
## [1] "1-500.txt"       "10001-10500.txt" "1001-1500.txt"   "10501-11000.txt"
## [5] "11001-11500.txt" "11501-12000.txt"
```

```
## [1] "89001-89500.txt" "89501-90000.txt" "90001-90500.txt" "9001-9500.txt"  
## [5] "90501-90939.txt" "9501-10000.txt"
```

```r
total_files <- sum(length(list.files("LIS_2007-2014")),length(list.files("LIS_1999-2006")))
```
A total of 374 files where downloaded from WoS.

Before moving on to reading the data, it is necessary to state the exact environment where the code has been run. Our machine specifications are as follows:

* Intel i7 4th Gen. 3.40 GHz microprocessor.
* 16 GB DDR3 RAM
* Windows 7 64 bits

R and data.table versions

```r
R.version
```

```
##                _                           
## platform       x86_64-w64-mingw32          
## arch           x86_64                      
## os             mingw32                     
## system         x86_64, mingw32             
## status                                     
## major          3                           
## minor          1.1                         
## year           2014                        
## month          07                          
## day            10                          
## svn rev        66115                       
## language       R                           
## version.string R version 3.1.1 (2014-07-10)
## nickname       Sock it to Me
```

```r
packageVersion('data.table')
```

```
## [1] '1.9.4'
```
[data.table](http://cran.r-project.org/web/packages/data.table/index.html) is an R package developed by Matt Dowle that allows handling tabular data much more efficiently than with data.frames. This package must be installed for these functions to work properly.

To sum up, the following functions enable R users to easily import multiple text files exported from Web of Science in "Tab-delimited (Win, UTF-8)" or "Plain Text" format into an R data.table using a sigle function call, and additionally, to pre-process multiple-element fields like authors, addresses and WoS categories fields.

### Functions


```r
library(data.table)
```

```
## Warning: package 'data.table' was built under R version 3.1.2
```

```r
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
split.simple <- function(source_dt, idcol = source_dt[['UT']], splitcol, delimiter = ';') {
  
  splitcol_name <- splitcol
  splitcol <- source_dt[[splitcol]]
  
  # creates empty data.table
  dt <- data.table(x=rep('0',100000000L))
  l  <-  list(rep('0',2))
  i  <- 1
  for (j in 1:2) {
    l[[i]] <- rep('0',100000000L)
    i <- i + 1
  }
  dt[, c('UT', splitcol_name) := l, with = FALSE]
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

split.c1 <- function(source_dt, idcol = source_dt[['UT']], splitcol = 'C1', delimiter = ';') {
  
  splitcol_name <- splitcol
  splitcol <- source_dt[[splitcol]]
  
  # creates empty data.table
  dt <- data.table(x=rep('0',100000000L))
  l  <-  list(rep('0',3))
  i  <- 1
  for (j in 1:3) {
    l[[i]] <- rep('0',100000000L)
    i <- i + 1
  }
  dt[, c('UT', 'author', 'address') := l, with = FALSE]
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

split.field <- function(source_dt, idcol = source_dt[['UT']], splitcol, delimiter = ';') {
  if (splitcol == 'C1') {
    split.c1(source_dt = source_dt, idcol = idcol, splitcol = 'C1', delimiter = delimiter)
  } else {
    split.simple(source_dt = source_dt, idcol = idcol, splitcol = splitcol, delimiter = delimiter)
  }
}
```

Let's read the data downloaded as "Tab-delimited (Win, UTF-8)"

```r
system.time(lis_07_14 <- read.wos('LIS_2007-2014'))
```

```
##    user  system elapsed 
##   86.04    1.00   87.06
```

```r
str(lis_07_14)
```

```
## Classes 'data.table' and 'data.frame':	95821 obs. of  59 variables:
##  $ PT: chr  "J" "J" "J" "J" ...
##  $ AU: chr  "Ratzan, SC" "Lee, CJ; Long, M; Slater, MD; Song, W" "Lee, HR; Lee, HE; Choi, J; Kim, JH; Han, HL" "Moldovan-Johnson, M; Martinez, L; Lewis, N; Freres, D; Hornik, RC" ...
##  $ BA: chr  NA NA NA NA ...
##  $ BE: chr  NA NA NA NA ...
##  $ GP: chr  NA NA NA NA ...
##  $ AF: chr  "Ratzan, Scott C." "Lee, Chul-joo; Long, Marilee; Slater, Michael D.; Song, Wen" "Lee, Hye-Ryeon; Lee, Hye Eun; Choi, Jounghwa; Kim, Jang Hyun; Han, Hae Lin" "Moldovan-Johnson, Mihaela; Martinez, Lourdes; Lewis, Nehama; Freres, Derek; Hornik, Robert C." ...
##  $ BF: chr  NA NA NA NA ...
##  $ CA: chr  NA NA NA NA ...
##  $ TI: chr  "Twenty Years Later: Ebola, AIDS, BSE and NCDs-What Have We Learned?" "Comparing Local TV News with National TV News in Cancer Coverage: An Exploratory Content Analysis" "Social Media Use, Body Image, and Psychological Well-Being: A Cross-Cultural Comparison of Korea and the United States" "The Role of Patient-Clinician Information Engagement and Information Seeking from Nonmedical Channels in Fruit and Vegetable In"| __truncated__ ...
##  $ SO: chr  "JOURNAL OF HEALTH COMMUNICATION" "JOURNAL OF HEALTH COMMUNICATION" "JOURNAL OF HEALTH COMMUNICATION" "JOURNAL OF HEALTH COMMUNICATION" ...
##  $ SE: chr  NA NA NA NA ...
##  $ BS: chr  NA NA NA NA ...
##  $ LA: chr  "English" "English" "English" "English" ...
##  $ DT: chr  "Editorial Material" "Article" "Article" "Article" ...
##  $ CT: chr  NA NA NA NA ...
##  $ CY: chr  NA NA NA NA ...
##  $ CL: chr  NA NA NA NA ...
##  $ SP: chr  NA NA NA NA ...
##  $ HO: chr  NA NA NA NA ...
##  $ DE: chr  NA NA NA NA ...
##  $ ID: chr  "COMMUNICATION; HEALTH" "TELEVISION-NEWS; HEALTH NEWS; FATALISTIC BELIEFS; US HEALTH; PREVENTION; SCIENCE; NEWSPAPERS; REPORTERS; AGREEMENT" "SELF-ESTEEM; COLLEGE-WOMEN; DIETARY RESTRAINT; ADOLESCENT GIRLS; DISSATISFACTION; WEIGHT; SATISFACTION; DEPRESSION; THIN; INDIV"| __truncated__ "LIFE-STYLE BEHAVIORS; FAMILY/FRIENDS INCREASES; MEDIATION ANALYSIS; HEALTH; RISK; COMMUNICATION; CONSUMPTION; PREVENTION; POPUL"| __truncated__ ...
##  $ AB: chr  NA "The authors compared local TV news with national TV news in terms of cancer coverage using a nationally representative sample o"| __truncated__ "This study examined the relationships among social media use for information, self-status seeking and socializing, body image, "| __truncated__ "Previous research suggests positive effects of health information seeking on prevention behaviors such as diet, exercise, and f"| __truncated__ ...
##  $ C1: chr  NA "[Lee, Chul-joo] Univ Illinois, Dept Commun, Urbana, IL 61801 USA; [Long, Marilee] Colorado State Univ, Dept Journalism & Tech C"| __truncated__ "[Lee, Hye-Ryeon; Lee, Hye Eun] Univ Hawaii Manoa, Dept Communicol, Coll Arts & Humanities, Honolulu, HI 96822 USA; [Choi, Joung"| __truncated__ "[Moldovan-Johnson, Mihaela] Texas Dept State Hlth Serv, Austin, TX USA; [Martinez, Lourdes] Michigan State Univ, Dept Commun, E"| __truncated__ ...
##  $ RP: chr  NA "Lee, CJ (reprint author), Univ Illinois, Dept Commun, 3001 Lincoln Hall,702 South Wright St, Urbana, IL 61801 USA." "Lee, HR (reprint author), Univ Hawaii Manoa, Dept Communicol, 2560 Campus Rd,George Hall 329, Honolulu, HI 96822 USA." "Hornik, RC (reprint author), Univ Penn, Ctr Excellence Canc Commun Res, Annenberg Sch Commun, 3620 Walnut St, Philadelphia, PA "| __truncated__ ...
##  $ EM: chr  NA "chuljoo@illinois.edu" "hyeryeon@hawaii.edu" "rhornik@asc.upenn.edu" ...
##  $ RI: chr  NA NA NA NA ...
##  $ OI: chr  NA NA NA NA ...
##  $ FU: chr  NA NA NA NA ...
##  $ FX: chr  NA NA NA NA ...
##  $ CR: chr  "Altman L. K., 2014, NY TIMES, pD2; Ratzan S. C., 1998, MAD COW CRISIS HLTH; Ratzan SC, 2006, J HEALTH COMMUN, V11, P131, DOI 10"| __truncated__ "Banerjee M, 1999, CAN J STAT, V27, P3, DOI 10.2307/3315487; Bodle JV, 1996, JOURNALISM MASS COMM, V73, P672; Bradshaw KA, 2005,"| __truncated__ "Anton S D, 2000, Eat Behav, V1, P153, DOI 10.1016/S1471-0153(00)00015-5; Arbuckle J. L., 2006, AMOS VERSION 7 0 COM; Blowers Lu"| __truncated__ "AJZEN I, 1991, ORGAN BEHAV HUM DEC, V50, P179, DOI 10.1016/0749-5978(91)90020-T; Allison P. D., 2001, MISSING DATA QUANTIT; Ame"| __truncated__ ...
##  $ NR: int  11 41 74 72 62 39 21 83 14 17 ...
##  $ TC: int  0 0 2 0 0 1 0 0 0 0 ...
##  $ Z9: int  0 0 2 0 0 1 0 0 0 0 ...
##  $ PU: chr  "TAYLOR & FRANCIS INC" "TAYLOR & FRANCIS INC" "TAYLOR & FRANCIS INC" "TAYLOR & FRANCIS INC" ...
##  $ PI: chr  "PHILADELPHIA" "PHILADELPHIA" "PHILADELPHIA" "PHILADELPHIA" ...
##  $ PA: chr  "530 WALNUT STREET, STE 850, PHILADELPHIA, PA 19106 USA" "530 WALNUT STREET, STE 850, PHILADELPHIA, PA 19106 USA" "530 WALNUT STREET, STE 850, PHILADELPHIA, PA 19106 USA" "530 WALNUT STREET, STE 850, PHILADELPHIA, PA 19106 USA" ...
##  $ SN: chr  "1081-0730" "1081-0730" "1081-0730" "1081-0730" ...
##  $ EI: chr  "1087-0415" "1087-0415" "1087-0415" "1087-0415" ...
##  $ BN: chr  NA NA NA NA ...
##  $ J9: chr  "J HEALTH COMMUN" "J HEALTH COMMUN" "J HEALTH COMMUN" "J HEALTH COMMUN" ...
##  $ JI: chr  "J. Health Commun." "J. Health Commun." "J. Health Commun." "J. Health Commun." ...
##  $ PD: chr  "DEC 2" "DEC 2" "DEC 2" "DEC 2" ...
##  $ PY: int  2014 2014 2014 2014 2014 2014 2014 2014 2014 2014 ...
##  $ VL: chr  "19" "19" "19" "19" ...
##  $ IS: chr  "12" "12" "12" "12" ...
##  $ PN: chr  NA NA NA NA ...
##  $ SU: chr  NA NA NA NA ...
##  $ SI: chr  NA NA NA NA ...
##  $ MA: chr  NA NA NA NA ...
##  $ BP: chr  "1327" "1330" "1343" "1359" ...
##  $ EP: chr  "1329" "1342" "1358" "1376" ...
##  $ AR: chr  NA NA NA NA ...
##  $ DI: chr  "10.1080/10810730.2014.989098" "10.1080/10810730.2014.894598" "10.1080/10810730.2014.904022" "10.1080/10810730.2014.906521" ...
##  $ D2: chr  NA NA NA NA ...
##  $ PG: chr  "3" "13" "16" "18" ...
##  $ WC: chr  "Communication; Information Science & Library Science" "Communication; Information Science & Library Science" "Communication; Information Science & Library Science" "Communication; Information Science & Library Science" ...
##  $ SC: chr  "Communication; Information Science & Library Science" "Communication; Information Science & Library Science" "Communication; Information Science & Library Science" "Communication; Information Science & Library Science" ...
##  $ GA: chr  "AW1QJ" "AW1QJ" "AW1QJ" "AW1QJ" ...
##  $ UT: chr  "WOS:000346064100001" "WOS:000346064100002" "WOS:000346064100003" "WOS:000346064100004" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```
As can be seen, in this case it is only necessary to specify the directory where the text files have been saved. If the format parameter is not specified in the function call, read.wos tries to read files in "Tab-delimited (Win, UTF-8)" format.

Additionally, the function system.time() informs us that it takes less than 1.5 minutes to read almost 100,000 records.

All fields are read as text fields except the NR, TC, Z9 and PY fields, which are read as integers. The meaning of each field name can be found [here](http://images.webofknowledge.com/WOKRS56B5/help/WOS/hs_wos_fieldtags.html).

Now, let's read the records downloaded as "Plain Text"


```r
system.time(lis_99_06 <- read.wos('LIS_1999-2006', format = 'plain_text'))
```

```
##    user  system elapsed 
##  727.85    1.59  729.99
```

```r
str(lis_99_06)
```

```
## Classes 'data.table' and 'data.frame':	90913 obs. of  59 variables:
##  $ PT: chr  "J" "J" "J" "J" ...
##  $ AU: chr  "[Anonymous]" "Smith, S" "Bernstein, J" "Bates, ME" ...
##  $ BA: chr  NA NA NA NA ...
##  $ BE: chr  NA NA NA NA ...
##  $ GP: chr  NA NA NA NA ...
##  $ AF: chr  "[Anonymous]" "Smith, Steve" "Bernstein, Jared" "Bates, Mary Ellen" ...
##  $ BF: chr  NA NA NA NA ...
##  $ CA: chr  NA NA NA NA ...
##  $ TI: chr  "Show me state" "The revenue streams of 2007" "InSites" "Info pro on the edge" ...
##  $ SO: chr  "ECONTENT" "ECONTENT" "ECONTENT" "ECONTENT" ...
##  $ SE: chr  NA NA NA NA ...
##  $ BS: chr  NA NA NA NA ...
##  $ LA: chr  "English" "English" "English" "English" ...
##  $ DT: chr  "Editorial Material" "Editorial Material" "Article" "Editorial Material" ...
##  $ CT: chr  NA NA NA NA ...
##  $ CY: chr  NA NA NA NA ...
##  $ CL: chr  NA NA NA NA ...
##  $ SP: chr  NA NA NA NA ...
##  $ HO: chr  NA NA NA NA ...
##  $ DE: chr  NA NA NA NA ...
##  $ ID: chr  NA NA NA NA ...
##  $ AB: chr  NA NA NA NA ...
##  $ C1: chr  NA NA NA NA ...
##  $ RP: chr  NA NA NA NA ...
##  $ EM: chr  NA "popeyesmith@comcast.net" NA "mbates@batesinfo.com" ...
##  $ RI: chr  NA NA NA NA ...
##  $ OI: chr  NA NA NA NA ...
##  $ FU: chr  NA NA NA NA ...
##  $ FX: chr  NA NA NA NA ...
##  $ CR: chr  NA NA NA NA ...
##  $ NR: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ TC: int  0 0 1 0 0 0 0 0 0 0 ...
##  $ Z9: int  0 0 1 0 0 0 0 0 0 0 ...
##  $ PU: chr  "ONLINE INC" "ONLINE INC" "ONLINE INC" "ONLINE INC" ...
##  $ PI: chr  "WILTON" "WILTON" "WILTON" "WILTON" ...
##  $ PA: chr  "213 DANBURY RD, WILTON, CT 06897-4007 USA" "213 DANBURY RD, WILTON, CT 06897-4007 USA" "213 DANBURY RD, WILTON, CT 06897-4007 USA" "213 DANBURY RD, WILTON, CT 06897-4007 USA" ...
##  $ SN: chr  "1525-2531" "1525-2531" "1525-2531" "1525-2531" ...
##  $ EI: chr  NA NA NA NA ...
##  $ BN: chr  NA NA NA NA ...
##  $ J9: chr  "ECONTENT" "ECONTENT" "ECONTENT" "ECONTENT" ...
##  $ JI: chr  "Econtent" "Econtent" "Econtent" "Econtent" ...
##  $ PD: chr  "DEC" "DEC" "DEC" "DEC" ...
##  $ PY: int  2006 2006 2006 2006 2006 2006 2006 2006 2006 2006 ...
##  $ VL: chr  "29" "29" "29" "29" ...
##  $ IS: chr  "10" "10" "10" "10" ...
##  $ PN: chr  NA NA NA NA ...
##  $ SU: chr  NA NA NA NA ...
##  $ SI: chr  NA NA NA NA ...
##  $ MA: chr  NA NA NA NA ...
##  $ BP: chr  "7" "10" "12" "17" ...
##  $ EP: chr  "7" "10" "16" "17" ...
##  $ AR: chr  NA NA NA NA ...
##  $ DI: chr  NA NA NA NA ...
##  $ D2: chr  NA NA NA NA ...
##  $ PG: chr  "1" "1" "5" "1" ...
##  $ WC: chr  "Information Science & Library Science" "Information Science & Library Science" "Information Science & Library Science" "Information Science & Library Science" ...
##  $ SC: chr  "Information Science & Library Science" "Information Science & Library Science" "Information Science & Library Science" "Information Science & Library Science" ...
##  $ GA: chr  "111JX" "111JX" "111JX" "111JX" ...
##  $ UT: chr  "WOS:000242448600001" "WOS:000242448600002" "WOS:000242448600003" "WOS:000242448600004" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```
Reading text files in "Plain Text" format is a bit more complex, since the content for each field has to be parsed first. Because of that, this process takes a bit longer than reading tabulated data: in this case it takes over 12 minutes to read 90,000 WoS records.

Now we can merge both sets:

```r
lis_99_14 <- rbind(lis_99_06,lis_07_14)
total_records <- dim(lis_99_14)[1]
```
A total of 186734 records have been imported.

Once all records are imported, the function split.field() allows us to create indexes out of those fields that contain various elements separated by some delimiter. Some such fields are: AU and AF (authors), C1 (authors / addresses), CR (cited references), WC and SC (subject categories). Let's see how we can create an author index:

```r
system.time(authors <- split.field(lis_99_14, splitcol = 'AU'))
```

```
##    user  system elapsed 
##   17.14    0.72   17.87
```

```r
head(authors)
```

```
##                     UT           AU
## 1: WOS:000242448600001  [Anonymous]
## 2: WOS:000242448600002     Smith, S
## 3: WOS:000242448600003 Bernstein, J
## 4: WOS:000242448600004    Bates, ME
## 5: WOS:000242448600005     Doyle, B
## 6: WOS:000242448600006 Bernstein, J
```
Only two parameters are needed in the split.field() function: 

1. source_dt: the name of the data.table from which we want to create an index.
2. splitcol: the name of the field we want to split. 

There are other two parameters that are filled by default, but could be changed at need: 

3. idcol: which is the name of the field that we want to use as an external key for the index. By default it is set to 'UT', which is the identifier WoS assigns to each of the records in its database.
4. delimiter: the character used as a delimiter in the field. By default, it is set to ';' (semicolon).

Now let's try to create an index from the C1 (author / addresses) field:


```r
system.time(addresses <- split.field(lis_99_14, splitcol = 'C1'))
```

```
##    user  system elapsed 
##   52.46    0.70   53.17
```

```r
tail(addresses)
```

```
##                     UT                author
## 1: WOS:000253974100123       Fernandez, Eloi
## 2: WOS:000253974100123        Borges, Marcia
## 3: WOS:000253974100125          Sharp, Peter
## 4: WOS:000253974100143 van Reijsen, Jurriaan
## 5: WOS:000253974100152      Wildner, Stephen
## 6: WOS:000253974100152         Lehner, Franz
##                                     address
## 1:    Catholic Univ, Rio De Janeiro, Brazil
## 2:    Catholic Univ, Rio De Janeiro, Brazil
## 3: Regents Coll & MaKE Inc, London, England
## 4:    Andarr Org Serv, Utrecht, Netherlands
## 5:             Univ Passau, Passau, Germany
## 6:             Univ Passau, Passau, Germany
```
The function call is exactly the same that in the case of the 'AU' field, but in this case, the output is a data.table with three columns instead of two: UT, author, and address. The split.field() function detects when the 'C1' field is selected, and applies a slightly different set of commands from the ones used for simple multiple-element fields.

Once the data has been imported to an R data.table, it is easy to make computations that would take a lot of time to do with the tools provided by Web of Science's web interface. For example, we can compute the h index of every journal with a single command:

```r
# First we've got to define a function to compute de h index
hindex <- function(int_vector) {
  int_vector <- sort(int_vector, decreasing = TRUE)
  i = 1L
  if (sum(int_vector) != 0) {
    for (j in int_vector) {
      if (j >= i) {
        h <- i
        i  <- i + 1L
      } else {
        break
      }
    }
  } else {
    h  <- 0L
  }
  h
}

# Now we're ready to compute the h index
journals_hindex <- lis_99_14[, hindex(TC), by = SO]

# Lastly, we order the resulting data.table by h index, decreasingly
setorder(journals_hindex, -V1)
setnames(journals_hindex, "V1", "hindex")
head(journals_hindex,10)
```

```
##                                                                         SO
##  1:                                                          MIS QUARTERLY
##  2:                JOURNAL OF THE AMERICAN MEDICAL INFORMATICS ASSOCIATION
##  3:                                               INFORMATION & MANAGEMENT
##  4:                                           INFORMATION SYSTEMS RESEARCH
##  5: JOURNAL OF THE AMERICAN SOCIETY FOR INFORMATION SCIENCE AND TECHNOLOGY
##  6:                                                         SCIENTOMETRICS
##  7:                              JOURNAL OF MANAGEMENT INFORMATION SYSTEMS
##  8:              INTERNATIONAL JOURNAL OF GEOGRAPHICAL INFORMATION SCIENCE
##  9:                                    INFORMATION PROCESSING & MANAGEMENT
## 10:                                        JOURNAL OF HEALTH COMMUNICATION
##     hindex
##  1:    100
##  2:     83
##  3:     77
##  4:     73
##  5:     66
##  6:     63
##  7:     61
##  8:     52
##  9:     51
## 10:     46
```
While it's true that a deeper pre-processing of the data would have been necessary before computing these sort of indicators (in this case, journal names should have been better normalized), R enables users to perform quick and powerful exploratory analyses with very little effort.

### Acknowledgments
Alberto Martín-Martín enjoys a four-year doctoral fellowship (FPU2013/05863) granted by the Spanish Ministry of Education, Culture and Sports.

### References

M Dowle, T Short, S Lianoglou, A Srinivasan, R Saporta, E Antonyan (2014). data.table: Extension of data.frame. Version 1.9.4. Available at: http://cran.r-project.org/web/packages/data.table/index.html (Accessed 2015-03-09 13:06:31)
