# read.wos.R

In this repository I'll be posting functions to read data exported from the Web of Science into R

## Required packages

data.table

## Functions

read.wos.plain: it reads data from Web of Science exported in "Plain Text" format, and converts it into a data.table. 

read.wos.tabwin.utf8: it reads data from Web of Science exported in "Tab-delimited (Win, UTF-8)" format, and converts it into a data.frame.

### Arguments

filefolder: path to the folder that contains the list of files. Default is "./files".
fields_path: list of fields you want to extract from the files. Default is "fields.txt".
nrows: number of records to be processed. Default is 10,000,000 (it will remove empty records).

## Update log

v0.3	replaced the method for building the table, from data.frame/rbind to data.table. Performance is greatly improved.

v0.2    Added read.wos.tabwin.utf8

v0.1	First version of the function to import data from Plain Text files. It works but it's pretty slow... wonder how it could be optimised.