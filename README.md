# read.wos.R

In this repository I'll be posting functions to read data exported from the Web of Science into R

## Required packages

data.table

## Functions

### read.wos
reads data from Web of Science exported in "Tab-delimited (Win, UTF-8)" or "Plain Text" format, and converts it into a data.table. 
#### Arguments:
* path: path to the folder where the exported files are. Default: "./files/"
* format: format of the exported files: "tab_win_utf8" or "plain_text". Default: "tab_win_utf8"

### split.field
splits a field with multiple elements (authors, subject categories, addresses, etc... and creates a new data.table containing each element in a separate row, keeping its relationship to the original record through the ID column.
#### Arguments:
* source_dt: data.table from which to extract the data. No default.
* idcol: name of the variable in the data.table that will serve as the identifier (external key). Default: "UT"
* splitcol: name of the variable in the data.table that will be split. No default. Possible values: "AU", "AF", "C1", "WC", "SC". If "C1" is selected, the resulting data.table will have three columns: the ID column, author, and address.
* delimiter: character that separates the elements inside splitcol. Default: ";"

## Update log

v0.5	Added split.c1 function to split the C1 WoS field (authors/addresses), split.field() and read.wos() wrappers

v0.4    Added split.field function to split author, category etc. fields. It creates a new table containing each element in a separate row, keeping its relationship to the original record through the ID column.

v0.3	Replaced the method for building the table, from data.frame/rbind to data.table. Performance is greatly improved.

v0.2    Added read.wos.tabwin.utf8

v0.1	First version of the function to import data from Plain Text files. It works but it's pretty slow... wonder how it could be optimised.