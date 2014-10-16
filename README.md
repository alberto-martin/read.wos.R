# read.wos.R

In this repository I'll be posting functions to read data exported from the Web of Science into R

## read.wos.plain

Reads data from Web of Science exported in "Plain Text" format, and converts it into a data.frame. 

Through arguments in the function, you can specify the path to the folder that contains the list of files (filefolder), as well as the list of fields you want to extract from the files (fields_path). By default, the file folder is "./files" and the list of fields is stored in "fields.txt".


## Update log

v0.1	First version of the function to import data from Plain Text files. It works but it's pretty slow... wonder how it could be optimised.