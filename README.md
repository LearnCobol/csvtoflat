# csvtoflat
canonical COBOL terminal program to convert comma-delimited (.csv) file into flatfile text format
tested to compile on Slackware (and it is hoped any) Linux terminal command line GNU Cobol compiler
cobc -x csvtoflat.cbl to compile
./csvtoflat /path to csv file/file.csv will out out put flat formatted csv data to file flatfile.dat
in same directory/folder as binary
This may also compile under Micro Focus Cobol for Windows and the .exe may work as intended in the Windows NT command
terminal, I have not checked recently, even fewer guarantees than under Linux + GNU Cobol.
