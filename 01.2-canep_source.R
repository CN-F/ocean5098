##########################################
### canephora source
##########################################
library(readxl)
rm(list=ls())

#####################
### read GPS data from source data
source_path<- "C:\\Coffea_research\\D01.0-Canep_data\\gcb16191-sup-0003-tables1.xlsx"
GPS_table<- read_xlsx(source_path, sheet=2)
GPS_table$Record.ID

#####################
### read sample name (accession number of canephora)
sample_path<- "C:\\Coffea_research\\D00.1-sample_from_Zana\\Samples_summary_noLib.xlsx"
sample_table<- read_excel(sample_path)
canep_acsn_list<- sample_table[sample_table$Country == "canephora",]$Accession_ID
