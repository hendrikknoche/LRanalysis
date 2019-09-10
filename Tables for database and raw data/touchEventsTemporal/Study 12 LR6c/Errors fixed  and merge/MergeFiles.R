#Renember to set working directory to the location of the .r file

#list all files in folder
file_list <- list.files(path=getwd(), pattern="*.csv", full.names=T, recursive=FALSE)

#create a blank dataframe 
results_final = data.frame()
temp = data.frame()

#create a "for" loop where all CSV files in folder will be processed and row bound
for (i in file_list) {
  # read the file into a new verible called temp
  temp <- read.csv(i, header = TRUE,  sep = ";")
  #Using rbind function to merge the result_final and temp
  results_final<-rbind(results_final,temp)
}

#create a CSV file with the row bound results
write.csv(results_final,"Study12TouchEventTemporal.csv", row.names=FALSE)

