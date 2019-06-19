#list all files in folder
file_list <- list.files(path="D:/Dropbox/Medialogy/Work with Hendrik/GitHub_DATA/LRanalysis/data/TargetSizeTestDataOutsetCenterGoal/Data/With T", pattern="*.csv", full.names=TRUE, recursive=FALSE)


#create a blank dataframe 
results_final = data.frame()

#create a "for" loop where all CSV files in folder will be processed and row bound
for (i in file_list) {
  
  # read the file into results
  results <- read.csv(i, header = TRUE,  sep = ",")
  results_final<-rbind(results_final,results)
}

#create a CSV file with the row bound results
write.csv(results_final,"results_final.csv")


