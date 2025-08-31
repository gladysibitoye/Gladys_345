# Assignment

# Write a function classify_gene() 

# that takes:
#   - logFC (log2FoldChange)
#   - padj  (adjusted p-value)

# and returns:
#   - "Upregulated" if log2FC > 1 and padj < 0.05
#   - "Downregulated" if log2FC < -1 and padj < 0.05
#   - "Not_Significant" otherwise


classify_gene <- function(logFC,padj){
  if(logFC > 1 && padj < 0.05){
    
    return("Upregulated")
    
  }else if(logFC < -1 && padj < 0.05) {
    
    return("Downregulated")
    
  } else {
    
    return("Not_Significant")
  }
  
}

# Apply it in a for-loop to process both datasets (DEGs_data_1.csv, DEGs_data_2.csv)
#   - Replace missing padj values with 1
#   - Add a new column 'status'
#   - Save processed files into Results folder
#   - Print summary counts of significant, upregulated, and downregulated genes
#   - Use table() for summaries


input_dir <- "data"
output_dir <- "result"

# create output folder if not already exist
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}


# List which file to process

files_to_process <- c("DEGs_Data_1.csv","DEGs_Data_2.csv") # names of files

# Prepare empty list to store result in R

result_list <- list()

#Automating for loop

for (file_names in files_to_process) {
  cat("\nprocessing:",file_names,"\n")
  input_file_path <- file.path(input_dir,file_names)
  
  # Import dataset
  data <- read.csv(input_file_path,header = TRUE)
  cat("File imported.checking for missing values...\n")
  
  #1. Handling missing values
  
  if("padj"%in% names(data)){
    missing_count <- sum(is.na(data$padj))
    cat("Missing values in 'padj':",missing_count, "\n")
    data$padj[is.na(data$padj)] <- 1
  } 
  
  if("logFC"%in% names(data)){
    missing_count <- sum(is.na(data$logFC))
    
    cat("Missing values in 'logFC':",missing_count, "\n")
    data$logFC[is.na(data$logFC)] <- mean(data$logFC,na.rm=TRUE)
  }
  
  
  #2. Adding a new column called 'status'
  
  # creating a empty column
  data$status <-NA
  
  # filling the column using the for loop
  
  for (i in 1:nrow(data)){
    data$status[i]
    if (data$logFC[i] > 1 & data$padj[i] < 0.05) {
      data$status[i] <- "Upregulated"
    } else if (data$logFC[i] < -1 & data$padj[i] < 0.05) {
      data$status[i] <- "Downregulated"
    } else {
      data$status[i] <- "Not_Significant"
    }
    # summary count
    cat("Summary counts for", file_names, ":\n")
    print(table(data$status))
  }
  
  
  
  #3. Saving processed files into Results folder
  
  result_list[[file_names]] <- data
  # save result in result folder
  output_file_path <- file.path(output_dir,paste0("DEGs_results",file_names))
  write.csv(data,output_file_path,row.names = FALSE)
  #write.csv(data,here("output_file_path","data.csv"),row.names=FALSE)
  cat("Results saved to:",output_file_path,"\n")
  
}

result_1<- result_list[[1]]
result_2 <- result_list[[2]]


