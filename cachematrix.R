## About the function: 
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating
## the name of the pollutant for which we will calculate the
## mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers to be used
## Return the mean of the pollutant across all monitors list
## in the 'id' vector (ignoring NA values)
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## get filenames 
  f_names <- list.files(path=directory, pattern="*.csv")
  
  ## initiate pollutant values vector 
  pollutant_vals <- vector()
  
  ## loop over all id's
  for(i in id) {
    
    ## construct currnet file name
    f_names <- sprintf("%03d.csv", i)
    f_path <- paste(directory, f_names, sep="/")
    
    ## load current file data
    spec_data <- read.csv(f_path)
    
    ## Select required column
    data <- spec_data[,pollutant]
    
    ## ignore any missing values
    data <- data[!is.na(data)]
    
    ## append to pollutant values vector
    pollutant_vals <- c(pollutant_vals, data)
  }
  
  ## return the mean of the pollutant across all monitors list
  mean(pollutant_vals)
}
