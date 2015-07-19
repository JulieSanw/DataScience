complete <- function(directory, id = 1:332) {

        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

    path <- paste(getwd(), directory, sep="/")    
    nobs<-vector()

    for(i in id){
        n <- read.csv(paste0(getwd(),"/",directory,"/",dir(path))[i])
        c <- complete.cases(n)
        nobs[i] <- sum(c)
    }

    colnames <- c("date", "sulfate", "nitrate", "id") 
    data.frame(id = id, nobs = nobs[id])

}