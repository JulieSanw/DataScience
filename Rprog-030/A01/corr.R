source("complete.R")

corr <- function(directory, threshold = 0) {
     
	    ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!

        path <- paste(getwd(), directory, sep="/")    
        c <- numeric(0)

        for (i in 1:332) {
            data <- na.omit(read.csv(paste0(getwd(),"/",directory,"/",dir(path))[i])) 
            if (nrow(data) >= threshold) {
                n <- cor(data["sulfate"], data["nitrate"])
                if (!is.na(n))
                    c <- append(c, n)
            }
        }

        c
}