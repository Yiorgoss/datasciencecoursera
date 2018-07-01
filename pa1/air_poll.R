##date, sulphate, nitrate
poll_mean <- function( direc="specdata", pol="sulfate", id=1:10 ){

    folder <- "/Users/squanchyjr/Programming/R/datasciencecoursera/pa1/specdata/"
    file_list <- list.files(path=folder, pattern="*.csv")
    file_list <- file_list[id]

    data <- do.call("rbind", 
                    lapply( file_list, 
                           function(x)
                               read.csv(paste(folder, x, sep=""),
                                        stringsAsFactors=FALSE)))
    data <- data[[pol]]
    mean <- mean( data, na.rm=TRUE )
    print(mean)
}

complete <- function( direc="specdata", id=30:25 ){
    folder <- "/Users/squanchyjr/Programming/R/datasciencecoursera/pa1/specdata/"
    file_list <- list.files(path=folder, pattern="*.csv")

    num <- sapply( id, function(x){
                      data <- read.csv(paste(folder, file_list[x], sep=""))
                      sum( complete.cases( data[c("nitrate", "sulfate")] ) )
                    } )
    print(num)
    data.frame(id=id, num=num)
}

corr <- function( direc="specdata", threshold=0 ){
    folder <- "/Users/squanchyjr/Programming/R/datasciencecoursera/pa1/specdata/"
    files <- list.files("specdata/", pattern=".csv", full.names=T)

    cor_vec <- 0[-1]
    i <- 0
    for(file in files){
        cur <- read.csv(file)
        if(sum(complete.cases(cur)) < threshold){
            next()
        }
        cur <- cur[complete.cases(cur), ]
        new <- cor(cur$sulfate, cur$nitrate)
        cor_vec <- c(cor_vec, new)

    }
    cor_vec 
}


