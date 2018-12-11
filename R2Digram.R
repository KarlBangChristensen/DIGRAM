# Auhtor: Anne Lyngholm Sørensen
# Description: Create files for IR analysis in Digram

library(data.table)
library(ggplot2)
library(eRm)
library(psychotools)

source("~/Documents/Karl/DISABKIDS/code/prepareDisab.R")

dC11 <- data.frame(fullChild[,c("ID", "treat", "org", "gAge", "gHbA1c", "Gender",
                                "X_1_G_CH", "X_2_G_CH", "X_3_G_CH",
                                "X_4_G_CH", "X_5_G_CH", "X_6_G_CH")])
dC12 <- data.frame(fullChild[,c("ID", "treat", "org", "gAge", "gHbA1c", "Gender",
                                "X_1_G_CH", "X_2_G_CH", "X_3_G_CH",
                                "X_4_G_CH", "X_5_G_CH", "X_6_G_CH")])

data <- dC11
idvar <- "ID"
exo <- c("treat", "org", "gAge", "gHbA1c", "Gender")
items <- c("X_1_G_CH", "X_2_G_CH", "X_3_G_CH",
           "X_4_G_CH", "X_5_G_CH", "X_6_G_CH")


RDigram(dC11, idvar = "ID", items = c("X_1_G_CH", "X_2_G_CH", "X_3_G_CH",
                                      "X_4_G_CH", "X_5_G_CH", "X_6_G_CH"), exo = 
          c("treat", "org", "gAge", "gHbA1c", "Gender"), folder = "D:\\GRM3\\", 
        OS = "Windows")

RDigram <- function(data, idvar, items, exo, folder, model, 
                    personpar = FALSE, reduce = FALSE, LD = FALSE, DIF = FALSE, 
                    GlobalTests = FALSE, Itemfits = FALSE, ARI = FALSE, 
                    name = "DIGRAM", deletetemp = TRUE, commandfile = TRUE, OS = "Linux"){
  # return three files (.imp, .vmp, .exe) for creating an analysis in DIGRAM when
  # data is initially prepared in R
  #
  # args: 
  #   data: a data.frame containing the repsonses to the questionaire (items), id variable and exo. variables
  #   idvar: name of the id variable from data
  #   items: the names of the items from data
  #   
  # returns:
  #   three files in a specified folder
  
  if(class(data) != "data.frame"){
    stop("data must be a data.frame")
  }
  
  if(OS == "Windows"){
    delim <- "\n"
  } else {
    delim <- "\r\n"
  }
  
  # create and export the .imp file (contains folder path, name of program (default is DIGRAM),
  # and the command for opening)
  outimp <- paste(folder, delim, name, delim, "-", delim, name, ".cmd", sep = "")
  destpathimp <- paste(folder, "/", name, ".imp", sep = "")
  write(outimp, destpathimp)
  
  # create and export the .imv file (contains the names of the items, names of each item level, 
  # name of the exo var and the names of each level)
  ncolI <- dim(data[,items])[2]
  ncolE <- dim(data[,exo])[2]
  
  # item category range
  rangeItems <- sapply(1:ncolI, function(x){
    return(unique(data[,items][,x]))
  })
  
  minI <- min(unlist(rangeItems), na.rm = TRUE)
  maxI <- max(unlist(rangeItems), na.rm = TRUE)
  rowsItems <- paste(minI:maxI, ",", numbers2words(minI:maxI), sep = "")
  
  # levels exo. variables
  ExoMatrix <- sapply(1:ncolE, function(x){
    as.numeric(factor(data[,exo][[x]]))
  })
  rangeExo <- sapply(1:ncolE, function(x){
    return(sort(unique(ExoMatrix[,x])))
  })
  rowsExo <- sapply(1:ncolE, function(x){
    paste(unlist(rangeExo[[x]]), ",", numbers2words(unlist(rangeExo[[x]])), sep = "")
  })
  
  lrow <- letters[1:(ncolI+ncolE)]
  namerow <- c(items, exo)
  
  outimv <- paste(
    paste(
      paste(lrow[1:ncolI], namerow[1:ncolI], paste(rowsItems, collapse = ","), sep = ","),
      delim, sep =""), 
    collapse = "")
  outimv <- paste(outimv, paste(paste(lrow[(ncolI+1):(ncolI+ncolE)], namerow[(ncolI+1):(ncolI+ncolE)],
        sapply( rowsExo, paste0, collapse=","), sep = ","), delim, sep= "", collapse = ""),
        sep = "", collapse = "")
  
  if(OS == "Windows"){
    outimv <- gsub('.{1}$', '', outimv)
  } else {
    outimv <- gsub('.{2}$', '', outimv)
  }
  
  destpathimv <- paste(folder, "/", name, ".imv", sep = "")
  write(outimv, destpathimv)
  
  # create and export the .csv file (contains the needed data)
  csvdata <- data[,colnames(data) %in% c(idvar, items)]
  csvdata$ID <- 1:dim(data)[1]
  csvdata <- cbind(csvdata, ExoMatrix)
  #colnames(csvdata) <- c("ID", items, exo)
  csvdata <- rbind(c("ID", items, exo), csvdata)
  
  df_args <- c(csvdata, sep=",")
  csvdata <- do.call(paste, df_args)
  csvdata <- gsub("NA","", csvdata, perl = T)
  
  csvdata <- paste(csvdata, collapse = delim)
  
  destpathcsv <- paste(folder, "/", name, ".csv", sep = "")
  write(csvdata, destpathcsv)
  }

numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  helper <- function(x){
    
    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and", 
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],"," ,
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    #Tidy leading/trailing whitespace, space before comma
    text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
    #Clear any trailing " and"
    text=gsub(" and$","",text)
    #Clear any trailing comma
    gsub("\ *,$","",text)
  }  
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
  #Disable scientific notation
  opts <- options(scipen=100) 
  on.exit(options(opts)) 
  ones <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine") 
  names(ones) <- 0:9 
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9 
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety") 
  names(tens) <- 2:9 
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")     
  if (length(x) > 1) return(trim(sapply(x, helper)))
  helper(x)
}

