# Environment
LoadPackage <- function(name)
{
  if(!require(name, character.only=TRUE))
    install.packages(name, dependencies=TRUE, repos='http://cran.rstudio.com/')
  library(name, character.only=TRUE)
}

LoadPackages <- function(names)
{
  for(name in names)
  {
    LoadPackage(name)
  }
}

ConfigureEnv <- function()
{
  LoadPackages(c("dplyr", "reshape2", "ggplot2", "zoo"))
  Sys.setlocale("LC_TIME", "C")
  setwd("pwd")
}

# Input
ReadCsv <- function(filename)
{
  return(read.csv(filename, header = T, sep = ",", dec = "."))
}

SummarizeDiff <- function(value_before, value_after, name)
{
  if (value_after > value_before)
    return(paste("added", value_after-value_before, name))
  if (value_before > value_after)
    return(paste("lost", value_before-value_after, name))
  return("")
}

SummarizeColDiff <- function(before, after)
{
  bcols = colnames(before)
  acols = colnames(after)
  if (length(acols) > length(bcols))
    return(paste("added ", length(acols)-length(bcols), " cols [", setdiff(acols, bcols), "]", sep=""))
  if (length(bcols) > length(acols))
    return(paste("lost ", length(bcols)-length(acols), " cols [", setdiff(bcols, acols), "]", sep=""))
  return("")
}

SummarizeNasDiff <- function(before, after)
{
  bnas <- sum(colSums(is.na(before)))
  anas <- sum(colSums(is.na(after)))
  diff <- SummarizeDiff(bnas, anas, "nas")
  if (diff == "")
    return("")
  
  diffs <- paste(diff, ":")
  bcols <- colnames(before)
  acols <- colnames(after)
  for(col in intersect(bcols, acols))
  {
    bnas <- sum(is.na(before[[col]]))
    anas <- sum(is.na(after[[col]]))
    diff <- SummarizeDiff(bnas, anas, paste("nas in", col))
    if (diff != "")
      diffs <- paste(diffs, "\n    ", diff)
  }
  for(col in setdiff(bcols, acols))
  {
    bnas <- sum(is.na(before[[col]]))
    diff <- SummarizeDiff(bnas, 0, paste("nas in", col))
    if (diff != "")
      diffs <- paste(diffs, "\n    ", diff)
  }
  for(col in setdiff(acols, bcols))
  {
    anas <- sum(is.na(after[[col]]))
    diff <- SummarizeDiff(0, anas, paste("nas in", col))
    if (diff != "")
      diffs <- paste(diffs, "\n    ", diff)
  }
  return(diffs)
}

SummarizeChange <- function(snapshot, after, name, stop=FALSE)
{
  # compare data
  before <- snapshot$data
  diffs = c(SummarizeDiff(nrow(before), nrow(after), "rows"),
            SummarizeColDiff(before, after),
            SummarizeNasDiff(before, after))
  diffs_str = paste(diffs, collapse=", ")
  if (stop)
    stopifnot(nrow(before) == nrow(after))
  
  # compare time
  now <- Sys.time()
  elapsed = difftime(now, snapshot$time, units="secs")
  
  # display
  step <- snapshot$step + 1
  if(diffs_str == ", ")
    cat("[", step, "] ", name, " (", elapsed, "s): ok\n", sep="")
  else
    cat("[", step, "] ", name, " (", elapsed, "s): ", diffs_str, "\n", sep="")
  
  # build new snapshot
  return(list("data"=data.frame(after), "time"=now, "step"=step))
}

SummarizeDataFrame <- function(data)
{
  text <- paste("[0] init:", ncol(data), "cols and", nrow(data), "rows")
  nas = sum(colSums(is.na(data)))
  if (nas)
  {
    cat(text, ", with ", nas, " nas\n", sep="")
    print(colSums(is.na(data)))    
  }
  else
  {
    cat(text, "\n")
  }
  return(list("data"=data.frame(data), "time"=Sys.time(), "step"=0))
}

BuildData <- function(pc, sites)
{
  cat("BuildData()\n")
  
  snapshot = SummarizeDataFrame(pc)
  data <- merge(pc, sites, by.x="reader", by.y="SAMM.project.Site_ID", all.x=T)
  snapshot = SummarizeChange(snapshot, data, "merge")
  
  data$Name <- NULL
  snapshot = SummarizeChange(snapshot, data, "clean names")
  
  data <- subset(data, !is.na(data$count))
  snapshot = SummarizeChange(snapshot, data, "subset")
  
  data$time <- strptime(data$date_time, "%d/%m/%Y %H:%M", "GMT")
  snapshot = SummarizeChange(snapshot, data, "clean time")
  
  return(data)
}

TestBuildData <- function()
{
  data <- mtcars
  snapshot = SummarizeDataFrame(data)

  data$new <- data$cyl
  snapshot = SummarizeChange(snapshot, data, "add new")
  
  data$new <- NA
  snapshot = SummarizeChange(snapshot, data, "clean new")
  
  data$disp <- NULL
  snapshot = SummarizeChange(snapshot, data, "remove disp col")
  
  data <- subset(data, data$vs == 0)
  snapshot = SummarizeChange(snapshot, data, "remove non v shape")

  return(data)
}

# Validators
BuildValidators <- function()
{
  return(list("dataframe"=list(), "columns"=list()))
}

Validate <- function(df, validators)
{
  for (val in names(validators$dataframe))
  {
    if (!validators$dataframe[[val]](df))
    {
      stop("Dataframe validator [", val, "] did not pass")
    }
  }
  for (col in names(validators$columns))
  {
    for (val in names(validators$columns[[col]]))
    {
      if (!validators$columns[[col]][[val]](df[[col]]))
      {
        stop("Column '", col, "' validator [", val, "] did not pass")
      }
    }
  }
}


# Summarizer

# ConfigureEnv()
# data <- BuildData(pc_complete, counter_sites)
my_data <- TestBuildData()

# Validators

validators <- BuildValidators()
validators$dataframe$no_nas = function(df) { sum(colSums(is.na(df))) == 0 }
validators$columns$am$no_invalid = function(col) { length(col[col < 0 | col > 1]) == 0 }

Validate(my_data, validators)
my_data2 <- mtcars
Validate(my_data2, validators)
my_data2$am <- my_data2$am + 1
Validate(my_data2, validators)
validators$columns$am <- NULL
Validate(my_data2, validators)