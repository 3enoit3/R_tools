# Functions
# function parameters and return value can be of any type: string, number, dataframe...
FunctionName <- function(functionParameter1, functionParameter2) {
    print(functionParameter1)
    print(functionParameter2)
    
    return("my_result")
}
a_result <- FunctionName("arg1", "arg2")
print(a_result)

# functions can have multiple returns
FunctionWithMultipleReturns <- function(number) {
    if (number == 1) {
        return("one")
    }
    else {
        return("something else")
    }
}
print(FunctionWithMultipleReturns(1))
print(FunctionWithMultipleReturns(2))

# Note: a function is a variable like any other: if you change the code of the function, you need to 
# re-run the function declaration in R

# Strings
a_string <- paste("string1", 12, "string2") # default separator is space
print(a_string)

a_string_without_separator <- paste("string1", 12, "string2", sep="")
print(a_string_without_separator)

# you can use cat() to print multiple strings:
cat("string1", 12, "string2", "\n") # default separator is space
cat("string1", 12, "string2", "\n", sep="") # no separator
# if you don't put "\n" (new line character) at the end, it will stay on the same line
cat("when there is no new line")
cat("it is less nice")

# Vectors
# a vector is 1D collection of objects
# it can contain anything (string, number, dataframe, vector..) but they need to be of the same type
an_empty_vector <- c()
print(length(an_empty_vector) == 0)

a_vector <- c("item1", "item2")

# accessing an element with []
print(a_vector[1] == "item1")

a_vector_of_mixed_types <- c(1, "item1") # 1 will be converted to string because of "item1"
print(typeof(a_vector_of_mixed_types[1]) == "character") # 1 is not stored as a number

# loop through a vector
for (item in a_vector)
{
    print(item)
}

# append an item to the vector
a_vector <- c(a_vector, "item3")
print(a_vector)

# Lists
# a list is a 1D collection of objects
# it can contain anything, in any combination
# items can have a name
an_empty_list <- list()
length(an_empty_list) == 0

a_list <- list(1, "item1")
a_named_list <- list("my_quantity"=1, "my_name"="item1")

# accessing an element with []: the name and the value are returned
print(a_list[2]) # returns [[1]], "item1"
print(a_named_list[2]) # returns $my_name, "item1"

# accessing an element with [[]]: only the value is returned
print(a_list[[2]]) # returns "item1"
print(a_named_list[[2]]) # returns "item1"

# accessing an element with $: only the value is returned
print(a_named_list$my_name) # returns "item1"

# types can be different
print(typeof(a_list[[1]]) == "double") # 1 is stored as a number
print(typeof(a_list[[2]]) == "character") # "item1" is stored as a string

# loop through a list
for (item in a_list)
{
    print(item)
}

# append a list
a_list <- append(a_list, list("item3"))
print(a_list)

# Dataframes

#bindlist()

# Loops
items <- c(1, 2, 3)

# stop the loop
for (item in items) {
    print(item)
    if (item == 2) {
        stop("there is a problem with ", item)
    }
}

# it can stop any block 
{
    print("this is executed")
    stop("we have to stop the execution")
    print("this is not executed")
}

# but not individual instructions
print("this is executed")
stop("we want to stop the execution")
print("this is executed")

# skip the current item
for (item in items) {
    print(item)
    if (item == 2) {
        next
    }
    cat(item, "was executed\n")
}

##### Advanced #####

### Use lists to create "objects"
my_record <- list(name="reader", file="c:/data.csv", data=data.frame())
# my_record$data <- read_csv(my_record$file)
cat(my_record$name, "contains", nrow(my_record$data), "rows")

### Transform collections by applying functions
AddThree <- function(number) {
    return(number + 3)
}

input <- c(1, 2, 3, 4, 5)
output <- lapply(input, AddThree)
print(output)