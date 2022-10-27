## ----eval=FALSE---------------------------------------------------------------
## name <- value


## -----------------------------------------------------------------------------
# to execute r command in Rstudio, select code chunk and hit ctrl + enter
x <- 5
print(x)
typeof(x)
class(x)
str(x)


## -----------------------------------------------------------------------------
# logical
x_logical <- c(TRUE, FALSE)
class(x_logical)
typeof(x_logical)

# integer
x_int <- c(1L, 2L) # ending L for integer
class(x_int)
typeof(x_int)

# double
x_double <- c(1.1, 2)
class(x_double)
typeof(x_double)

# character/string
x_char <- c("1", "2")
class(x_char)
typeof(x_char)


## -----------------------------------------------------------------------------
x_int + 1
x_double + 1
x_char + 1


## -----------------------------------------------------------------------------
my_list <- list(1:10, c("A", "B", "C"))
my_list


## -----------------------------------------------------------------------------
(x_vec_named <- c("A" = 1, "B" = 2))
(x_list_named <- list("A" = 1:10, "B" = letters))


## -----------------------------------------------------------------------------
1:10


## -----------------------------------------------------------------------------
seq(0, 100, by = 10)


## -----------------------------------------------------------------------------
rep(1, 10)


## -----------------------------------------------------------------------------
# atomic vector
x_vec_named <- c("A" = 1, "B" = 2)

x_vec_named[1]
x_vec_named['A']

x_vec_named[[2]]
x_vec_named[['B']]


## -----------------------------------------------------------------------------
# list
x_list_named <- list("A" = 1:10, "B" = letters)

x_list_named[1]
x_list_named['A']

x_list_named[[2]]
x_list_named[['B']]
x_list_named$B

x_list_named[[1]][5]


## -----------------------------------------------------------------------------
x_vec_named[c(TRUE, FALSE)]


## -----------------------------------------------------------------------------
x <- 1:10
x[x > 3]


## -----------------------------------------------------------------------------
head(iris) # display first 6 rows
typeof(iris)
class(iris)
attributes(iris)
methods(class=class(iris))


## -----------------------------------------------------------------------------
x <- 1:10

# add
x + 10

# multiplication
x * 10

# power
x ** 2

# square root
sqrt(x)

# log
log(x)

# exponential
exp(x)


## -----------------------------------------------------------------------------
## function(arg1 = value1, arg2 = value2, ...)
## # note: some functions do not require an argument


## -----------------------------------------------------------------------------
# example
sum(1:10)


## -----------------------------------------------------------------------------
## # help
## help(sum) # or ?sum


## -----------------------------------------------------------------------------
## # syntax for installation
## install.packages("package_name")
## install_github("github_repo")
## 
## # syntax for load package/function
## # method 1
## library(package_name)
## function_name()
## # method 2
## package_name::function_name


## -----------------------------------------------------------------------------
# install.packages("dplyr")

# method 1
library(dplyr)
count(iris, Species) 

# method 2
dplyr::count(iris, Species)


## -----------------------------------------------------------------------------
1 > 1
1 < 1
1 == 1
1 != 1
1 >= 1
1 <= 1
1 %in% 1:3
1 %in% 2:4
"a" %in% c("a", "b", "c")


## -----------------------------------------------------------------------------
TRUE

# negate
!TRUE

# AND
TRUE & TRUE
TRUE & FALSE
FALSE & FALSE

# OR
TRUE | TRUE
TRUE | FALSE
FALSE | FALSE


## -----------------------------------------------------------------------------
## if (condition1) {
##   do A
## } else if (condition2) {
##   do B
## } else if (conditionN) {
##   do N
## } else {
##   do Z
## }


## -----------------------------------------------------------------------------
a <- 1
b <- 1
if (a > b) {
  print(paste0(a, " is greater than ", b))
} else {
  print(paste0(a, " is less than ", b))
}


## -----------------------------------------------------------------------------
a <- 1
b <- 1
if (a > b) {
  print(paste0(a, " is greater than ", b))
} else if (a < b) {
  print(paste0(a, " is less than ", b))
} else {
  print(paste0(a, " equals to ", b))
}


## -----------------------------------------------------------------------------
## for (i in list_a) {
##   do something with i
## }


## -----------------------------------------------------------------------------
for(i in seq(1:10)) {
  print(i)
}


## -----------------------------------------------------------------------------
## while (condition) {
##   do something
##   update condition
## }


## -----------------------------------------------------------------------------
i <- 0

while(i < 10) {
  print(i)
  i <- i+2
}


## -----------------------------------------------------------------------------
## # without pipe
## functionB(functionA(x, y), z)
## 
## # with pipe
## x |> functionA(y) |> functionB(z)


## -----------------------------------------------------------------------------
# without pipe
set.seed(123)
mean(rnorm(1000, mean = 10, sd = 1))

# with pipe
set.seed(123)
rnorm(1000, mean = 10, sd = 1) |> mean()

