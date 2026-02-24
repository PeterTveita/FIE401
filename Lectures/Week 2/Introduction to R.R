################################
#                              #
#    Financial Econometrics    #
#      Introduction to R       #
#                              #
################################



## --------------------------------------------------------------------- ##

## Overview

# - First steps in R
# - Data objects
# - Data structures
# - Importing, saving, and loading data
# - Manipulation
# - Regression
# - Loops
# - Tidyverse implementation



## Other sources

# - Internet
# - Heiss F. 2016: Using R for Introductory Econometrics
# - Lam L. 2011: Introduction to R
# - Venables W.N. & Smith D.M.: An Introduction to R
# - Paradis E.: R for beginners
# - Other NHH courses



## --------------------------------------------------------------------- ##

## Get started

# - Download R: https://cran.r-project.org/bin/
# - Download R-Studio: https://www.rstudio.com/products/rstudio/download
# - Open R-Studio
# - Talk to TA if problems with this



## R-Studio interface

# - Lower-left: Console
# - Upper-left: Script 
#     - Sending code to console
#         (1) RUN to send whole code
#         (2) Mark code and press CTRL+ENTER (Apple COMMAND)
#         (3) press CTRL+ENTER sends line in which cursos is
# - Upper right: Environment/History/Connection Tab
# - Lower right: Plots/Helpdesk/Packages/...
# - You can arrange as you wish!
1+1

## First steps

# First program
print("Hello world")

# - Some calculations
1+1

# - Storing results in memory
x <- 1+1
x


## First steps: object names

# - Reading code by others (or even just old code) is a pain
# -> stick to some syntax

# - R is case sensitive
x <- 1
X <- 2

# - A dot and an underscore is allowed
X. <- 1
x_ <- 1

# - Object names can containg by not start with a number
x1 <- 1
1x <- 1 # gives error

# - "Strange" symbols such as !, +, -, #, are not allowed in names
# - USE syntax guide
#    - https://google.github.io/styleguide/Rguide.xml
#    - http://adv-r.had.co.nz/Style.html
#    - or any other 



## First steps: define a working directory

# - Current directory
getwd()

# - set a new working directory
setwd("/Users/petertveita/Documents/GitHub/FIE401")

# - Or: Session -> To Working Directory -> Choose directory

# - or get directory from explorer/finder
# - take care the / and \ are the right direction

# - check which files are in the WD
list.files()



## First steps: Where to get help?

# - online help
help(list.files)
?list.files

# - https://stackoverflow.com and Google
# - Rblogger
# - Manuals on CRAN for individual packages
# - Unlikely that you are the first person having a certain problem
# - Small advice: read carefully



## First steps: workspace and managing objects

# - Created objects during an R session are held in memory (upper right corner)
# - Display objects in memory
ls()

# - Workspace is not saved to disk unless you tell R to do that
# - I would not save sessions, only scripts
# - Limitation of R is that objects are in mempory and memory is not unlimited



## First steps: R packages

# - R can be easily extended by loading R packages
# - They usually contain R objects, formulas, data, and documentation (!)
# - Lively community producing packages all the time
# - A package needs to be installed once
install.packages("plm")

# - A package needs to be loaded every session
require(plm)
library(plm)



## --------------------------------------------------------------------- ##

## Data objects
  
# - Numerics
# - Characters
# - Logicals
# - Missing values
# - Factors
# - Date



## Data objects: Numerics

# - numerical values such as 1, 2, 3,...

x <- 1
class(x)



## Data objects: Characters

# - collection of characters between double quotes, such as "FIE" and "BAN"

course.code <- "FIN401"
class(course.code)



## Data objects: Missing data

# - NA = not available
# - NaN = Not a number

y <- NA
y



## Data objects: Factor

# - categorical variables (e.g. "Male" & "Female")
# - Internally saved as numbers with label 

# gender expressed as character
gender <- c("male", "female", "male")
class(gender)
as.numeric(gender)

# gender expressed as factor
gender <- factor(c("male", "female", "male"))
class(gender)
gender
as.numeric(gender)

  

## Data objects: Date

# - Date object allow manipulation of date variables
# - Internally saved as numeric (Number of days since 1970-01-01)

date.1 <- as.Date("1970-02-01")
class(date.1)
as.numeric(date.1)

# - There are also date variables with higher frequency than days

# - Sample dates saved as strings
date.as.character.1 <- "1980.1.31" 
date.as.character.2 <- "1980-31-1" 
date.as.character.3 <- "31/1/80" 

# - Transform to date format
as.Date(date.as.character.1, format = "%Y.%m.%d")
as.Date(date.as.character.2, format = "%Y-%d-%m")
as.Date(date.as.character.3, format = "%d/%d/%y")

# - Have date format and output in different formats
date.1 <- as.Date(date.as.character.1, format = "%Y.%m.%d")

date.1
format(date.1, "%d/%m/%Y")
format(date.1, "Year: %Y, Month: %m, Day: %d")

# Dictionary: see https://www.r-bloggers.com/date-formats-in-r/



## Data object: logical conditions

# - smaller than
1 < 2
2 < 1

# - smaller than or equal to
1 <= 1

# - larger than
1 > 2

# - larger than or equal to
1 >= 2

# - is equal to 
1 == 1
1 == 2

# - is unequal to / is not
1 != 1
1 != 2

# and 
TRUE & TRUE
(1 < 2) & (2 < 3)
(1 < 2) & (2 > 3)

# or 
TRUE | FALSE
(1 < 2) | (2 < 3)
(1 < 2) | (2 > 3)

# is within 
1 %in% c(1, 2, 3)
c(1, 2) %in% c(1, 3, 4)

# not
!TRUE

# - can be applied to any object class and data type



## --------------------------------------------------------------------- ##

## Data structures

# - Vector
# - Matrix
# - Data-frame (Tibble)
# - List



## Data structures: Vector

# - number of elements of the same type
vector.1 <- c(1, 2, 3)
vector.1

vector.2 <- c("a", "b", "c")
vector.2

vector.3 <- factor(x = c(1, 2, 1), labels = c("m", "f"))
vector.3



## Data structures: Matrix

# - generalization of a vector
# - all elements are of the same type

matrix( 1:9, nrow = 3, ncol = 3)
matrix( 1:9, nrow = 3, ncol = 3, byrow = T)

matrix.1 <- cbind(vector.1, vector.2)
class(matrix.1)
matrix.1 # why is column 1 not numerical?



## Data structures: Data-frame (Tibble)

# - extension of a matrix
# - different columns can have different types
# - very convenient data structure 
# - Tibble: even better, see section on tidyverse

data.frame.1 <- data.frame(
  numeric = vector.1, 
  character = vector.2, 
  gender = vector.3
)

data.frame.1
class(data.frame.1)
str(data.frame.1) # Anything wrong with column 2?

data.frame.1 <- data.frame(
  numeric = vector.1, 
  character = vector.2, 
  gender = vector.3, 
  stringsAsFactors = F
)
str(data.frame.1)

data.frame.1$numeric

# Is there any issue with using strings as factors? 
# (1) Factors save memory space only if levels are repeated
# (2) Factors can be made in numerics, which one might forget



## Data structures: List

# - Stacking all types you wish together
# - Similar to vector, but every element can be an object
# - Often used as output of functions (e.g. Regression output)

list.1 <- list(item.1 = 99, 
               item.2 = data.frame.1, 
               item.3 = "a list is great")
list.1
str(list.1)

list.1$item.1
list.1[[1]]



## --------------------------------------------------------------------- ##

## Importing, saving, and loading data

# - save
save(list.1, file = "list_1.RData")
list.files()

# - delete
rm(list.1)
ls()

# - load again
load("list_1.RData")



## Importing external files

# - Common files: 
#    - .csv (comma separated values)
#    - .txt (text files)

# - Common feature
#    - Data table contained
#    - Separation character: comma, semicolon, tab, etc. 

# - Open text file in editor (TextEdit, NotePad,...) that 
#   displays data without imposing a structure (such as Excel)



## Importing external files - common problems: 

# - Existance of header and row name
# - Empty last line
# - Quotes around data entries
#     - Quotes usually signal strings
# - Column separator creates non-unique number of elements per row
#     - Thousand, decimal, and date separator different dependent on region 
#     - Thousand, decimal, and date separator also column separator

# code
read.table(file = ..., header = ..., sep = ...)
read.csv(file = ..., header = ..., sep = ...)



## --------------------------------------------------------------------- ##

## Manipulation 

# - Let's say we have some data on the appearance of individuals
char.data <- data.frame(
  person = 1:6,
  hair = c("brown", "blonde", "black", "blonde", "blonde", "black"),
  heigth = c(180, 174, 185, 160, 165, 192), 
  grade = c(1, 2, 3, 2, 3, 1)
)
char.data

# - Let's say we want to extract some elements
char.data$hair
char.data[,"hair"]
char.data[["hair"]]
char.data[,2]
char.data[3,2]
char.data[char.data$hair == "blonde",]
char.data[char.data$hair == "blonde", "grade"]
char.data$grade[char.data$hair == "blonde"]
char.data[char.data$grade <= 2,]

# - Let's say we want to replace a variable
char.data$grade[char.data$grade <= 2] <- 2
char.data$grade

# - Let's sat we want to declare a new variable
char.date$new.var <- 6:1
char.date[["new.var"]] <- 6:1


## Manipulations: useful functions
length()
?length()

sum()
max()
min()
sort()
order()
table()
hist()
mean()
sd()
dim()
colnames()
rownames()
colSums()
rowSums()
head()
tail()
...



## --------------------------------------------------------------------- ##

## Regression 

# - Let's not care about the theory or understanding
# - Let's care about running a regression (whatever that means)

# - Let's create some random data
random.data <- data.frame(
  x = rnorm(100, 0, 3), 
  e = rnorm(100, 0, 1))
head(random.data)

# - We construct Y according to following formula y = 3 + 0.5*x + e
random.data$y <- 3 + 
  0.5* random.data$x + 
  random.data$e
head(random.data)

# - Display
plot(x = random.data$x, 
     y = random.data$y)

# - Let's estimate a regression of y on x
# - (Theory, understanding, etc. comes in the next lectures)
reg <- lm(y ~ x, data = random.data)

# - Inspect output
reg
summary(reg)
str(reg)
ls(reg)



## --------------------------------------------------------------------- ##

## Loops

# - repeat a sequence of code 
# - for-loop: number of iterations defined
# - Let's write a loop that 
#    - Iterates from 1 to 5
#    - For each iteration the iteration number squared 
#    - ...and subsequently displayed
for(i in 1:5){
  print(i^2)
}



## --------------------------------------------------------------------- ##

##  Tidyverse

# - Tidy data
# - Tibble
# - dplyr
# - see: https://www.tidyverse.org

 
## Tidy data

# - Every column is a variable
# - Every row is an observation
# - Every cell is a single value
# - Stick to it!

# - Not tidy
data.frame(day = 1:30,
           january = rnorm(30), 
           february = rnorm(30))

# - Tidy
data.frame(month = rep(c("january", "february"), each = 30),
           day = c(1:30, 1:30), 
           value = rnorm(60))

## Tibble

# - better data.frames
# - can use dplyr functions
require(tibble)
tibble(day = 1:30,
       january = rnorm(30), 
       february = rnorm(30))


## Dplyr

# - %>% pipe command
# - Grammar for data manipulation
require(dplyr)


## %>% pipe command

# - Output of a function is used as input of another function

x <- rnorm(100)
hist(x)


rnorm(100) %>% 
  hist()

# - Cleaner 
# - Unfortunately no improvement in memory management



## Grammar for data manipulation

# - Let's start with a tibble

our.data <- tibble(x = rnorm(100))

# - mutate() adds new variables that are functions of existing variables
mutate(our.data, y = x^2)

our.data %>% 
  mutate(y = x^2) -> our.data

# - select() picks variables based on their names.
select(our.data, y)
our.data %>% select(y)

# - filter() picks cases based on their values.
filter(our.data, y>0)

# - and some more, see: https://dplyr.tidyverse.org



## --------------------------------------------------------------------- ##

## Overview

# - First steps in R
# - Data objects
# - Data structures
# - Importing, saving, and loading data
# - Manipulation
# - Regression
# - Loops
# - Tidyverse implementation


