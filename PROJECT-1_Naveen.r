
############# Project 1 ############## 

# Part I 
# There are 20 files with .dat extention. You have to read all the files in to single dataframe.
# getwd()
# setwd ("F:/ACADGILD/Business Analytics With R/ASSIGNMENTS/_Project-1/iris")  

install.packages("data.table")
library("data.table")
testdata1 <- read.table("001.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata2 <- read.table("002.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata3 <- read.table("003.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata4 <- read.table("004.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata5 <- read.table("005.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata6 <- read.table("006.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata7 <- read.table("007.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata8 <- read.table("008.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata9 <- read.table("009.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata10 <- read.table("010.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata11 <- read.table("011.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata12 <- read.table("012.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata13 <- read.table("013.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata14 <- read.table("014.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata15 <- read.table("015.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata16 <- read.table("016.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata17 <- read.table("017.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata18 <- read.table("018.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata19 <- read.table("019.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)
testdata20 <- read.table("020.dat", header = FALSE, sep=",", skip=9, stringsAsFactors=FALSE)

df <- as.data.frame(rbind(testdata1,testdata2,testdata3,testdata4,testdata5,testdata6,testdata7,testdata8,testdata9,testdata10,testdata11,testdata12,testdata13,testdata14,testdata15,testdata16,testdata17,testdata18,testdata19,testdata20))
# str(df)
names(df) <- c('SepalLength', 'SepalWidth', 'PetalLength', 'PetalWidth', 'Class')
# head(df)  
View(df)

# Part II  
# The data is present in xml format, with file name, iris.xml. Your task is to read the XML data 
# and store itin the data frame df. 

install.packages("XML")
  
# Loading the package required to read XML files.
library("XML")

# Also load the other required package.
library("methods")

# Give the input file name to the function.
setwd ("F:/ACADGILD/Business Analytics With R/ASSIGNMENTS/_Project-1")
getwd()

result <- xmlParse("iris.xml")

# result1 <- xmlInternalTreeParse("iris.xml")
# print(result1)

# Print the result.
print(result) 

####### XML to Data Frame ########
# Convert the input xml file to a data frame.
xmldataframe <- xmlToDataFrame(result)
View(xmldataframe)

# Part III 
# Convert the iris data into the JSON format and read the data in JSON format and convert it into
# dataframe "iris_data". 

install.packages("rjson")
# Load the package required to read JSON files.
library("rjson")
library("jsonlite")

# Reading iris Data
data(iris)

# Reading JSON Data 
jsonOut<-toJSON(iris, byrow = TRUE, colNames = TRUE)
jsonOut

# Writing JSON Data to iris.json
write_json(jsonOut, "F:/ACADGILD/Business Analytics With R/ASSIGNMENTS/_Project-1/iris.json")

# Convert JSON file to a data frame iris_data
iris_data = data.frame(do.call(cbind, fromJSON(jsonOut)),stringsAsFactors = FALSE)

### Convert variables from character to numeric, but excluding one character variable ####
iris_data[] <- lapply(iris_data, function(x) type.convert(as.character(x), as.is = TRUE))
str(iris_data)
View(iris_data)

# Print the result.
print(iris_data)

# Part IV  
# Use dplyr function on the data iris_data. Implement select, match, filter, arrange, rename, and 
# mutatefunction on the iris_data. 

# install.packages("dplyr") 
library(dplyr)

# Load data
data("iris_data")

# Converts data to tbl class. tbl's are easier to examine than data frames. R displays only the data that fits onscreen.
tbl_df(iris_data)
str(iris_data)

###### Select columns by names ########
# Select three variables    
df1 <- iris_data %>% 
      select(Petal.Length, Petal.Width, Species)
View(df1)

# Remove a column using select   
df2 <- iris_data %>% 
        select(-Species)
View(df2)

####### Arrange : order rows by values of a column ###########
# Sort by descending order on Species and ascending order by petal length
iris.sorted <- arrange(iris, desc(Species), Petal.Length)
View(iris.sorted)

####### Filter : select a subset of the rows that meet certain criteria #######
# In the below-mentioned code, in order to exclude Species starting with letters/character values "SE"
iris_data %>% 
  select(Sepal.Length,Sepal.Width,Species) %>%
  filter(Species!= "SE" & as.numeric(Sepal.Length >= 6.9))%>%
  summarise(mean(Sepal.Width, na.rm = TRUE))

##### Mutating multiple columns #######
### Ex:1 ####
mutate_each(iris, funs(. * 10), -Species) %>% tbl_df

### Ex:2 ####
iris.ratio <- iris_data %>% 
  mutate(Petal.ratio = Petal.Length / Petal.Width) %>% 
  select(Petal.Length:Species, Petal.ratio)
View(iris.ratio)

##### Using Matching function ##### 
# Ex: 1 ######
select_vars(names(iris_data), matches(".t."))

# Ex: 2 ######
select_vars(names(iris_data), -matches(".t."))

####### Renaming variables #######
rename_ex1 <- select_vars(names(iris), petal_length = Petal.Length)
rename_ex1

###### Rename variables preserving all existing ######
rename_ex2 <- rename_vars(names(iris), petal_length = Petal.Length)
rename_ex2

# Part V  
# Print the summary of iris_data 
summary(iris_data)

