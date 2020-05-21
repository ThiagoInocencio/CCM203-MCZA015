install.packages("magrittr")
install.packages("rvest")
install.packages("purrr")
install.packages("Hmisc")
install.packages("xlsx")

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%

library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(plyr)
library(magrittr)
library(stringr)
library(xlsx)
library(Hmisc)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%


# #############################################
#  Data Cleaning functions
# #############################################
# ----------------------------------------------------------------------------
# |
# | Function to change blank values to NA
# |
# ----------------------------------------------------------------------------
removeBlankValues <- function (eSystemS) {
    # OperationSystems attribute
  eSystemS$OperationSystems[eSystemS$OperationSystems == ''] <- NA

  # Audience attribute
  eSystemS$Audience[eSystemS$Audience == ''] <- NA

  # Category attribute
  eSystemS$Category[eSystemS$Category == ''] <- NA

  # Language attribute
  eSystemS$Language[eSystemS$Language == ''] <- NA

  # Translations attribute
  eSystemS$Translations[eSystemS$Translations == ''] <- NA

  # ProgrammingLan attribute
  eSystemS$ProgrammingLan[eSystemS$ProgrammingLan == ''] <- NA

  # UserInterface attribute
  eSystemS$UserInterface[eSystemS$UserInterface == ''] <- NA

  # License attribute
  eSystemS$License[eSystemS$License == ''] <- NA

  # Returning systems without empty attributes
  return eSystemS
}


normalize <- function (value, min, max) {
  normalized = (value - min) / (max - min);
  normalized;
}


score <- function (vectorValues, fieldName, eSystems, i) {
  cont_score <- 0
  for( value in vectorValues ) {
    if(!(is.na(value))) {
        if(grepl(value, eSystems[i, fieldName], fixed=TRUE) == TRUE) {
          cont_score <- cont_score + 1
        }
    }
  }

  # return
  cont_score
}

normalizeData <- function (idealSystem, eSystems) {

  OperationSystems <- unlist(strsplit(as.character(idealSystem$OperationSystems), split="/"))
  status <- as.character(idealSystem$status)
  Audiences <- unlist(strsplit(as.character(idealSystem$Audience), split="/"))
  Categories <- unlist(strsplit(as.character(idealSystem$Category), split="/"))
  Languages <- unlist(strsplit(as.character(idealSystem$Language), split="/"))
  Translations <- unlist(strsplit(as.character(idealSystem$Translations), split="/"))
  ProgrammingLan <- unlist(strsplit(as.character(idealSystem$ProgrammingLan), split="/"))
  UserInterface <- unlist(strsplit(as.character(idealSystem$UserInterface), split="/"))
  License <- unlist(strsplit(as.character(idealSystem$License), split="/"))

  for( i in rownames(eSystems) ) {

    # count Status
    eSystems[i, "status"] <- normalize(as.double(eSystems[i, "status"]), 0, 1)

    # count Operation System
    eSystems[i, "OperationSystems"] <- normalize(as.double(eSystems[i, "OperationSystems"]), 0, length(OperationSystems))

    # count Audience
    eSystems[i, "Audience"] <- normalize(as.double(eSystems[i, "Audience"]), 0, length(Audiences))

    # Category
    eSystems[i, "Category"] <- normalize(as.double(eSystems[i, "Category"]), 0, length(Categories))

    # Languages
    eSystems[i, "Language"] <- normalize(as.double(eSystems[i, "Language"]), 0, length(Languages))

    # Translations
    eSystems[i, "Translations"] <- normalize(as.double(eSystems[i, "Translations"]), 0, length(Translations))


    # ProgrammingLan
    eSystems[i, "ProgrammingLan"] <- normalize(as.double(eSystems[i, "ProgrammingLan"]), 0, length(ProgrammingLan))

    # UserInterface
    eSystems[i, "UserInterface"] <- normalize(as.double(eSystems[i, "UserInterface"]), 0, length(UserInterface))

    # License
    eSystems[i, "License"] <- normalize(as.double(eSystems[i, "License"]), 0, length(eSystems))

  }

  eSystems

}


calculeScore <- function (idealSystem, eSystems) {

  OperationSystems <- unlist(strsplit(as.character(idealSystem$OperationSystems), split="/"))
  status <- as.character(idealSystem$status)
  Audiences <- unlist(strsplit(as.character(idealSystem$Audience), split="/"))
  Categories <- unlist(strsplit(as.character(idealSystem$Category), split="/"))
  Languages <- unlist(strsplit(as.character(idealSystem$Language), split="/"))
  Translations <- unlist(strsplit(as.character(idealSystem$Translations), split="/"))
  ProgrammingLan <- unlist(strsplit(as.character(idealSystem$ProgrammingLan), split="/"))
  UserInterface <- unlist(strsplit(as.character(idealSystem$UserInterface), split="/"))
  License <- unlist(strsplit(as.character(idealSystem$License), split="/"))

  for( i in rownames(eSystems) ) {

    # count Status
    eSystems[i, "status"] <- score(status, "status", eSystems, i)

    # count Operation System
    eSystems[i, "OperationSystems"] <- score(OperationSystems, "OperationSystems", eSystems, i)

    # count Audience
    eSystems[i, "Audience"] <- score(Audiences, "Audience", eSystems, i)

    # Category
    eSystems[i, "Category"] <- score(Categories, "Category", eSystems, i)

    # Languages
    eSystems[i, "Language"] <- score(Languages, "Language", eSystems, i)

    # Languages
    eSystems[i, "Translations"] <- score(Translations, "Translations", eSystems, i)

    # ProgrammingLan
    eSystems[i, "ProgrammingLan"] <- score(ProgrammingLan, "ProgrammingLan", eSystems, i)

    # UserInterface
    eSystems[i, "UserInterface"] <- score(UserInterface, "UserInterface", eSystems, i)

    # License
    eSystems[i, "License"] <- score(License, "License", eSystems, i)
  }


  eSystems[nrow(eSystems) + 1,] = c(
    "idealSystem",
    1,
    length(OperationSystems),
    length(Audiences),
    length(Categories),
    length(Languages),
    length(Translations),
    length(ProgrammingLan),
    length(UserInterface),
    "",
    length(License)
    )


  eSystems <- subset(eSystems, select = -c(LastUpdate))

  eSystems

}


# #############################################
#  Analysis goes here
# #############################################
data <- read.csv('../data_set.csv', header = TRUE, stringsAsFactors = FALSE)

# Removing Blank values
eSystemS <- removeBlankValues(data[1:3000,])


# #############################################
#  Defining ideal systems
# #############################################

# Educational Chat
idealSystem <- data.frame(
  "SystemName" = "Candidate System",
  "status" = "Beta",
  "OperationSystems" = "windows",
  "Audience" = "education",
  "Category" = "chat/video-conferencing/communications",
  "Language" = 0,
  "Translations" = "Portuguese",
  "ProgrammingLan" = "c++/java",
  "UserInterface" = "handheld-mobile-pda",
  "LastUpdate" = 0,
  "License" = 0
)

escoreForFirstSystem <- eSystemS
escoreForFirstSystem <- calculeScore(idealSystem, escoreForFirstSystem)

View(escoreForFirstSystem)

# #############################################
#  Normalizing Data
# #############################################
MyData <- normalizeData(idealSystem, escoreForFirstSystem)
# #############################################
#  Normalizing Data
# #############################################


# removing systems that are not in audience or category
MyData <- MyData[MyData$Audience > 0,]
MyData <- MyData[MyData$Category > 0,]


View(MyData)


# #############################################
#  k-means
# #############################################
# Lendo os arquivo que contém o data set com a pontuação calculada e normalizada
#MyData <- read.csv(file="data_set.csv", header=TRUE, sep=",")
View(MyData)

# execução do k-means nos dados normalizados com k = 10
test <- kmeans(MyData[,2:8], 5)

# cluster of ideal system
get_cluster <- tail(test$cluster,1)

# recuperando os sistemas no mesmo cluster
systems_candidates <- MyData[test$cluster==get_cluster,]

#mostrando os sistemas candidatos
View(systems_candidates)






# #############################################
# Hierarchy aglomerative
# #############################################
# Lendo os arquivo que contém o data set com a pontuação calculada e normalizada
MyData <- read.csv(file="example_2.csv", header=TRUE, sep=",")
#View(MyData)

ms <- dist(MyData[,2:10], method = "euclidean")
agrupamento <- hclust(ms, method = "ward")

grupos <- cutree(agrupamento, 24)

get_cluster <- tail(grupos,1)
# = 24

# recuperando os sistemas no mesmo cluster
systems_candidates <- MyData[grupos==get_cluster,]

#plot(agrupamento, main = "Agrupamento Hier?rquico Aglomerativo", xlab = "Sistemas", ylab = "Dist?ncia")

View(systems_candidates)

rect.hclust(agrupamento, k=30, border="red")


# #############################################
# DBScan
# #############################################
install.packages("fpc")
library("fpc")

MyData <- read.csv(file="example_2.csv", header=TRUE, sep=",")
#View(MyData)

# execução do dbscan nos dados normalizados com com eps = 1 e MinPts = 2
  dbs <- dbscan(MyData[,2:10], eps = 2, MinPts = 380)

# cluster of ideal system
get_cluster <- tail(dbs$cluster,1)

# recuperando os sistemas no mesmo cluster
systems_candidates <- MyData[dbs$cluster==get_cluster,]

#mostrando os sistemas candidatos
View(systems_candidates)

plot(dbs, MyData[,4:6])


# vary parameters for most readable graph
library(cluster)
clusplot(MyData[,2:10], test$cluster, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
install.packages("fpc")
plotcluster(MyData[,2:10], test$cluster)
