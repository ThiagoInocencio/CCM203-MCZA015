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

createPath <- function (strPath = "") {

  if (!file.exists('./raw')){
    dir.create(file.path('./raw'))
  }

  path <- sprintf('%s/%s' , './raw', strPath)

  if (!file.exists(path) && strPath != ""){
    dir.create(file.path(path))
  }

  path
}

getHTML <- function (url, folder = "", archive_name = '') {


  path <- createPath(folder)

  if(archive_name == '')
    archive_name <- url %>%  stringr::str_extract('[^=]*$')
  else
    archive_name <- archive_name

  arq <- sprintf('%s/%s.html' , path, archive_name)

  if(file.exists(arq)) {

    codHTML <- xml2::read_html(arq)

  } else {

    if(folder != "")
      httr::GET(url, httr::write_disk(arq, overwrite = TRUE))

    codHTML <-  url %>%
      httr::GET() %>%
      httr::content('text', encoding = 'latin1') %>%
      xml2::read_html()

  }
}

getSystemInfo <- function (sysName, url) {

  sysName <- (gsub("/", "", sysName))
  sysName <- (gsub("projects", "", sysName))

  codeHTML <- getHTML(url, "systems", (gsub("/", "-", sysName)))

  systName <- sysName

  # Variables
  systName <- codeHTML %>%
    rvest::html_nodes('div.title h1') %>%
    xml2::xml_text()

  systemDescription <- codeHTML %>%
    rvest::html_nodes('h3.summary') %>%
    xml2::xml_text()

  systemDescription <- (gsub("\n", "", systemDescription))

  status <- codeHTML %>%
    rvest::html_node('span.status-value') %>%
    xml2::xml_text()

  operationSystems <- tolower(codeHTML %>%
    rvest::html_nodes('div.platforms span') %>%
    xml2::xml_text())

  reviews <- codeHTML %>%
    rvest::html_nodes('div.rating a') %>%
    xml2::xml_text()

  category <- codeHTML %>%
     rvest::html_nodes('[itemprop=applicationCategory]') %>%
    xml2::xml_text()

  #category <- paste(category,collapse="/")
  category <- (gsub("/", "-", category))
  category <- (gsub(" ", "-", category))
  category <- tolower(category)

  rating <- codeHTML %>%
    rvest::html_nodes('[itemprop=ratingValue]') %>%
    xml2::xml_text()

  translations <- codeHTML %>%
    rvest::html_nodes('[itemprop=inLanguage]') %>%
    xml2::xml_text()

  for (translat in translations){
    translat <- (gsub("/", "-", translat))
    translat <- (gsub(" ", "-", translat))
    translat <- tolower(translat)
  }

  lastUpdate <- codeHTML %>%
    rvest::html_nodes('.dateUpdated') %>%
    xml2::xml_text()

  downloads <- "NA"

  download_content <- codeHTML %>%
    rvest::html_nodes('div.stats h2 a')

  for (content in download_content){

    if(grepl("Downloads", content, fixed=TRUE)) {
      downloads <- content %>%  xml2::xml_text()
    }
  }

  license <- c()
  audience <- c()
  programming_language <- c()
  user_interface <- c()

  projectInfo <- codeHTML %>%
    rvest::html_nodes('section.project-info a')

  for (content in projectInfo){

    if(grepl("license", content, fixed=TRUE)) {

      licen <- content %>%  xml2::xml_text()
      licen <- (gsub("/", "-", licen))
      licen <- (gsub(" ", "-", licen))
      licen <- tolower(licen)


      if(length(license) == 0)
        license <- licen
      else
        license <- c(license, (licen))
    }

    # programming languages
    if(grepl("language", content, fixed=TRUE)) {

      prog_lan <- content %>%  xml2::xml_text()
      prog_lan <- (gsub("/", "-", prog_lan))
      prog_lan <- (gsub(" ", "-", prog_lan))
      prog_lan <- tolower(prog_lan)

      translations_obj <- paste(translations, collapse="/")

      if(length(programming_language) == 0) {
        p_language <- prog_lan
        if(p_language %nin% translations_obj)
          programming_language <- p_language
      }
      else
        p_language <- prog_lan
        if(p_language %nin% translations_obj) {
        programming_language <- c(programming_language, p_language)
        }
    }

    if(grepl("audience", content, fixed=TRUE)) {
      audi <- content %>%  xml2::xml_text()
      audi <- (gsub("/", "-", audi))
      audi <- (gsub(" ", "-", audi))
      audi <- tolower(audi)

      if(length(audience) == 0)
        audience <- audi
      else
        audience <- c(audience, audi)
    }

    #user interface
    if(grepl("environment", content, fixed=TRUE)) {

      us_in <- content %>%  xml2::xml_text()
      us_in <- (gsub("/", "-", us_in))
      us_in <- (gsub(" ", "-", us_in))
      us_in <- tolower(us_in)

      if(length(user_interface) == 0)
        user_interface <- us_in
      else
        user_interface <- c(user_interface, (us_in))
    }
  }

  # creating the data frame
  data <- data.frame(
      #"SystemURL" = systemUrl,
      "SystemName" = systName,
      #"SystemDesc" = systemDescription,
      "status" = status,
      "OperationSystems" = paste(operationSystems,collapse="/"),
      "Audience" = paste(audience,collapse="/"),
      "Category" = paste(category,collapse="/"),
      "Language" = paste(programming_language, collapse="/"),
      "Translations" = paste(translations, collapse="/"),
      "ProgrammingLan" = paste(programming_language, collapse="/"),
      "UserInterface" = paste(user_interface, collapse="/"),
      "LastUpdate" = lastUpdate,
      #"Reviews" = reviews,
      #"Rating" = rating
      "License" = paste(license, collapse="/")
      )
  data
}

getSystemByPage <- function (url) {
  codeHTML <- getHTML(url, "pages")
  systems <- codeHTML %>% rvest::html_nodes('[itemprop=itemListElement]') %>% rvest::html_nodes('[itemprop=url]') %>% rvest::html_attr("href")
}

allPagesURL <- sprintf("https://sourceforge.net/directory/?page=%d", 1:20)
systems <- c()
for(page in allPagesURL) {
  systems <- c(systems, getSystemByPage(page))
}

data <- data.frame()
for(system in systems) {
  sysURL <- sprintf("https://sourceforge.net%s", system)
  try(data <- rbind(data, getSystemInfo(system, sysURL)))
}


View(data)
write.csv(data,'data_set.csv', row.names = FALSE)


eSystemS <- data[1:10,]

idealSystem <- data.frame(
  "SystemName" = "Candidate System",
  "status" = "Beta",
  "OperationSystems" = "linux/windows",
  "Audience" = "developers",
  "Category" = 0,
  "Language" = "english",
  "Translations" = "english",
  "ProgrammingLan" = "c++",
  "UserInterface" = 0,
  "LastUpdate" = 0,
  "License" = "gnu-general-public-license-version-2.0-(gplv2)"
)

# Read CSV into R
MyData <- read.csv(file="normalized_new.csv", header=TRUE, sep=",")
View(MyData)



# #############################################
#  k-means
# #############################################
# Lendo os arquivo que contém o data set com a pontuação calculada e normalizada
MyData <- read.csv(file="example_1.csv", header=TRUE, sep=",")
View(MyData)

# execução do k-means nos dados normalizados com k = 10
test <- kmeans(MyData[,2:10], 10)

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

plot(agrupamento, main = "Agrupamento Hier?rquico Aglomerativo", xlab = "Sistemas", ylab = "Dist?ncia")

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
