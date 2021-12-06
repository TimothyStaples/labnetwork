rm(list=ls())

library(readxl)
library(d3r)
library(jsonlite)

setwd("/home/timothy/Dropbox/Tim/Post-doc/Presentations/School Lab Speed Talk")

# Make graph ####
  
  aTab <- read.csv("./pandNet.csv", stringsAsFactors = FALSE)
  nodes <- read.csv("./nodes.csv", stringsAsFactors = FALSE)
  
  # convert table into graph
  library(igraph)
  network=graph_from_data_frame(d=aTab, directed=F)
  
  V(network)$cat = nodes$Cat[match(V(network)$name, nodes$Node)]
  V(network)$picPath = nodes$picPath[match(V(network)$name, nodes$Node)]
  
  V(network)$count = c(30,15,15,15,15,22,30)[as.factor(V(network)$cat)]
  #V(network)$count[V(network)] = 
  
  V(network)$linkStr = c(5,1,1,1,1,5,5)[as.factor(V(network)$cat)]
  
  V(network)$textAbove = c(1,0,0,0,0,1,1)[as.factor(V(network)$cat)]
  
  V(network)$centre = c(1,0,0,0,0,0,0)[as.factor(V(network)$cat)]
  
  #V(network)$primary = "#324158"
  V(network)$primary = c("#FF0000", "#0000FF", "#808080","#0000FF", "#0000FF", "#000000", "#000000")[as.factor(V(network)$cat)]
  
  V(network)$label = V(network)$name
  V(network)$shadow = "rgb(255, 255, 255) 2px 0px 0px, rgb(255, 255, 255) 1.75517px 0.958851px 0px, rgb(255, 255, 255) 1.0806px 1.68294px 0px, rgb(255, 255, 255) 0.141474px 1.99499px 0px, rgb(255, 255, 255) -0.832294px 1.81859px 0px, rgb(255, 255, 255) -1.60229px 1.19694px 0px, rgb(255, 255, 255) -1.97999px 0.28224px 0px, rgb(255, 255, 255) -1.87291px -0.701566px 0px, rgb(255, 255, 255) -1.30729px -1.51361px 0px, rgb(255, 255, 255) -0.421592px -1.95506px 0px, rgb(255, 255, 255) 0.567324px -1.91785px 0px, rgb(255, 255, 255) 1.41734px -1.41108px 0px, rgb(255, 255, 255) 1.92034px -0.558831px 0px"
  V(network)$cat = 0
  V(network)$url = NA
  V(network)$offlabel = V(network)$name
  V(network)$width = 1
  V(network)$strokeCol = "#000000"
  
  # Transform it in a JSON format for d3.js
  data_json <- d3_igraph(network)
  
  # Save this file
  write(data_json, "./data.json")
# 
# V(network)$count = countWithMe[match(V(network)$name, names(countWithMe))]
# V(network)$count <- ifelse(V(network)$count == 0, 5, 3.5*V(network)$count)
# # V(network)$count[V(network)$name == myName] = 15
# 
# V(network)$primary = "#324158"
# V(network)$primary[V(network)$name %in% myPubYears] = "white"
# 
# V(network)$label = V(network)$name
# 
# V(network)$shadow = "rgb(255, 255, 255) 2px 0px 0px, rgb(255, 255, 255) 1.75517px 0.958851px 0px, rgb(255, 255, 255) 1.0806px 1.68294px 0px, rgb(255, 255, 255) 0.141474px 1.99499px 0px, rgb(255, 255, 255) -0.832294px 1.81859px 0px, rgb(255, 255, 255) -1.60229px 1.19694px 0px, rgb(255, 255, 255) -1.97999px 0.28224px 0px, rgb(255, 255, 255) -1.87291px -0.701566px 0px, rgb(255, 255, 255) -1.30729px -1.51361px 0px, rgb(255, 255, 255) -0.421592px -1.95506px 0px, rgb(255, 255, 255) 0.567324px -1.91785px 0px, rgb(255, 255, 255) 1.41734px -1.41108px 0px, rgb(255, 255, 255) 1.92034px -0.558831px 0px"
# V(network)$shadow[V(network)$name %in% myPubYears] = "rgb(50, 65, 88) 2px 0px 0px, rgb(50, 65, 88) 1.75517px 0.958851px 0px, rgb(50, 65, 88) 1.0806px 1.68294px 0px, rgb(50, 65, 88) 0.141474px 1.99499px 0px, rgb(50, 65, 88) -0.832294px 1.81859px 0px, rgb(50, 65, 88) -1.60229px 1.19694px 0px, rgb(50, 65, 88) -1.97999px 0.28224px 0px, rgb(50, 65, 88) -1.87291px -0.701566px 0px, rgb(50, 65, 88) -1.30729px -1.51361px 0px, rgb(50, 65, 88) -0.421592px -1.95506px 0px, rgb(50, 65, 88) 0.567324px -1.91785px 0px, rgb(50, 65, 88) 1.41734px -1.41108px 0px, rgb(50, 65, 88) 1.92034px -0.558831px 0px"
# 
# # add coordinates for each year
# yearVs <- !is.na(as.numeric(V(network)$name))
# yearYs <- seq(-length(yearVs), length(yearVs), len=sum(yearVs))
# V(network)$cat = 0
# V(network)$cat[yearVs] = yearYs
# V(network)$catStrength = as.numeric(yearVs)
# 
# url <- read.csv("url.csv")
# 
# V(network)$url = NA
# V(network)$url[match(url$name,
#                      V(network)$name)] = as.character(url$url)
# 
# V(network)$hasurl <- !is.na(V(network)$url)
# 
# V(network)$offlabel = V(network)$name
# V(network)$offlabel[V(network)$name %in% myCoAuth]=""
# V(network)$offlabel[match(url$name,
#                        V(network)$name)]= as.character(url$label)
# 
# V(network)$width = 0
# V(network)$width[V(network)$name %in% myPubYears] = 5
# V(network)$strokeCol = "#324158"
# V(network)$strokeCol[V(network)$name %in% myPubYears] = "#324158"

