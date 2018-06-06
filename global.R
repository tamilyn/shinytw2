library(phyloseq)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(DT)

#source("https://raw.githubusercontent.com/alekseyenko/Tw2/master/code/Tw2.R")
source("Tw2copy.R")

distance_choices <- phyloseq::distanceMethodList

