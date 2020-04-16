###############################################################################
#################### SCRIPT PARA INICIAR O APP SHINY ##########################
###############################################################################

# necessary packages to run app

app_packages <- c(
  "quantmod"
  ,"xts"
  ,"dplyr"
  ,"stringr"
  ,"zoo"
  ,"DT"
  ,"rvest"
  ,"shiny"
  ,"shinydashboard"
  ,"ggthemes"
  ,"ggplot2"
  ,"rhandsontable"
)

if (sum(app_packages %in% installed.packages()) < length(app_packages)) {
  
  # install if is not
  
  install.packages(
    pkgs = app_packages[!app_packages %in% installed.packages()]
  )
  
}

# loading packages to memory

library(quantmod)
library(xts)
library(dplyr)
library(stringr)
library(zoo)
library(rvest)
library(shiny)
library(shinydashboard)
library(ggthemes)
library(ggplot2)
library(rhandsontable)


# sourcing scripts with functions for the app

source("R/funcoes_atualizacao.R", encoding = "UTF-8")
source("R/funcoes_analise.R", encoding = "UTF-8")
source("R/funcoes_geracao_arquivos.R", encoding = "UTF-8")


shinyAppDir("shiny/")
