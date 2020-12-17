
library(meta)
library(lme4)
library(hglm)
library(metafor)
library(shiny)
library(R2jags)
library(metafor)
library(R2jags)
library(reshape2)
path<<-getwd()

source(file = paste0(path,"/Functions/Hestimate.R"), local = TRUE)
source(file = paste0(path,"/Functions/ContCorr.R"), local = TRUE)
source(file = paste0(path,"/Functions/ReshapeData.R"), local = TRUE)
source(file = paste0(path,"/Data/Example_All.R"), local = TRUE)



