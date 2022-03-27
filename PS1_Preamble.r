install.packages("pacman")
install.packages("igraph",  type = "binary")

pacman::p_load(data.table, ggplot2, rstudioapi, DataExplorer, bit64, boot)

setwd(dirname(getActiveDocumentContext()$path)); getwd()

data_repo <- paste0(getwd(),"/data")

dir.create(data_repo)
