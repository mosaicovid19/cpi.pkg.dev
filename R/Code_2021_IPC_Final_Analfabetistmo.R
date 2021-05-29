rm(list = ls())
library(tidyverse)
library(readxl)

# setwd("C:/Users/pchir/Downloads/CSV_RJ")
#
# basico <- read.csv("BASICO_RJ.csv", header = TRUE, sep = ";")
# entorno <- read.csv("Entorno03_RJ.csv", header = TRUE, sep = ";")
# dom.i <- read.csv("Domicilio01_RJ.csv", header = TRUE, sep = ";")
# dom.ii <- read.csv("Domicilio02_RJ.csv", header = TRUE, sep=";")
# pessoa <- read.csv("Pessoa03_RJ.csv", header = TRUE, sep=";")
# dom.renda <- read.csv("DomicilioRenda_RJ.csv", header = TRUE, sep=";")
# resp.alfa <- read.csv("DomicilioRenda_RJ.csv", header = TRUE, sep=";")

source("R/VulIndex.R", encoding = "UTF-8")

# ipc.RJ <- VulIndex(basico,entorno,dom.i,dom.ii,pessoa,dom.renda)
# dados <- reduction_dimension(ipc.RJ)

# ipc.RJ%>%str

### todos os estados ###
set.seed(1234)

#install_github("pchiroque/vulindex")
#library(vulindex)

root <- "C:/Users/pchir/Downloads"

states <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG",
            "MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR",
            "RS","SC","SE","SP1","TO")
#6,7,11,23,27
#"CE" "DF" "MG" "RS" "TO"

#st=19
full <- NULL
for(st in c(1:27)[-c(6,7,11,23,27)]){

  estado <- states[st]
  censo_dir <- file.path("~/Downloads/Censo2010")
  censo_dw <- file.path(censo_dir, "zips")
  estado <- "CE"

  # allocated data set here!
  bases <- c(
    Basico = file.path(censo_dir, paste0("Basico_", estado, ".xls")),
    Entorno = file.path(censo_dir, paste0("Entorno03_",estado,".xls")),
    Domicilio01 = file.path(censo_dir, paste0("Domicilio01_",estado,".xls")),
    Domicilio02 = file.path(censo_dir, paste0("Domicilio02_",estado,".xls")),
    Pessoa = file.path(censo_dir, paste0("Pessoa03_",estado,".xls")),
    Dom.Renda = file.path(censo_dir, paste0("DomicilioRenda_",estado,".xls")),
    Resp.Alfa = file.path(censo_dir, paste0("Responsavel02_",estado,".xls"))
  )

  # % dos arquivos encontrados
  stopifnot( mean(unlist(lapply(bases, file.exists))) == 1)

  basico <- read_excel(bases["Basico"])
  entorno <- read_excel(bases["Entorno"])
  dom.i <- read_excel(bases["Domicilio01"])
  dom.ii <- read_excel(bases["Domicilio02"])
  pessoa <- read_excel(bases["Pessoa"])
  dom.renda <- read_excel(bases["Dom.Renda"])
  resp.alfa <- read_excel(bases["Resp.Alfa"]) # não está sendo usado pela função

  ipc_f <- VulIndex(basico,entorno,dom.i,dom.ii,pessoa,dom.renda)
  ipc_f$Nome_UF <- estado
  full <- rbind(full,ipc_f)
}

ipc.full <- full

#save(ipc.full,file = "C:/Users/pchir/Google Drive/AcaoCovid/indice_vulnerabilidade-master/Pacote_IPC/IPC_FULL_UF_MUNI_BAIRRO.rda")

load("C:/Users/pchir/Google Drive/AcaoCovid/indice_vulnerabilidade-master/Pacote_IPC/IPC_FULL.rda")


ipc.Cap_states <- ipc.full%>%group_by(UF)%>%summarise_at("ipc",mean)%>%
  `colnames<-`(c("state","ipc"))

Nome_capital <- read_excel("C:/Users/pchir/Google Drive/AcaoCovid/p_R0_Vacinados/dados_R0-original/IBGE_states.xlsx",sheet = "InfoFull_IBGE")
Nome_capital%>%str
#ipc.Cap_states%>%left_join(Nome_capital)

ipc_Capitais_UF <- Nome_capital%>%left_join(ipc.Cap_states)%>%
  dplyr::select(c("state","UF","Capital","Cod_IBGE","ipc"))%>%
  as.data.frame()

#save(ipc_Capitais_UF,file = "ipc_Capitais_UF.rda")
