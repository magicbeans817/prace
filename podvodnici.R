install.packages("stopwords")
install.packages("haven")
install.packages("utils")
install.packages("dplyr")
install.packages("stringr")

library(haven)
library(stopwords)
library(utils)
library(dplyr)
library(stringr)

setwd("H:/podvodnici") #set your working directory

########


pridej_sloupec_plny_nul <- function(data){
  sloupec_nul <- rep(0, nrow(data))
  datasesloupcem <- cbind(data, sloupec_nul)
  return(datasesloupcem)
}

pocet_stopek <- function(data){
  
  #Stopwords
  stopky <- stopwords::stopwords("cs", source = "stopwords-iso")
  stopky %>% head()
  nase_stopky <- c("ano", "ne", "nikdy", "nijak", "nwm", "Nevím","nevim","nevím", "Nevim")
  stopky <- c(stopky, nase_stopky)
  
  data <- pridej_sloupec_plny_nul(data)
  colnames(data)[ncol(data)] <- "pocet_stopu"
  pom_data <- data[, sapply(data, class) == 'character']
  for (sloupec in 1:ncol(pom_data)) {
    for (radek in 1:nrow(pom_data)) {
      #rozdeleno <- pom_data[radek, sloupec] %>% tolower() #nechci riskovat, ze na serveru nepujde tm
      #rozdeleno <- rozdeleno %>% tm::removeNumbers()
      #rozdeleno <- rozdeleno %>% tm::removePunctuation()
      if (pom_data[radek, sloupec] == ""){
        pom_data[radek, sloupec] <- "chybi-hodnota"
      }
      rozdeleno <- strsplit(tolower(pom_data[radek,sloupec]), " ") %>% unlist()
      for (clen in 1:length(rozdeleno)) {
        if(rozdeleno[clen] == "sys_filtered_off" | rozdeleno[clen] == "sys_empty" ){
          break
        }
        for (stopka in 1:length(stopky)) {
          if (rozdeleno[clen] == stopky[stopka]){
            data[radek, "pocet_stopu"] <- data[radek, "pocet_stopu"] + 1
            break
          }
        }
      }
    }
  }
  return(data)
}

pocet_hlasek <- function(data){
  
  samohlasky <- c("a", "e", "i", "y", "o", "u", "á", "í", "é", "ý", "ě", "ů", "ú", "ó")
  souhlasky <- c("h", "c", "k", "r", "d", "t", "n", "ž", "š","č", "ř", "j", "b", "f", "l",
                 "m", "p", "s", "v", "z")
  
  data <- pridej_sloupec_plny_nul(data)
  data <- pridej_sloupec_plny_nul(data)
  colnames(data)[(ncol(data)-1):ncol(data)] <- c("samohlasky", "souhlasky")

  pom_data <- data[, sapply(data, class) == 'character']
  for (sloupec in 1:ncol(pom_data)) {
    for (radek in 1:nrow(pom_data)) {
      if (pom_data[radek, sloupec] == "sys_filtered_off" |
          pom_data[radek, sloupec] == "sys_empty" |
          pom_data[radek, sloupec] == ""){
        pom_data[radek, sloupec] <- "0"
      }
        rozdeleno <- strsplit(tolower(pom_data[radek, sloupec]), "") %>% unlist()
        for (clen in 1:length(rozdeleno)) {
          for (samohlaska in 1:length(samohlasky)) {
            if (rozdeleno[clen] == samohlasky[samohlaska]){
              data[radek, "samohlasky"] <- data[radek, "samohlasky"] + 1
            }
          }
          for (souhlaska in 1:length(souhlasky)) {
            if (rozdeleno[clen] == souhlasky[souhlaska]){
              data[radek, "souhlasky"] <- data[radek, "souhlasky"] + 1
            }
          }
        }
      
    }
  }
  return(data)
}

vyfiltruj_podezrele <- function(data){
  data <- data %>% filter(pocet_stopu == 0)
  respidy <- data$respid
  p_cprespid <- data$p_cprespid
  sou <- data$souhlasky
  sam <- data$samohlasky
  data <- data[, sapply(data, class) == 'character']
  data <- cbind(data, sou)
  data <- cbind(data, sam)
  data <- cbind(respidy, data)
  data <- cbind(p_cprespid, data)
  return(data)
}

podezreli <- function(data){
  data <- pocet_stopek(data)
  data <- pocet_hlasek(data)
  data <- vyfiltruj_podezrele(data)
  return(data)
}

seradit_filtrovat <- function(data){
  data <- podezreli(data)
  data <- data %>%
    mutate(nasobek = sam * sou) %>%
    arrange(nasobek)
  data <- select(data, -sou, -sam, -nasobek)
  return(data)
}


#### Testovani otevrenek
############################################################################################
####
data1 <- haven::read_sav("data_netbus_289_v2.sav")
data1$p_cprespid <- 0

View(data1)
#data1 %>% seradit_filtrovat() %>% system.time()
data1 <- data1 %>% seradit_filtrovat()
View(data1)

####

data3 <- haven::read_sav("data_netbus_289_v1.sav")
data3$p_cprespid <- 0

#data3 %>% seradit_filtrovat() %>% system.time()
data3 <- data3 %>% seradit_filtrovat()
View(data3)


####
data2 <- haven::read_sav("data_populace_01.sav")



dim(data2)

#data2 %>% seradit_filtrovat() %>% system.time()
data2 <- data2 %>% seradit_filtrovat()
View(data2)


#### Kod na testovani baterii - NEDOKONCENO!!!
############################################################################################
####

data3 <- haven::read_sav("data_netbus_289_v1.sav")
data3$p_cprespid <- 0
View(data3)


data3$p1_1 %>% class()
data3$p1_1 %>% levels()


data3$p3_2 <- factor(data3$p3_2)

data3 %>% glimpse()


data3[colnames(data3)] <- lapply(data3[colnames(data3)], factor)
matice <- lapply(data3[colnames(data3)], nlevels) %>% unlist


View(data3)

vyfiltruj_podle_levelu <- function(data){
  #vytvorim nahodnou matici na kterou budu pripojovat, na konci se zbavim prvniho radku.
  #jsem koště a neumim problem vyresit elegantne
  matice <- matrix(c("nazev", 6), nrow = 1, ncol = 2)
  for (sloupec in colnames(data)) {
    pocet <- nlevels(data$sloupec)
    #baterie maximum 11
    matice <- rbind(matice, c(sloupec, pocet))
  }
  return(matice)
}



