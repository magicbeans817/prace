library(readxl)
library(dplyr)
library(tidyr)

data <- read_excel("data.xlsx") #nactu data
head(data) #prohlednu data

#Nejdriv kouknu na data a zasadni promenne
data %>% glimpse()

data %>%
  count(a01)

data %>%
  count(a02)

#Vytvorim promennou pro ruzne prostredky, aby me reseni odpovidalo zadani
data <- data %>%
  mutate(prostredek = "0")

for (i in 1:nrow(data)){
  if (data[i,"a02"] > 9){
    data[i, "prostredek"] <- "jine_znacky"
  } else {
    data[i, "prostredek"] <- as.character(data[i, "a02"])
  }
}

########################################################################################## Prace na prvni tabulce

#Vytvorim relativni frekvence celkove
celkem_myti <- data %>% 
  count(a01)

celkem_myti$n <- celkem_myti$n / celkem_myti$n %>% sum()

#Vytvorim relativni frekvence pro muze
data_muzi <- data %>%
  filter(p_sex == 1)

muzi_myti <- data_muzi %>%
  count(a01)

muzi_myti$n <- muzi_myti$n / muzi_myti$n %>% sum()

#Vytvorim relativni frekvence pro zeny
data_zeny <- data %>%
  filter(p_sex == 2)

zeny_myti <- data_zeny %>%
  count(a01)

zeny_myti$n <- zeny_myti$n / zeny_myti$n %>% sum()

#Ted to dam dohromady a vytvorim prvni cast tabulky
horni_tabulka <- matrix(c(celkem_myti$n, muzi_myti$n, zeny_myti$n), nrow = 4, ncol = 3, byrow = FALSE)

#Pokracuji s frekvencemi mycich prostredku
celkem_prostredky <- data %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

muzi_prostredky <- data %>%
  filter(p_sex == 1) %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

zeny_prostredky <- data %>%
  filter(p_sex == 2) %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

zeny_prostredky <- rbind(zeny_prostredky[1:5,],
                         c("6", 0, 0.0000),
                         zeny_prostredky[6:8,])
zeny_prostredky$celkem <- zeny_prostredky$celkem %>% as.numeric()

dolni_tabulka <- matrix(c(celkem_prostredky$celkem, muzi_prostredky$celkem, zeny_prostredky$celkem),
                        nrow = 9, ncol = 3, byrow = FALSE)

dolni_tabulka <- rbind(dolni_tabulka, 0)
dolni_tabulka[10,] <- dolni_tabulka[9,]
dolni_tabulka[9,] <- 0

horni_tabulka <- horni_tabulka %>% round(3) * 100
dolni_tabulka <- dolni_tabulka %>% round(3) * 100

tabulka <- rbind(horni_tabulka, dolni_tabulka)
tabulka <- tabulka 

colnames(tabulka) <- c("Celkem", "Muž", "Žena")
rownames(tabulka) <- c("dennì nebo témìø dennì", "4 až 5 dní v týdnu", "3 dny v týdnu", "1 až 2 dny v týdnu",
                       "znaèka 1", "znaèka 2", "znaèka 3", "znaèka 4", "znaèka 5", "znaèka 6", "znaèka 7",
                       "znaèka 8", "znaèka 9", "jiné znaèky")

tabulka <- tabulka %>% as.data.frame()


library(openxlsx)

wb <- createWorkbook()

s1 <- "Pohlaví" #jmeno prvniho sheetu

## Add a worksheet
addWorksheet(wb, s1)

## Merguju bunky, aby podobnost s puvodnim souborem byla nejvetsi
mergeCells(wb, s1, cols = 1:2, rows = 1:2)
mergeCells(wb, s1, cols = 1, rows = 3:6)
mergeCells(wb, s1, cols = 1, rows = 7:16)
mergeCells(wb, s1, cols = 4:5, rows = 1)
mergeCells(wb, s1, cols = 3, rows = 1:2)

writeData(wb, s1, tabulka, startCol = 2, startRow = 2, colNames =  TRUE, rowNames = TRUE) #zapisu tabulku
writeData(wb, s1, "Celkem", startCol = 3, startRow = 1, xy = NULL)
writeData(wb, s1, "Pohlaví", startCol = 4, startRow = 1, xy = NULL)
writeData(wb, s1, "A1. Jak èasto myjete nádobí?", startCol = 1, startRow = 3, xy = NULL)
writeData(wb, s1, "A2. Kterou znaèku mycího prostøedku používate nejradìji?",
          startCol = 1, startRow = 7, xy = NULL)
setColWidths(wb, s1, cols = c(1,2), widths = "auto")




####################################################################### Prace na druhe tabulce - do 35 let vcetne

data_35 <- data %>%
  filter(p_age <= 35)

head(data_35)

#Vytvorim relativni frekvence celkove
celkem_myti <- data_35 %>% 
  count(a01)

celkem_myti$n <- celkem_myti$n / celkem_myti$n %>% sum()

#Vytvorim relativni frekvence pro muze
data_muzi <- data_35 %>%
  filter(p_sex == 1)

muzi_myti <- data_muzi %>%
  count(a01)

muzi_myti$n <- muzi_myti$n / muzi_myti$n %>% sum()

#Vytvorim relativni frekvence pro zeny
data_zeny <- data_35 %>%
  filter(p_sex == 2)

zeny_myti <- data_zeny %>%
  count(a01)

zeny_myti$n <- zeny_myti$n / zeny_myti$n %>% sum()

#Ted to dam dohromady a vytvorim prvni cast tabulky
horni_tabulka <- matrix(c(celkem_myti$n, muzi_myti$n, zeny_myti$n), nrow = 4, ncol = 3, byrow = FALSE)

#Pokracuji s frekvencemi mycich prostredku
celkem_prostredky <- data_35 %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

muzi_prostredky <- data_35 %>%
  filter(p_sex == 1) %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

zeny_prostredky <- data_35 %>%
  filter(p_sex == 2) %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

celkem_prostredky <- rbind(celkem_prostredky[1:3,],
                         c("4", 0, 0.0000),
                         celkem_prostredky[4:nrow(celkem_prostredky),])

celkem_prostredky <- rbind(celkem_prostredky[1:5,],
                           c("6", 0, 0.0000),
                           celkem_prostredky[6:nrow(celkem_prostredky),])

celkem_prostredky$celkem <- celkem_prostredky$celkem %>% as.numeric()

muzi_prostredky <- rbind(muzi_prostredky[1:3,],
                         c("4", 0, 0.0000),
                         muzi_prostredky[4:6,])

muzi_prostredky <- rbind(muzi_prostredky[1:4,],
                         c("5", 0, 0.0000),
                         muzi_prostredky[5:7,])

muzi_prostredky <- rbind(muzi_prostredky[1:5,],
                         c("6", 0, 0.0000),
                         muzi_prostredky[6:8,])

muzi_prostredky$celkem <- muzi_prostredky$celkem %>% as.numeric()

zeny_prostredky <- rbind(zeny_prostredky[1:3,],
                         c("4", 0, 0.0000),
                         zeny_prostredky[4:nrow(zeny_prostredky),])

zeny_prostredky <- rbind(zeny_prostredky[1:5,],
                         c("6", 0, 0.0000),
                         zeny_prostredky[6:nrow(zeny_prostredky),])

zeny_prostredky$celkem <- zeny_prostredky$celkem %>% as.numeric()

dolni_tabulka <- matrix(c(celkem_prostredky$celkem, muzi_prostredky$celkem, zeny_prostredky$celkem),
                        nrow = 9, ncol = 3, byrow = FALSE)

dolni_tabulka <- rbind(dolni_tabulka, 0)
dolni_tabulka[10,] <- dolni_tabulka[9,]
dolni_tabulka[9,] <- 0

horni_tabulka <- horni_tabulka %>% round(3) * 100
dolni_tabulka <- dolni_tabulka %>% round(3) * 100

tabulka <- rbind(horni_tabulka, dolni_tabulka)
tabulka <- tabulka 

colnames(tabulka) <- c("Celkem - do 35 let vèetnì", "Muž", "Žena")
rownames(tabulka) <- c("dennì nebo témìø dennì", "4 až 5 dní v týdnu", "3 dny v týdnu", "1 až 2 dny v týdnu",
                       "znaèka 1", "znaèka 2", "znaèka 3", "znaèka 4", "znaèka 5", "znaèka 6", "znaèka 7",
                       "znaèka 8", "znaèka 9", "jiné znaèky")

tabulka <- tabulka %>% as.data.frame()

s2 <- "Pohlaví - do 35 let vèetnì" #jmeno druheho sheetu

## Add a worksheet
addWorksheet(wb, s2)

## Merguju bunky, aby podobnost s puvodnim souborem byla nejvetsi
mergeCells(wb, s2, cols = 1:2, rows = 1:2)
mergeCells(wb, s2, cols = 1, rows = 3:6)
mergeCells(wb, s2, cols = 1, rows = 7:16)
mergeCells(wb, s2, cols = 4:5, rows = 1)
mergeCells(wb, s2, cols = 3, rows = 1:2)

writeData(wb, s2, tabulka, startCol = 2, startRow = 2, colNames =  TRUE, rowNames = TRUE) #zapisu tabulku
writeData(wb, s2, "Celkem - do 35 let vèetnì", startCol = 3, startRow = 1, xy = NULL)
writeData(wb, s2, "Pohlaví - do 35 let vèetnì", startCol = 4, startRow = 1, xy = NULL)
writeData(wb, s2, "A1. Jak èasto myjete nádobí?", startCol = 1, startRow = 3, xy = NULL)
writeData(wb, s2, "A2. Kterou znaèku mycího prostøedku používate nejradìji?",
          startCol = 1, startRow = 7, xy = NULL)
setColWidths(wb, s2, cols = c(1:20), widths = "auto")



####################################################################### Prace na treti tabulce - od 36 let vcetne

data_36 <- data %>%
  filter(p_age >= 36)

head(data_36)

#Vytvorim relativni frekvence celkove
celkem_myti <- data_36 %>% 
  count(a01)

celkem_myti$n <- celkem_myti$n / celkem_myti$n %>% sum()

#Vytvorim relativni frekvence pro muze
data_muzi <- data_36 %>%
  filter(p_sex == 1)

muzi_myti <- data_muzi %>%
  count(a01)

muzi_myti$n <- muzi_myti$n / muzi_myti$n %>% sum()

#Vytvorim relativni frekvence pro zeny
data_zeny <- data_36 %>%
  filter(p_sex == 2)

zeny_myti <- data_zeny %>%
  count(a01)

zeny_myti$n <- zeny_myti$n / zeny_myti$n %>% sum()

#Ted to dam dohromady a vytvorim prvni cast tabulky
horni_tabulka <- matrix(c(celkem_myti$n, muzi_myti$n, zeny_myti$n), nrow = 4, ncol = 3, byrow = FALSE)

#Pokracuji s frekvencemi mycich prostredku
celkem_prostredky <- data_36 %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

muzi_prostredky <- data_36 %>%
  filter(p_sex == 1) %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))

zeny_prostredky <- data_36 %>%
  filter(p_sex == 2) %>%
  count(prostredek) %>%
  mutate(celkem = n / sum(n))


muzi_prostredky <- rbind(muzi_prostredky[1:6,],
                         c("7", 0, 0.0000),
                         muzi_prostredky[7:nrow(muzi_prostredky),])

muzi_prostredky$celkem <- muzi_prostredky$celkem %>% as.numeric()

zeny_prostredky <- rbind(zeny_prostredky[1:2,],
                         c("3", 0, 0.0000),
                         zeny_prostredky[3:nrow(zeny_prostredky),])

zeny_prostredky <- rbind(zeny_prostredky[1:5,],
                         c("6", 0, 0.0000),
                         zeny_prostredky[6:nrow(zeny_prostredky),])

zeny_prostredky$celkem <- zeny_prostredky$celkem %>% as.numeric()





dolni_tabulka <- matrix(c(celkem_prostredky$celkem, muzi_prostredky$celkem, zeny_prostredky$celkem),
                        nrow = 9, ncol = 3, byrow = FALSE)

dolni_tabulka <- rbind(dolni_tabulka, 0)
dolni_tabulka[10,] <- dolni_tabulka[9,]
dolni_tabulka[9,] <- 0

horni_tabulka <- horni_tabulka %>% round(3) * 100
dolni_tabulka <- dolni_tabulka %>% round(3) * 100

tabulka <- rbind(horni_tabulka, dolni_tabulka)
tabulka <- tabulka 

colnames(tabulka) <- c("Celkem - od 36 let vèetnì", "Muž", "Žena")
rownames(tabulka) <- c("dennì nebo témìø dennì", "4 až 5 dní v týdnu", "3 dny v týdnu", "1 až 2 dny v týdnu",
                       "znaèka 1", "znaèka 2", "znaèka 3", "znaèka 4", "znaèka 5", "znaèka 6", "znaèka 7",
                       "znaèka 8", "znaèka 9", "jiné znaèky")

tabulka <- tabulka %>% as.data.frame()

s3 <- "Pohlaví - od 36 let vèetnì" #jmeno tretiho sheetu

## Add a worksheet
addWorksheet(wb, s3)

## Merguju bunky, aby podobnost s puvodnim souborem byla nejvetsi
mergeCells(wb, s3, cols = 1:2, rows = 1:2)
mergeCells(wb, s3, cols = 1, rows = 3:6)
mergeCells(wb, s3, cols = 1, rows = 7:16)
mergeCells(wb, s3, cols = 4:5, rows = 1)
mergeCells(wb, s3, cols = 3, rows = 1:2)

writeData(wb, s3, tabulka, startCol = 2, startRow = 2, colNames =  TRUE, rowNames = TRUE) #zapisu tabulku
writeData(wb, s3, "Celkem - od 36 let vèetnì", startCol = 3, startRow = 1, xy = NULL)
writeData(wb, s3, "Pohlaví - od 36 let vèetnì", startCol = 4, startRow = 1, xy = NULL)
writeData(wb, s3, "A1. Jak èasto myjete nádobí?", startCol = 1, startRow = 3, xy = NULL)
writeData(wb, s3, "A2. Kterou znaèku mycího prostøedku používate nejradìji?",
          startCol = 1, startRow = 7, xy = NULL)
setColWidths(wb, s3, cols = c(1:20), widths = "auto")

############################################################# Prace na ctvrte tabulce - jednotlive stupne vzdelani


#Tady uz jsem zjistil, jak to delat jednoduse
head(data)

horni_tabulka <- data %>%
  group_by(p_edu, a01) %>%
  count( )%>% 
  spread(key = p_edu, value = n, fill = 0)

horni_tabulka <- horni_tabulka[,-1]
horni_tabulka = apply(horni_tabulka,2,function(x){x/sum(x) * 100} %>% round(1))


dolni_tabulka <- data %>%
  group_by(p_edu, prostredek) %>%
  count( )%>% 
  spread(key = p_edu, value = n, fill = 0)

dolni_tabulka <- dolni_tabulka[,-1]
dolni_tabulka = apply(dolni_tabulka,2,function(x){x/sum(x) * 100} %>% round(1))
dolni_tabulka

dolni_tabulka <- rbind(dolni_tabulka, 0)
dolni_tabulka[10,] <- dolni_tabulka[9,]
dolni_tabulka[9,] <- 0

tabulka <- rbind(horni_tabulka, dolni_tabulka)


colnames(tabulka) <- c("Stupeò vzdìlání 1", "Stupeò vzdìlání 2", "Stupeò vzdìlání 3", "Stupeò vzdìlání 4")
rownames(tabulka) <- c("dennì nebo témìø dennì", "4 až 5 dní v týdnu", "3 dny v týdnu", "1 až 2 dny v týdnu",
                       "znaèka 1", "znaèka 2", "znaèka 3", "znaèka 4", "znaèka 5", "znaèka 6", "znaèka 7",
                       "znaèka 8", "znaèka 9", "jiné znaèky")

tabulka <- tabulka %>% as.data.frame()



s4 <- "Jednotlivé stupnì vzdìlání" #jmeno ctvrteho sheetu

## Add a worksheet
addWorksheet(wb, s4)

## Merguju bunky, aby podobnost s puvodnim souborem byla nejvetsi
mergeCells(wb, s4, cols = 1:2, rows = 1:2)
mergeCells(wb, s4, cols = 1, rows = 3:6)
mergeCells(wb, s4, cols = 1, rows = 7:16)
mergeCells(wb, s4, cols = 3:6, rows = 1)


writeData(wb, s4, tabulka, startCol = 2, startRow = 2, colNames =  TRUE, rowNames = TRUE) #zapisu tabulku
writeData(wb, s4, "Jednotlivé stupnì vzdìlání", startCol = 3, startRow = 1, xy = NULL)
writeData(wb, s4, "A1. Jak èasto myjete nádobí?", startCol = 1, startRow = 3, xy = NULL)
writeData(wb, s4, "A2. Kterou znaèku mycího prostøedku používate nejradìji?",
          startCol = 1, startRow = 7, xy = NULL)
setColWidths(wb, s4, cols = c(1:20), widths = "auto")

############################################################# Prace na pate tabulce - jednotlive kraje


head(data)

horni_tabulka <- data %>%
  group_by(p_nuts3, a01) %>%
  count( )%>% 
  spread(key = p_nuts3, value = n, fill = 0)

horni_tabulka <- horni_tabulka[,-1]
horni_tabulka = apply(horni_tabulka,2,function(x){x/sum(x) * 100} %>% round(1))


dolni_tabulka <- data %>%
  group_by(p_nuts3, prostredek) %>%
  count( )%>% 
  spread(key = p_nuts3, value = n, fill = 0)

dolni_tabulka <- dolni_tabulka[,-1]
dolni_tabulka = apply(dolni_tabulka,2,function(x){x/sum(x) * 100} %>% round(1))
dolni_tabulka

dolni_tabulka <- rbind(dolni_tabulka, 0)
dolni_tabulka[10,] <- dolni_tabulka[9,]
dolni_tabulka[9,] <- 0

tabulka <- rbind(horni_tabulka, dolni_tabulka)

rownames(tabulka) <- c("dennì nebo témìø dennì", "4 až 5 dní v týdnu", "3 dny v týdnu", "1 až 2 dny v týdnu",
                       "znaèka 1", "znaèka 2", "znaèka 3", "znaèka 4", "znaèka 5", "znaèka 6", "znaèka 7",
                       "znaèka 8", "znaèka 9", "jiné znaèky")

tabulka <- tabulka %>% as.data.frame()



s5 <- "Kraje" #jmeno pateho sheetu

## Add a worksheet
addWorksheet(wb, s5)

## Merguju bunky
mergeCells(wb, s5, cols = 1:2, rows = 1:2)
mergeCells(wb, s5, cols = 1, rows = 3:16)
mergeCells(wb, s5, cols = 1, rows = 7:16)
mergeCells(wb, s5, cols = 3:6, rows = 1)


writeData(wb, s5, tabulka, startCol = 2, startRow = 2, colNames =  TRUE, rowNames = TRUE) #zapisu tabulku
writeData(wb, s5, "Èísla krajù podle NUTS3", startCol = 3, startRow = 1, xy = NULL)
writeData(wb, s5, "A1. Jak èasto myjete nádobí?", startCol = 1, startRow = 3, xy = NULL)
writeData(wb, s5, "A2. Kterou znaèku mycího prostøedku používate nejradìji?",
          startCol = 1, startRow = 7, xy = NULL)
setColWidths(wb, s5, cols = c(1:20), widths = "auto")

############################################################# Prace na seste tabulce - velikost obce


head(data)

horni_tabulka <- data %>%
  group_by(p_size, a01) %>%
  count( )%>% 
  spread(key = p_size, value = n, fill = 0)

horni_tabulka <- horni_tabulka[,-1]
horni_tabulka = apply(horni_tabulka,2,function(x){x/sum(x) * 100} %>% round(1))


dolni_tabulka <- data %>%
  group_by(p_size, prostredek) %>%
  count( )%>% 
  spread(key = p_size, value = n, fill = 0)

dolni_tabulka <- dolni_tabulka[,-1]
dolni_tabulka = apply(dolni_tabulka,2,function(x){x/sum(x) * 100} %>% round(1))
dolni_tabulka

dolni_tabulka <- rbind(dolni_tabulka, 0)
dolni_tabulka[10,] <- dolni_tabulka[9,]
dolni_tabulka[9,] <- 0

tabulka <- rbind(horni_tabulka, dolni_tabulka)

rownames(tabulka) <- c("dennì nebo témìø dennì", "4 až 5 dní v týdnu", "3 dny v týdnu", "1 až 2 dny v týdnu",
                       "znaèka 1", "znaèka 2", "znaèka 3", "znaèka 4", "znaèka 5", "znaèka 6", "znaèka 7",
                       "znaèka 8", "znaèka 9", "jiné znaèky")

tabulka <- tabulka %>% as.data.frame()



s6 <- "Velikost obce" #jmeno pateho sheetu

## Add a worksheet
addWorksheet(wb, s6)

## Merguju bunky
mergeCells(wb, s6, cols = 1:2, rows = 1:2)
mergeCells(wb, s6, cols = 1, rows = 3:8)
mergeCells(wb, s6, cols = 1, rows = 7:16)
mergeCells(wb, s6, cols = 3:6, rows = 1)


writeData(wb, s6, tabulka, startCol = 2, startRow = 2, colNames =  TRUE, rowNames = TRUE) #zapisu tabulku
writeData(wb, s6, "Velikost obce", startCol = 3, startRow = 1, xy = NULL)
writeData(wb, s6, "A1. Jak èasto myjete nádobí?", startCol = 1, startRow = 3, xy = NULL)
writeData(wb, s6, "A2. Kterou znaèku mycího prostøedku používate nejradìji?",
          startCol = 1, startRow = 7, xy = NULL)
setColWidths(wb, s6, cols = c(1:20), widths = "auto")


saveWorkbook(wb, "tabulky_hromadny_vystup.xlsx", overwrite = TRUE)



############################################################# ID tabulky pro respondenty vyzkumu

wb2 <- createWorkbook()

matice <- matrix(c(1:10), ncol = 1, nrow = 10) %>%as.data.frame
rownames(matice) <- c("ID", "Datum", "Délka rozhovoru v sekundách", "Pohlaví",
                       "Vìk", "Vzdìlání", "Velikost obce", "Kraj", "A1. Jak èasto myjete nádobí?",
                       "A2. Kterou znaèku mycího prostøedku používate nejèastìji?")
colnames(matice) <- FALSE

x <- as.Date(data$tstart, tryFormats = "%d-%m-%Y")
datum <- format(x, "%d-%m-%Y")
head(datum)

delka <- difftime(data$tstop, data$tstart, units = "secs")
head(delka)

newdata <- data[,-ncol(data)]
head(newdata)
newdata[,2] <- datum
newdata[,3] <- delka


for (i in 1:nrow(newdata)){
  for (j in 1:ncol(newdata)){
    matice[j,1] <- newdata[i,j]
    if (j == 10){
      matice[j,1] <- matice[j,1] %>% as.numeric()
      jmeno_sheetu <- as.character(paste("ID",as.numeric(newdata[i,1])))
      addWorksheet(wb2, jmeno_sheetu)
      writeData(wb2, jmeno_sheetu, matice, startCol = 1, startRow = 1, colNames = FALSE, rowNames = TRUE)
      setColWidths(wb2, jmeno_sheetu, cols = c(1,2), widths = "auto")
    }
  }
}

saveWorkbook(wb2, "ID.xlsx", overwrite = TRUE)
