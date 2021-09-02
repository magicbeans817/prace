library(dplyr)
library(readxl)

data <- read_excel("data_prace.xls")
data %>% dim()

########################### Logistický regresní model

vars <- c("age", "sex", "edu", "c1", "c3", "c6_1", "c6_2", "c6_3", "c6_4","c6_6") #explanatory variables


data_1 <- data[, c(vars, "c4")] #subset puvodniho datasetu

data_1 %>% count(c4) #jen at mam poneti, ceho je tam kolik - pomer rozumny pro delani modelu

cols <- data_1 %>% colnames()
data_1[cols] <- lapply(data_1[cols], factor)
data_1$age <- data_1$age %>% as.numeric


data_1$response <- ifelse(data_1$c4 == "Ano", 1, 0)

data_1 %>% glimpse()

fmla <- as.formula(paste("response ~", paste(vars, collapse= "+")))
fmla

model1 <- glm(fmla, data = data_1, family = binomial("logit"))
summary(model1)


# Kvalita modelu

predikce <- predict(model1, data_1, type = "response")
predikce

library(pROC)

ROC <- roc(data_1$response, predikce)

# ROC køivka
plot(ROC, col = "blue")

# Plocha pod ROC køivkou
auc(ROC)


########################### Shluková analýza (clustering)

data_2 <- select(data, 19:26)

for (i in 1:ncol(data_2)){
  for (j in 1: nrow(data_2)){
    if (data_2[j,i] == "10 Velice èasto") {
      data_2[j,i] <- "10"
    }
  }
}

for (i in 1:ncol(data_2)){
  for (j in 1: nrow(data_2)){
    if (data_2[j,i] == "1 Témìø nikdy") {
      data_2[j,i] <- "1"
    }
  }
}

cols <- data_2 %>% colnames()
data_2[cols] <- lapply(data_2[cols], as.numeric)

data_2 %>% glimpse()


# Elbow method
library(ggplot2)
library(purrr)

options(repr.plot.width=4, repr.plot.height=4)
set.seed(420)
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = data_2, centers = k, iter.max = 10)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10)

#Silhouette width
library(cluster)

options(repr.plot.width=4, repr.plot.height=4)
set.seed(420)
sil_width <- map_dbl(2:10, function(k){
  model = pam(x = data_2, k = k)
  model$silinfo$avg.width
})

sil_data <- data.frame(
  k = 2:10,
  sil_width = sil_width
)

ggplot(sil_data, aes(x = k, y = sil_width)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)


#Rozdìlení
table1 <- data %>% count(edu)
table1 <- rbind(table1, table1)
table1 <- rbind(table1, table1)
table1


library(dendextend)
dist_prace <- dist(data_2, method = "euclidean")
hc_prace <- hclust(dist_prace, method = "complete")
dend_prace <- as.dendrogram(hc_prace)
options(repr.plot.width=8, repr.plot.height=5)
dend_colored <- color_branches(dend_prace, k = 4)
plot(dend_prace)

cluster_assignments <- cutree(hc_prace, k = 4) #hierarchicke shlukovani
vzdelani_shluky <- data %>%
  mutate(hc = cluster_assignments) %>%
  count(hc, educat)



data_2 <- data_2 %>%
  mutate(hc = cluster_assignments)
prumery <- aggregate(data_2, by=list(cluster=data_2$hc), mean) %>%round(2)

### Výsledky
prumery
vzdelani_shluky

