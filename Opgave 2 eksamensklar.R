library(dplyr)
library(tidyr)
library(corrplot)

###Opgave 2.1###

#Data retrieval 
# 2552 observationer
boligdataframe <- read.csv("boligsiden.csv")

#Datapreparation 

## Fjerner NA-værdier fra datasæt 
# 2367 observationer
boligdataframe <- na.omit(boligdataframe)

##Tjekker struktur 
str(boligdataframe)

##Fjerner kr., tusindtalsseperatorer, og konverterer til numerisk format 
boligdataframe$pris <- as.numeric(gsub("\\.", "", gsub(" kr\\.", "", as.character(boligdataframe$pris))))
boligdataframe$størrelse <- as.numeric(boligdataframe$størrelse)
boligdataframe$liggetid <- as.numeric(gsub(" dag", "", boligdataframe$liggetid))
boligdataframe$grund <- as.numeric(gsub("\\.", "", as.character(boligdataframe$grund)))
boligdataframe$mdudg <- as.numeric(gsub("\\.", "", as.character(boligdataframe$mdudg)))
boligdataframe$kvmpris <- as.numeric(gsub("\\.", "", as.character(boligdataframe$kvmpris)))


##Laver opført kategori om til alder
boligdataframe$alder <- 2024 - boligdataframe$opført

summary(boligdataframe)

#Fjerner outlierst
# Fjern rækker med de 3 højeste priser
boligdataframe <- boligdataframe[order(boligdataframe$pris, decreasing = TRUE)[-1:-3], ]

boligdataframe <- boligdataframe %>%
  filter(
    pris >= 500000 & pris <= 7000000,
    kvmpris >= 5000 & kvmpris <= 50000,
    størrelse >= 50 & størrelse <= 300,
    grund >= 300 & grund <= 5000,
    opført >= 1900 & opført <= 2024,
    mdudg >= 1000 & mdudg <= 8000, # Tilføj en øvre grænse for mdudg, hvis ønsket
    værelser >= 3 # Minimum antal værelser for en villa
  )

# Tjek resultatet
summary(boligdataframe)

##Beskrivende statistik 
summary(boligdataframe[c("pris", "kvmpris", "mdudg", "grund", "størrelse", "liggetid", "alder")])

###Opgave 2.2###

# Beregn korrelation mellem størrelse (m2) og pris
korrelation <- cor(boligdataframe$størrelse, boligdataframe$pris, use = "complete.obs")

#Kun en korrelation på 0.29

model <- lm(pris ~ størrelse, data = boligdataframe)

# Se resultaterne af modellen
summary(model)

####Opgave 2.3###

# Simple regressioner med kvmpris som afhængig variabel
model_mdudg <- lm(kvmpris ~ mdudg, data = boligdataframe)
model_grund <- lm(kvmpris ~ grund, data = boligdataframe)
model_størrelse <- lm(kvmpris ~ størrelse, data = boligdataframe)
model_liggetid <- lm(kvmpris ~ liggetid, data = boligdataframe)
model_alder <- lm(kvmpris ~ alder, data = boligdataframe)

# Se resultaterne af modellerne
summary(model_mdudg)
summary(model_grund)
summary(model_størrelse)
summary(model_liggetid)
summary(model_alder)

#Mdudg forklarer bedst kvmpris 

#Korrelation
cor(boligdataframe$kvmpris, boligdataframe$mdudg)
cor(boligdataframe$kvmpris, boligdataframe$størrelse)
cor(boligdataframe$kvmpris,boligdataframe$grund)
cor(boligdataframe$kvmpris,boligdataframe$liggetid)
cor(boligdataframe$kvmpris,boligdataframe$alder)

#Scatterplot
# Plot for mdudg (månedlige udgifter) vs kvmpris
plot(boligdataframe$mdudg, boligdataframe$kvmpris,
     main = "Klar positiv sammenhæng mellem månedlige udgifter og pris pr. m²",
     xlab = "Månedlige udgifter (kr.)",
     ylab = "Pris pr. m² (kr.)",
     pch = 16, col = rgb(0.1, 0.2, 0.5, 0.5))  # scatter plot
abline(lm(kvmpris ~ mdudg, data = boligdataframe), col = "red")  # regressionslinje

# Plot for grundstørrelse vs kvmpris
plot(boligdataframe$grund, boligdataframe$kvmpris,
     main = "Negativ sammenhæng mellem grundstørrelse og pris pr. m2 ",
     xlab = "Grundstørrelse (kvm)",
     ylab = "Pris pr. m² (kr.)",
     pch = 16, col = rgb(0.1, 0.2, 0.5, 0.5))  # scatter plot
abline(lm(kvmpris ~ grund, data = boligdataframe), col = "red")  # regressionslinje

# Plot for størrelse (boligens størrelse) vs kvmpris
plot(boligdataframe$størrelse, boligdataframe$kvmpris,
     main = "Ingen sammenhæng mellem boligens størrelse og pris pr. m2",
     xlab = "Boligens størrelse (m²)",
     ylab = "Pris pr. m² (kr.)",
     pch = 16, col = rgb(0.1, 0.2, 0.5, 0.5))  # scatter plot
abline(lm(kvmpris ~ størrelse, data = boligdataframe), col = "red")  # regressionslinje

# Plot for liggetid (antal dage til salg) vs kvmpris
plot(boligdataframe$liggetid, boligdataframe$kvmpris,
     main = "Meget svag negativ sammenhæng mellem liggetid og pris pr. m²",
     xlab = "Liggetid (dage)",
     ylab = "Pris pr. m² (kr.)",
     pch = 16, col = rgb(0.1, 0.2, 0.5, 0.5))  # scatter plot
abline(lm(kvmpris ~ liggetid, data = boligdataframe), col = "red")  # regressionslinje

# Plot for alder (boligens alder) vs kvmpris
plot(boligdataframe$alder, boligdataframe$kvmpris,
     main = "Svag negativ sammenhæng mellem boligens alder og pris pr. m²",
     xlab = "Boligens Alder (år)",
     ylab = "Pris pr. m² (kr.)",
     pch = 16, col = rgb(0.1, 0.2, 0.5, 0.5))  # scatter plot
abline(lm(kvmpris ~ alder, data = boligdataframe), col = "red")  # regressionslinje


##Konfusionsmatrix 

# Vælg de relevante variabler til korrelationsmatrixen
selected_vars <- boligdataframe[c("kvmpris", "mdudg", "grund", "størrelse", "liggetid", "alder")]

# Beregn korrelationsmatrixen
correlation_matrix <- cor(selected_vars, use = "complete.obs")

# Vis korrelationsmatrixen
print(correlation_matrix)

# Ændre variabelnavnene til kun stort forbogstav
colnames(correlation_matrix) <- sapply(colnames(correlation_matrix), function(x) {
  paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "")
})
rownames(correlation_matrix) <- sapply(rownames(correlation_matrix), function(x) {
  paste(toupper(substring(x, 1, 1)), substring(x, 2), sep = "")
})

# Visualiser korrelationsmatrixen med etiketter med stort forbogstav på begge sider
corrplot(correlation_matrix, 
         method = "number",      # Kun vis tal i matrixen
         type = "upper",         # Kun den øverste del af matrixen
         col = "black",          # Farven på tallene
         tl.col = "black",       # Farven på tekstetiketterne
         tl.cex = 1.5,           # Større tekstetiketter
         diag = FALSE,           # Fjern diagonal linje
         number.cex = 0.9,       # Juster størrelsen på tallene
         mar = c(0, 0, 1, 0))    # Fjern unødvendig margin

# Tilføj en konkluderende titel
mtext("Størst korrelation mellem kvmpris og månedlige udgifter", side = 3, line = 0.5, cex = 1.2)







