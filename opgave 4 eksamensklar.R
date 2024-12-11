library(tidyverse)
library(ggplot2)


# Indlæs filen
fu02_data <- read.csv("FU02.csv", header = FALSE, stringsAsFactors = FALSE)

# Fjern de første fem rækker, rækker 16 og 17 samt den første kolonne
fu02_data <- fu02_data[-c(1:5, 16:17), -1]

# Tilpas kolonnenavne: Første kolonne som "Kategori", og resten som årstal fra 2000-2022
names(fu02_data) <- c("Kategori", paste0(2000:2022))

# Rens kategorier for specialtegn
fu02_data$Kategori <- iconv(fu02_data$Kategori, from = "latin1", to = "UTF-8")

# Lav en liste med dine egne titler, en for hver kategori
konkluderende_titler <- c(
  "Fluktuerende forbrug på spiritus og likør med stigning efter 2015",
  "Stigning i forbrug af alkoholiske læskedrikke",
  "Markante udsving i forbrug af vin af druer med højeste niveau omkring 2010",
  "Fald i forbrug af vin af andre frugter",
  "Ustabilt forbrug af hedvin med gentagne stigninger og fald over perioden",
  "Kraftig stigning i forbrug af vinbaserede drva. omkring 2010 efterfulgt af et markant fald",
  "Fald i forbrug af pilsnerøl og guldøl",
  "Stigning i forbrug af andre alkoholh. øl omkring 2015 med efterfølgende stabilisering",
  "Øget forbrug af alkoholfri øl",
  "Kort stigning i forbrug af ølbaserede drva. omkring 2015, efterfulgt af et brat fald"
)

# Gå igennem hver række og plot med specifik titel
for (i in 1:nrow(fu02_data)) {
  kategori <- fu02_data$Kategori[i]         # Hent kategorinavn
  år <- colnames(fu02_data)[-1]            # Kolonnenavne som år
  værdier <- as.numeric(fu02_data[i, -1])  # Forbrugsdata for kategorien
  
  # Hent den konkluderende titel for denne kategori
  titel <- konkluderende_titler[i]
  
  # Lav en simpel graf
  plot(
    as.numeric(år), værdier,
    type = "l", col = "blue", lwd = 2,
    xlab = "År", ylab = "Gennemsnitligt forbrug pr. husstand (kr.)",
    main = titel,
    sub = "Kilde: Danmarks Statistik"
  )
}

# Opg 4.2 - Korrelation 

# Fjern kategorikolonnen og gør årene til rækker (transponér data)
numeriske_data <- fu02_data[, -1]  # Ekskluder kategorikolonnen
rownames(numeriske_data) <- fu02_data$Kategori  # Brug "Kategori" som rækkeetiketter
transponeret_data <- t(numeriske_data)  # Transponér data

# Beregn korrelationsmatrix mellem forbrugsgrupperne
korrelationer <- cor(transponeret_data, use = "complete.obs", method = "pearson")

# Visualisering af korrelationerne 
library(corrplot)
corrplot(
  korrelationer, 
  method = "number",       # Vis kun værdier som tal
  type = "upper",          # Kun den øvre trekant
  col = "black",           # Sort tekst for tallene
  cl.col = "black",        # Sort farve på bjælken
  cl.cex = 0.8,            # Justér tekststørrelse for bjælken
  tl.col = "black",        # Sort farve for labels (forbrugsgrupper)
  tl.cex = 0.8,            # Justér labelstørrelsen
  number.cex = 0.8         # Justér størrelsen på tallene
)



#plot for at de gemmes samlet

# Opsætning: 5 rækker x 2 kolonner, med justerede marginer
par(mfrow = c(5, 2), mar = c(6, 4, 2, 1))  # Giver ekstra plads i bunden (c(6, 4, 2, 1))

# Gå igennem og plot alle grafer
for (i in 1:nrow(fu02_data)) {
  kategori <- fu02_data$Kategori[i]
  år <- colnames(fu02_data)[-1]
  værdier <- as.numeric(fu02_data[i, -1])
  titel <- konkluderende_titler[i]
  
  # Plot hver graf med plads til kilde
  plot(
    as.numeric(år), værdier,
    type = "l", col = "blue", lwd = 2,
    xlab = "År", ylab = "Gennemsnitligt forbrug pr. husstand (kr.)",
    main = titel
  )
  
  # Tilføj kilden som tekst under grafen
  mtext(
    "Kilde: Danmarks Statistik, ", 
    side = 1, line = 4, cex = 0.8
  )
}


