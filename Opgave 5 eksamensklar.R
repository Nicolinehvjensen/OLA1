###Opgave 5

# Opgave 5.1 

# Opret første kolonne "Klasse" med 9 gentagelser af hvert bogstav A, B, C, D
klasse <- rep(c("A", "B", "C", "D"), each = 9)

# Opret anden kolonne "Uge" ved hjælp af seq() til at generere tal fra 1 til 9 gentaget 4 gange
uge <- rep(seq(1, 9, by = 1), times = 4)

# Generer tredje kolonne "Score" med tilfældige decimaltal
score <- runif(36, min = 0, max = 100)

# Rund værdierne i "Score" op til nærmeste heltal
score <- ceiling(score)

# Saml kolonnerne til en data frame
dataframe <- data.frame(Klasse = klasse, Uge = uge, Score = score)

# Udskriv dataframen
print(dataframe)


#Opgave 5.2

# Opret en tom data frame til kvartalsvise observationer
kvartals_dataframe <- data.frame(Klasse = character(), 
                                 Uge = integer(),
                                 Score = numeric(),
                                 stringsAsFactors = FALSE)


for (i in seq(3, nrow(dataframe), by = 3)) {
# Hent 1. og 2. element (Klasse og Uge) fra den oprindelige data frame
  klasse <- dataframe$Klasse[i]
  uge <- dataframe$Uge[i]
  
# Beregn gennemsnittet af de forrige tre Score-værdier
  score_gennemsnit <- mean(dataframe$Score[(i-2):i])
  
 # Opret en ny række til kvartals data frame
  ny_række <- data.frame(Klasse = klasse,
                         Uge = uge,
                         Score = score_gennemsnit,
                         stringsAsFactors = FALSE)
  
# Tilføj den nye række til kvartals data frame
  kvartals_dataframe <- rbind(kvartals_dataframe, ny_række)
  
}



# Opgave 5.3 

 # Indlæs tidyr-pakken
library(tidyr)

# Konverter den eksisterende data frame til bredere format
bred_dataframe <- pivot_wider(
  data = kvartals_dataframe,
  names_from = Klasse,     # Konverterer 'Klasse' til kolonnenavne (A, B, C, D)
  values_from = Score      # Kolonnen der skal udfylde de nye kolonner
)

# Udskriv den nye brede data frame
print(bred_dataframe)
