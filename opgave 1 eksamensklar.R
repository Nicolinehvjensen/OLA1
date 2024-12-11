#Opgave 1 

#Opgave 1.1 - find data 

boligsiden <- read.csv("boligsiden.csv")

## Find de to ejendomme
which(boligsiden$vej == "tousvej" & boligsiden$vejnr == 106)
which(boligsiden$vej == "egevej" & boligsiden$vejnr == 20)

boligsiden[71,]
boligsiden[2202,]

#Opgave 1.2 - Vælg

## Udvælg to tilfældige huse
sample(1:nrow(boligsiden), 2)

## Se info om de to huse
boligsiden[887, ]
boligsiden[2077, ]

#Opgave 1.3 - 

##Data uden NA-værdier
boligdf_clean2 <- na.omit(boligsiden)
View(boligsiden_clean)

## Rens pris for kr. og konverter til numeric
boligdf_clean2$pris <- gsub("kr\\.\\s*|\\.", "", boligsiden_clean$pris)
boligdf_clean2$pris <- as.numeric(boligsiden_clean$pris)
str(boligsiden_clean)


## Plot
ggplot(data = boligsiden_clean)+
  geom_point(mapping = aes(x = størrelse, y = pris))+
  geom_smooth(mapping = aes(x = størrelse, y = pris))+
  theme_minimal()
