library(ggplot2)
##### Ola 1 #####

#### opgave 3.1 ####

roll_prob <- function(n){
  roll5 <- sample(1:6, n, replace = TRUE)
  sum5 <- sum(roll5 == 5)
  prob5 <- sum5/n*100
  return(list(sum5, prob5))
}

roll_prob(25000)

#Her var der 4155 gange der var nummeret 5.
#det vil så sige at der er 4155/25000=0,166==16,6%


#### opgave 3.2 Plot 1 ####

# lav et script der slår med 6 terninger og vis summen
roll2 <- sum(sample(1:6, 6, replace = TRUE))
# test terning
roll2

# slå nu 10.000 gange og lav et barplot af resultatet
roll2_6 <- replicate(10000, sum(sample(1:6, 6, replace = TRUE)))

# test
roll2_6

# lav en tabel over alle terning kast
roll_count <- as.data.frame(table(roll2_6))
View(roll_count)
# opsæt barplot over 10.000 kast
ggplot(data = roll_count)+
  geom_bar(aes(x = roll2_6, y = Freq), stat = "identity")+
  labs(x = "Sum af terningkast", 
       y = "Antal forekomster",
       title = "Summen af terningekast ligger oftest mellem 18 og 23")


#### opgave 3.3 plot 2 ####

# brug vores sample fra opgave 3.2 og slå 1.000.000 gange og lav et barplot
roll_1mil <- replicate(1000000, sum(sample(1:6, 6, replace = TRUE)))
# test
roll_1mil
# lav en tabel over kast
count_of_rolls <- as.data.frame(table(roll_1mil))
# opsæt et barplot over 1.000.000 kast
ggplot(data = count_of_rolls)+
  geom_bar(aes(x = roll_1mil, y = Freq), stat = "identity")+
  labs(x = "Sum af terningkast",
       y = "Antal forekomster",
       title = "Fordeling af 1.000.000 kast med 6 terninger ligner en normalfordeling")

#### opgave 3.4 Lav dine egne data ####

# Generer en tilfældig række af tal
Random_numbers <- sample(c(1,2,3,5,6), size = 5)

# Test resultatet
print(Random_numbers)

# Opret to matricer
matrix1 <- matrix(data = (2:6), nrow = 5, ncol = 1)
matrix2 <- matrix(data = Random_numbers, nrow = 5, ncol = 1, dimnames = NULL)

# Kombiner matricerne
CM <- cbind(matrix1, matrix2)
colnames(CM) <- NULL

# Vis den kombinerede matrix
print(CM)

