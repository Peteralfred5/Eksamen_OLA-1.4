library(dkstat)
library(ggplot2) # brugt til at lave et plot
library(tidyr)  # brugt til at omforme det sidste data
library(ggcorrplot)

dstsearch <- dst_search("Forbrug")

print(dstsearch)

metadata <- dst_meta("FU02", lang = "da")

print(metadata)

alko_query <- list(
  PRISENHED = "Faste priser",
  KONSUMGRP = c("02.1.1.1 Spiritus og likør",
                "02.1.1.2 Alkoholiske læskedrikke",
                "02.1.2.1 Vin af druer",
                "02.1.2.2 Vin af andre frugter",
                "02.1.2.3 Hedvin",
                "02.1.2.4 Vinbaserede drikkevarer og alkoholfri vin",
                "02.1.3.1 Pilsnerøl, guldøl",
                "02.1.3.2 Andre alkoholholdige øl",
                "02.1.3.3 Øl med lavt alkoholindhold og alkoholfri øl",
                "02.1.3.4 Øl-baserede drikkevarer"),
  Tid = "*"
)


# indlæs fil via api'en 
alko_api <- dst_get_data(table = "FU02", query = alko_query, lang = "da")

# reshape dataen fra lang to bred
alko_api <- reshape(
  alko_api,
  timevar = "KONSUMGRP",
  idvar = "TID",
  direction = "wide")

#### Subsetting område ####

# fjern hver anden kolone fordi kolonen består kun af "faste priser"
alko_api <- alko_api[, seq(1, ncol(alko_api), by = 2)]

#simpel gsub til at fjerne value fra overskrifterne (man kan nøjes med at bruge names fordi vi har brugt reshape)
names(alko_api) <- gsub("value\\.", "", names(alko_api))


#indlæs filen via csv 
alko <- read.csv("FU02.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE,
                 strip.white = TRUE, na.strings = c("NA",""), fileEncoding = "latin1")

#fjerne alle na værdier i df
alko <- na.omit(alko)

#reset række numre 
rownames(alko) <- NULL

# lave årstal fra første række om til headers
colnames(alko) <- alko[1, ]

#fjern de resterende årstal og # fjern `.1` kolonen fra dataframen 
alko <- alko[-1, -1]

# fjern `.1` kolonen fra dataframen 
#alko <- alko[, -1]

# rotere alko med brug af transpose
alko <- as.data.frame(t(alko))

# ændre overskrifterne til kolonenavne
colnames(alko) <- alko[1, ]
alko <- alko[-1, ]

# skabe en ny kolone med årstal for at bedre kunne bruge ggplot
alko$Year <- as.numeric(rownames(alko))

# fjern de tal som er med i overskrifterne
colnames(alko) <- gsub("^[0-9.]+ ", "", colnames(alko))

# lave den nuværende kolonenavne med årstal om til en kolone så den kan bruges i plot
alko$Year <- as.numeric(rownames(alko))

#flyt den nye årstal kolone til den første række
alko <- alko[, c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

# slet årene optil 2000
alko <- alko[-(1:6), ]

# reset colnames
rownames(alko) <- NULL

library(ggplot2) # brugt til at lave et plot
library(tidyr)  # brugt til at omforme det sidste data

# Omdanne data til long format (LAVE EKSEMPEL MED BASE-R)
alko_long <- pivot_longer(alko, cols = -Year, names_to = "Kategori", values_to = "Forbrug")

# konvertere Forbrug kolone til numeriske 
alko_long$Forbrug <- as.numeric(alko_long$Forbrug)

# Fjerne denne kolone fordi det er en sammensmeltning af alle kategorier
alko_long <- alko_long[alko_long$Kategori != "ALKOHOLISKE DRIKKEVARER OG TOBAK", ]
alko$`ALKOHOLISKE DRIKKEVARER OG TOBAK` <- NULL

# lave alle koloner om til numeriske ved brug af lapply
alko[] <- lapply(alko, as.numeric)

forbrug_2022 <- data.frame(
  Kategori = c("Spiritus og Likør", "Vin af druer", "Pilsner og Guldøl"),
  Forbrug = c(867, 2530, 888)
)

# lave row index til at hjælpe med at danne totalforbrug for et bestemt 
row_index <- 23

# lave en dataframe med det totale forbrug for året 2022
totalforbrug_2022 <- as.data.frame(sum(alko[row_index, names(alko) != "Year"]))

# omforme dtaframen så jeg kan udføre en rbind i næste kode stykke
totalforbrug_2022$Kategori <- "Alle"
colnames(totalforbrug_2022)[1] <- "Forbrug"
totalforbrug_2022 <- totalforbrug_2022[, c(2,1)]

# rbind de to dtaframe sammen 
forbrug_2022 <- rbind(forbrug_2022, totalforbrug_2022)

# lave en barplot med tre søjler for at vise procentvis hvad de udgør af hele forbruget 
plot_data <- forbrug_2022[1:3, ] # Vælg de første 3 rækker
plot_data$Procent <- round((plot_data$Forbrug / forbrug_2022$Forbrug[4]) * 100, 1) # Beregn procent

#### Lav bar-plot med de tre største kategorier ####
ggplot(plot_data, aes(x = Kategori, y = Procent, fill = Kategori)) +
  geom_bar(stat = "identity", width = .6) +  # Bar-graf
  scale_fill_manual(values = c("#F9B041", "#B8D6EE", "#002E6D")) +  # Farver til søjler
  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) + 
  labs(
    title = paste0("Tilsammen udgør vores tre største kategorier ", 
                   sum(plot_data$Procent), " %"),
    x = "Kategori",
    y = "Procent (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Roter x-aksen
    plot.title = element_text(hjust = 0.5, size = 14)   # Centrer titlen
  )

#### Plot over alle kategorier ####
# Skabe plot over alle kategorier målt i kr pr. husstand 
ggplot(alko_long, aes(x = Year, y = Forbrug, color = Kategori, group = Kategori)) +
  geom_line(size = 0.5) +  # Tyggelse af linjerne
  geom_point(size = 0) +   # størrelsen af punkterne
  labs(
    title = "Dansker bruger flest penge på Vin af druer",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +  # punkter på y axis
  theme_minimal(base_size = 14) +  # Minimal theme with larger text
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),  # Sæt titlen i midten af plottet
    legend.position = "bottom",                         # Move legend to the bottom
    legend.title = element_text(size = 12),             # Adjust legend title size
    legend.text = element_text(size = 10)               # Adjust legend text size
  )

# fokus på de fire kategorier (LIGEGYLDIGT PLOT)

# Reshape data from wide to long format
alko_long <- pivot_longer(alko, cols = -Year, names_to = "Kategori", values_to = "Forbrug")

# Convert the Forbrug column to numeric
alko_long$Forbrug <- as.numeric(alko_long$Forbrug)

# Remove the unwanted category
alko_long <- alko_long[alko_long$Kategori != "ALKOHOLISKE DRIKKEVARER OG TOBAK", ]

# Define the categories to highlight
highlight_categories <- c(
  "Spiritus og likør",
  "Vin af druer",
  "Pilsnerøl, guldøl",
  "Øl med lavt alkoholindhold og alkoholfri øl"
)

# Add a new column to group categories into highlight or other
alko_long$color_group <- ifelse(
  alko_long$Kategori %in% highlight_categories,
  alko_long$Kategori,  # Keep the category name for highlighted ones
  "Andre"  # Group all others into "Andre"
)

# Define custom colors for highlighted categories and gray for others
custom_colors <- c(
  "Spiritus og likør" = "blue",
  "Vin af druer" = "purple",
  "Pilsnerøl, guldøl" = "green",
  "Øl med lavt alkoholindhold og alkoholfri øl" = "red",
  "Andre" = "gray"
)

# Create the plot
ggplot(alko_long, aes(x = Year, y = Forbrug, color = color_group, group = Kategori)) +
  geom_line(size = 0.5) +  # Line thickness
  geom_point(size = 0) + # Point size
  labs(
    title = "Udvikling i alkoholdforbrug over tid",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  theme_minimal(base_size = 14) +  # Minimal theme with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centered title
    legend.position = "bottom",             # Legend at the bottom
    legend.title = element_text(size = 12), # Legend title size
    legend.text = element_text(size = 10)   # Legend text size
  )

#### NYE PLOT MED FOKUS PÅ DE 3 STORE KATEGORIER ####

# Filtrer kun de tre ønskede kategorier med base R
plot_data1 <- alko_long[alko_long$Kategori %in% c("Spiritus og likør", "Vin af druer", "Pilsnerøl, guldøl"), ]

# Skab plottet
ggplot(plot_data1, aes(x = Year, y = Forbrug, color = Kategori, group = Kategori)) +
  geom_line(size = 0.8) +  # Tykkelsen af linjerne
  geom_point(size = 2) +   # Størrelsen af punkterne
  labs(
    title = "Dansker bruger flest penge på Vin af druer",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +  # Punkter på y-aksen
  scale_color_manual(  # Angiv farverne manuelt
    values = c(
      "Spiritus og likør" = "#B8D6EE",   
      "Vin af druer" = "#002E6D",       
      "Pilsnerøl, guldøl" = "#F9B041"   
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),  # Centrer titlen
    legend.position = "bottom",                         # Flyt legen til bunden
    legend.title = element_text(size = 12),             # Størrelse på legen-titel
    legend.text = element_text(size = 10)               # Størrelse på legen-tekst
  )

#### PLOT TIL DE TO KATEGORIER SOM OVERLAPPER I 2016 ####

# Filtrer kun de to ønskede kategorier i base R
plot_data2 <- alko_long[alko_long$Kategori %in% c("Pilsnerøl, guldøl", "Andre alkoholholdige øl"), ]

# Genskab plottet
ggplot(plot_data2, aes(x = Year, y = Forbrug, color = Kategori, group = Kategori)) +
  geom_line(size = 0.8) +  # Tykkelsen af linjerne
  geom_point(size = 2) +   # Størrelsen af punkterne
  labs(
    title = "Pilsnerøl og andre alkoholholdige øl overlapper i 2016",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +  # Punkter på y-aksen
  scale_color_manual(  # Angiv farver manuelt
    values = c(
      "Pilsnerøl, guldøl" = "#F9B041",          
      "Andre alkoholholdige øl" = "#002E6D"     
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),  # Centrer titlen
    legend.position = "bottom",                         # Flyt legenden til bunden
    legend.title = element_text(size = 12),             # Størrelse på legen-titel
    legend.text = element_text(size = 10)               # Størrelse på legen-tekst
  )

#### PLOT DE SAMME TO KATEGORIER MEN UDEN DE 2 ÅR DE OVERLAPPER ####

# Filtrer kun de to ønskede kategorier og fjern årene 2015 og 2016
plot_data3 <- alko_long[alko_long$Kategori %in% c("Pilsnerøl, guldøl", "Andre alkoholholdige øl") &
                          !alko_long$Year %in% c(2015, 2016), ]

# Genskab plottet
ggplot(plot_data3, aes(x = Year, y = Forbrug, color = Kategori, group = Kategori)) +
  geom_line(size = 0.8) +  # Tykkelsen af linjerne
  geom_point(size = 2) +   # Størrelsen af punkterne
  labs(
    title = "Pilsnerøl og andre alkoholholdige øl uden 2015 og 2016",
    x = "År",
    y = "Faste priser i kr. pr. husstand",
    color = "Kategori"
  ) +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +  # Punkter på y-aksen
  scale_color_manual(  # Angiv farver manuelt
    values = c(
      "Pilsnerøl, guldøl" = "#F9B041",          
      "Andre alkoholholdige øl" = "#002E6D"     
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16),  # Centrer titlen
    legend.position = "bottom",                         # Flyt legenden til bunden
    legend.title = element_text(size = 12),             # Størrelse på legen-titel
    legend.text = element_text(size = 10)               # Størrelse på legen-tekst
  )


#### PROCENTVIS ÆNDRING AF DE TRE KATEGORIER ####

# Spiritus og likør
# værdi i 2022 - værdi i 2000 / værdi i 2000
ændring1 <- (alko[23,2]-alko[1,2])/alko[1,2]
procent_ændring1 <- (867-697)/697
print(procent_ændring)
#24% ændring

# Vin af druer
ændring2 <- (alko[23,4]-alko[1,4])/alko[1,4]
print(ændring2)
#-6% ændring 

# Pilsnerøl, guldøl
ændring3 <- (alko[23,8]-alko[1,8])/alko[1,8]
print(ændring3)
#-51%

#### 4.2 Korrelationsmatrix #### 

# lav en dataframe med disse tre koloner fra alko dataframen 
kor_mat_plot <- alko[, c("Spiritus og likør", "Vin af druer", "Pilsnerøl, guldøl", "Andre alkoholholdige øl")]

# konvertere koloner til numerics
kor_mat_plot$`Spiritus og likør` <- as.numeric(kor_mat_plot$`Spiritus og likør`)
kor_mat_plot$`Vin af druer` <- as.numeric(kor_mat_plot$`Vin af druer`)
kor_mat_plot$`Pilsnerøl, guldøl` <- as.numeric(kor_mat_plot$`Pilsnerøl, guldøl`)
kor_mat_plot$`Andre alkoholholdige øl` <- as.numeric(kor_mat_plot$`Andre alkoholholdige øl`)

# Beregn korrelationsmatrix
kor_matrix <- cor(kor_mat_plot, use = "complete.obs")

# Vis korrelationsmatrix
print(kor_matrix)

# Plot korrelationmatrixen
ggcorrplot(kor_matrix, 
           method = "square",    # Shape of the plot (square tiles)
           type = "lower",       # Display only lower triangle of the matrix
           lab = TRUE,           # Add correlation coefficients to the plot
           lab_size = 5,         # Size of the labels
           colors = c("blue", "white", "red"), # Color gradient
           title = "Stiger danskernes forbrug af vin eller øl, stiger modparten ligeledes",
           legend.title = "Corr") # Title of the legend


#### Korrelationsmatrix uden 2015-16 ####

# Lav en kopi af data
alko_filtered1 <- alko

# Sæt værdier til NA for Pilsnerøl, guldøl og Andre alkoholholdige øl i 2015 og 2016
alko_filtered1$`Pilsnerøl, guldøl`[alko_filtered1$Year %in% c(2015, 2016)] <- NA
alko_filtered1$`Andre alkoholholdige øl`[alko_filtered1$Year %in% c(2015, 2016)] <- NA

# Opret en ny dataframe med de ønskede kolonner
kor_mat_plot <- alko_filtered1[, c("Pilsnerøl, guldøl", "Andre alkoholholdige øl")]

# Konverter kolonner til numerics (hvis nødvendigt)
kor_mat_plot[] <- lapply(kor_mat_plot, as.numeric)

# Beregn korrelationsmatrix med de opdaterede data
kor_matrix <- cor(kor_mat_plot, use = "pairwise.complete.obs")

# Udskriv korrelationsmatrix
print(kor_matrix)

ggcorrplot(kor_matrix, 
           method = "square",    # Shape of the plot (square tiles)
           type = "lower",       # Display only lower triangle of the matrix
           lab = TRUE,           # Add correlation coefficients to the plot
           lab_size = 5,         # Size of the labels
           colors = c("blue", "white", "red"), # Color gradient
           title = "Korrelation uden 2015-2016 for udvalgte kolonner",
           legend.title = "Corr")

