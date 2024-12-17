library(dkstat)

metadata <- dst_meta("FU02", lang = "da")

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

# Omdanne data til long format 
alko_long <- pivot_longer(alko, cols = -Year, names_to = "Kategori", values_to = "Forbrug")

# konvertere Forbrug kolone til numeriske 
alko_long$Forbrug <- as.numeric(alko_long$Forbrug)

# Fjerne denne kolone fordi det er en sammensmeltning af alle kategorier
alko_long <- alko_long[alko_long$Kategori != "ALKOHOLISKE DRIKKEVARER OG TOBAK", ]

# Skabe plot 
ggplot(alko_long, aes(x = Year, y = Forbrug, color = Kategori, group = Kategori)) +
  geom_line(size = 0.5) +  # Tyggelse af linjerne
  geom_point(size = 0) +   # størrelsen af punkterne
  labs(
    title = "Udvikling i alkoholdforbrug over tid",
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

# fokus på de fire kategorier

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

#### 4.2 #### 

library(ggcorrplot)

# lav en dataframe med disse tre koloner fra alko dataframen 
kor_mat_plot <- alko[, c("Spiritus og likør", "Vin af druer", "Pilsnerøl, guldøl")]

# konvertere koloner til numerics
kor_mat_plot$`Spiritus og likør` <- as.numeric(kor_mat_plot$`Spiritus og likør`)
kor_mat_plot$`Vin af druer` <- as.numeric(kor_mat_plot$`Vin af druer`)
kor_mat_plot$`Pilsnerøl, guldøl` <- as.numeric(kor_mat_plot$`Pilsnerøl, guldøl`)

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