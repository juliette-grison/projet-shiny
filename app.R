library(shiny)
library(sf)
library(readxl)
library(tidyverse)
library(leaflet)

# Import base de données
leg <- read_excel("data/legislatives_2024.xlsx", col_names = TRUE)

# Import carte
leg_carte <- st_read("data/contours_circo.shp")

# suppression des 0 à gauche
leg_carte <- leg_carte |>
  mutate(id_circo = str_remove(id_circo, "^0+"))

# jointure dataframes
legislatives <- left_join(leg, leg_carte, by = c("Code circonscription législative" = "id_circo"))

# colonnes à sélectionner
colonnes <- c(
  "Code département", "Libellé département",
  "Code circonscription législative",
  "Libellé circonscription législative", "libelle",
  "Nom candidat 1", "Prénom candidat 1", "Nuance candidat 1", "Elu 1",
  "Nom candidat 2", "Prénom candidat 2", "Nuance candidat 2", "Elu 2",
  "Nom candidat 3", "Prénom candidat 3", "Nuance candidat 3", "Elu 3",
  "Nom candidat 4", "Prénom candidat 4", "Nuance candidat 4", "Elu 4",
  "Nom candidat 5", "Prénom candidat 5", "Nuance candidat 5", "Elu 5",
  "Nom candidat 6", "Prénom candidat 6", "Nuance candidat 6", "Elu 6",
  "Nom candidat 7", "Prénom candidat 7", "Nuance candidat 7", "Elu 7",
  "Nom candidat 8", "Prénom candidat 8", "Nuance candidat 8", "Elu 8",
  "Nom candidat 9", "Prénom candidat 9", "Nuance candidat 9", "Elu 9",
  "Nom candidat 10", "Prénom candidat 10", "Nuance candidat 10", "Elu 10",
  "Nom candidat 11", "Prénom candidat 11", "Nuance candidat 11", "Elu 11",
  "Nom candidat 12", "Prénom candidat 12", "Nuance candidat 12", "Elu 12",
  "Nom candidat 13", "Prénom candidat 13", "Nuance candidat 13", "Elu 13",
  "Nom candidat 14", "Prénom candidat 14", "Nuance candidat 14", "Elu 14",
  "Nom candidat 15", "Prénom candidat 15", "Nuance candidat 15", "Elu 15",
  "Nom candidat 16", "Prénom candidat 16", "Nuance candidat 16", "Elu 16",
  "Nom candidat 17", "Prénom candidat 17", "Nuance candidat 17", "Elu 17",
  "Nom candidat 18", "Prénom candidat 18", "Nuance candidat 18", "Elu 18",
  "Nom candidat 19", "Prénom candidat 19", "Nuance candidat 19", "Elu 19",
  "geometry"
)

legislatives <- legislatives |>
  select(all_of(colonnes))




# Onglet 1 :






# Onglet 2 : Florian

library(htmltools)
library(leaflet)

# Définition des couleurs par parti
couleurs <- c(
  "EXG" = "#8B0000",  "UG"  = "#FF0000",  "SOC" = "#E60000",  "ECO" = "#008000",
  "DVG" = "#FF8080",  "ENS" = "#FFD700",  "DVC" = "#DAA520",  "UDI" = "#6495ED",
  "LR"  = "#0000CD",  "DVD" = "#87CEFA",  "REG" = "#A52A2A",  "DIV" = "#808080",
  "DSV" = "#C0C0C0",  "UXD" = "#1E90FF",  "HOR" = "#4B0082",  "REC" = "#191970",
  "EXD" = "#00008B",  "RN"  = "#000080"
)

# Vecteur d'ordre des partis (de l'extrême gauche à l'extrême droite)
ordre_partis <- c("EXG", "UG", "SOC", "ECO", "DVG", "ENS", "DVC", "UDI", "LR", "DVD", 
                  "REG", "DIV", "DSV", "UXD", "HOR", "REC", "EXD", "RN")

# Génération du contenu des popups
legislatives$popup_content <- apply(legislatives, 1, function(row) {
  # En-tête du tableau
  lignes <- sprintf("<strong>%s</strong><br>
                    <table border='1' style='border-collapse: collapse;'>
                    <tr><th>Prénom</th><th>Nom</th><th>Parti</th></tr>", row["libelle"])
  
  # Création des lignes pour les candidats non vides
  candidats <- sapply(1:19, function(i) {
    nom <- row[[paste("Nom candidat", i)]]
    prenom <- row[[paste("Prénom candidat", i)]]
    nuance <- row[[paste("Nuance candidat", i)]]
    
    # Vérification pour exclure les candidats sans données
    if (!is.na(nom) && !is.na(prenom) && !is.na(nuance)) {
      couleur <- if (nuance %in% names(couleurs)) couleurs[[nuance]] else "#FFFFFF"
      
      return(list(prenom = prenom, nom = nom, nuance = nuance, couleur = couleur))
    } else {
      return(NULL)  # Ignore les candidats sans données
    }
  }, simplify = FALSE)
  
  # Supprime les candidats vides et trie selon l'ordre des partis
  candidats <- candidats[!sapply(candidats, is.null)]
  
  # Si des candidats existent, les trier par nuance
  if (length(candidats) > 0) {
    candidats_sorted <- candidats[order(match(sapply(candidats, function(x) x$nuance), ordre_partis))]
    
    # Construire les lignes du tableau triées
    lignes_candidats <- sapply(candidats_sorted, function(candidat) {
      sprintf("<tr style='background-color: %s; color: white;'>
                <td>%s</td><td>%s</td><td>%s</td></tr>", 
              candidat$couleur, candidat$prenom, candidat$nom, candidat$nuance)
    })
    
    # Fusionner les lignes et construire le tableau final
    popup_text <- paste0(lignes, paste(lignes_candidats, collapse = ""), "</table>")
  } else {
    popup_text <- paste0(lignes, "<tr><td colspan='3'>Aucun candidat</td></tr></table>")
  }
  
  HTML(popup_text)
})

# Affichage avec leaflet
leaflet(legislatives$geometry) |>
  addTiles() |>
  addPolygons(
    popup = legislatives$popup_content
  )








# Onglet 3 :







# Onglet 4 :








# Onglet 5 : Juliette
library(dplyr)

## Différents partis politiques
partis_uniques <- unique(unlist(legislatives[, grep("^Nuance candidat", names(legislatives))]))
print(partis_uniques)

# Nom, prénom et nuance de chaque élu dans chaque circonscription
library(dplyr)
library(tidyr)

legislatives_resultats <- legislatives |>
  select(-geometry) |>
  pivot_longer(cols = starts_with("Elu"), names_to = "Candidat", values_to = "Elu") |>
  pivot_longer(cols = starts_with("Nuance candidat"), names_to = "Nuance", values_to = "Parti") |>
  pivot_longer(cols = starts_with("Nom candidat"), names_to = "Nom_col", values_to = "Nom") |>
  pivot_longer(cols = starts_with("Prénom candidat"), names_to = "Prenom_col", values_to = "Prenom") |>
  filter(
    substr(Candidat, 5, 100) == substr(Nuance, 16, 100),
    substr(Candidat, 5, 100) == substr(Nom_col, 5, 100),
    substr(Candidat, 5, 100) == substr(Prenom_col, 8, 100)
  ) |>
  filter(Elu == "2lu") |>
  select(Circonscription, Prenom, Nom, Parti) |>
  distinct()

print(legislatives_resultats)

### Graphique demi-circulaire des résultats
library(tidyverse)
library(ggplot2)
library(ggforce)

# Exemple de données
resultats_assemblee <- data.frame(
  Parti = c("Nouveau Front Populaire", "Gauche", "Ensemble", "Centre", "Régionalistes", "Les Républicains", "Droite", "Rassemblement National", "Divers"),
  Sieges = c(182, 13, 168, 6, 4, 46, 14, 143, 1), # Remplace par les valeurs réelles
  Couleur = c("firebrick4", "firebrick1", "gold", "gold3", "lightgoldenrod4", "dodgerblue", "dodgerblue3", "navy", "gray47") # Couleurs pour chaque parti
)


# Calcul des positions pour le demi-cercle
resultats_assemblee <- resultats_assemblee |>
  mutate(
    start = pi * cumsum(lag(Sieges, default = 0)) / sum(Sieges) - pi / 2, # Rotation -90°
    end = pi * cumsum(Sieges) / sum(Sieges) - pi / 2 # Rotation -90°
  )

# Créer le graphique demi-circulaire
ggplot(resultats_assemblee) +
  geom_arc_bar(
    aes(
      x0 = 0, y0 = 0, r0 = 0.5, r = 1, # Position du demi-cercle
      start = start,
      end = end,
      fill = Parti
    ),
    color = "white"
  ) +
  scale_fill_manual(values = setNames(resultats_assemblee$Couleur, resultats_assemblee$Parti)) +
  coord_fixed() +
  theme_void() +
  labs(
    title = "Répartition des sièges à l’Assemblée nationale",
    caption = "Source : Le Monde (2024)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Centrer le titre
    plot.caption = element_text(hjust = 0.5) # Centrer la sous-légende
  )

### Essai avec des points
# Création d'un vecteur représentant tous les sièges (577 sièges)
seats <- rep(resultats_assemblee$Parti, resultats_assemblee$Sieges)

# Création de la data frame pour les points
seats_data <- data.frame(
  Seat = 1:577,
  Parti = seats,
  Couleur = rep(resultats_assemblee$Couleur, resultats_assemblee$Sieges)
)

# Calcul des coordonnées (position des points sur le demi-cercle horizontal)
seats_data <- seats_data %>%
  mutate(
    angle = pi * (Seat - 1) / 577, # Angle de 0 à pi pour un demi-cercle de gauche à droite
    x = cos(angle), # Coordonnée X
    y = sin(angle) # Coordonnée Y
  )

# Création du graphique avec 577 points représentant les sièges
ggplot(seats_data) +
  geom_point(
    aes(x = x, y = y, color = Couleur),
    size = 3, # Taille des points ajustée
    shape = 16
  ) +
  scale_color_identity() + # Utiliser les couleurs exactes de chaque parti
  coord_fixed() + # Fixer les proportions pour éviter la distorsion
  theme_void() +
  labs(
    title = "Répartition des sièges à l’Assemblée nationale",
    caption = "Source : Le Monde (2024)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  )





#---- app principale ------

library(shiny)

ui <- fluidPage(
  fluidRow(
    tabsetPanel(
      tabPanel(
        "Onglet un"
      ),
      tabPanel("Onglet deux"),
      tabPanel("Onglet trois"),
      tabPanel(
        "onglet quatre"
      ),
      tabPanel(
        "onglet cinq"
      )
    )
  ),
  fluidRow(
    column(
      6,
      "truc"
    ),
    column(
      6,
      "machin"
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # draw the histogram with the specified number of bins
    hist(x,
      breaks = bins, col = "lightblue", border = "white",
      xlab = "Temps d'attente avant le prochain jet (en minutes)",
      ylab = "Fréquence",
      main = "Histogramme du temps d'attente"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)



# ----------------
