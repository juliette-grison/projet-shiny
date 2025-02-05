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



# Créer un texte de popup formaté avec les noms et prénoms des 19 candidats
legislatives$popup_content <- apply(legislatives, 1, function(row) {
  # Initialiser avec le libelle (nom de la circonscription)
  popup_text <- row["libelle"]
  
  # Stocker les candidats dans un vecteur
  candidats <- c()
  
  for (i in 1:19) {
    nom_col <- paste("Nom candidat", i)
    prenom_col <- paste("Prénom candidat", i)
    
    # Ajouter le nom et prénom si les colonnes existent et ne sont pas NA
    if (!is.na(row[[nom_col]]) && !is.na(row[[prenom_col]])) {
      candidats <- c(candidats, sprintf("%s %s", row[[nom_col]], row[[prenom_col]]))
    }
  }
  
  # Coller les candidats avec des sauts de ligne
  paste(c(popup_text, candidats), collapse = "\n")
})



# Utiliser leaflet pour afficher les polygones et ajouter des popups avec les noms et prénoms
leaflet(legislatives$geometry) |> 
  addTiles() |>  
  addPolygons(
    popup = legislatives$popup_content  # Utiliser directement les données pour le popup
  )


# Onglet 3 :







# Onglet 4 :








# Onglet 5 : Juliette










#---- app principale ------

library(shiny)

ui<-fluidPage(
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
    column(6,
           "truc"
    ),
    column(6,
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




