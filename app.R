# ----- PACKAGES -----

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(stringi)
library(DT)





# ----- IMPORTS -----

## Circonscriptions et candidats et résultats
legislatives <- read_rds("data/legislatives.rds")

## Circonscriptions non affichées sur la carte
legislatives_empty <- read_rds("data/legislatives_empty.rds")

## Villes et coordonnées géographiques
villes <- read_rds("data/villes.rds")





# ----- ONGLET 1 : INFORMATIONS GENERALES -----










# ----- ONGLET 2 : CARTE DES CIRCONSCRIPTIONS - Florian -----

# Couleurs par parti
couleurs <- c(
  "Extrême Gauche"          = "#A50026",  # Rouge très foncé
  "France Insoumise"        = "#D1191B",  # Rouge foncé
  "Union de la Gauche"      = "#D73027",  # Rouge vif
  "Parti Socialiste"        = "#F46D43",  # Rouge orangé
  "Écologistes"             = "#1A9850",  # Vert soutenu
  "Divers Gauche"           = "#FDAE61",  # Orange clair
  "Ensemble"                = "#FFD700",  # Jaune doré
  "Divers Centre"           = "#FEC44F",  # Jaune orangé
  "Union des Démocrates et Indépendants" = "#92C5DE",  # Bleu ciel
  "Les Républicains"        = "#4575B4",  # Bleu moyen
  "Divers Droite"           = "#74ADD1",  # Bleu clair
  "Régionalistes"           = "#7F3B08",  # Brun foncé
  "Divers"                  = "#BEBEBE",  # Gris moyen
  "Divers Souverainistes"   = "#D9D9D9",  # Gris clair
  "Union de la Droite"      = "#313695",  # Bleu foncé
  "Horizons"                = "#542788",  # Violet
  "Reconquête"              = "#2D004B",  # Violet très foncé
  "Extrême Droite"          = "#081D58",  # Bleu nuit profond
  "Rassemblement National"  = "#041E42"   # Bleu marine presque noir
)

# Ordre des partis de l'extrême gauche à l'extrême droite
ordre_partis <- c(
  "Extrême Gauche", "France Insoumise", "Union de la Gauche", "Parti Socialiste", "Écologistes",
  "Divers Gauche", "Ensemble", "Divers Centre", "Union des Démocrates et Indépendants",
  "Les Républicains", "Divers Droite", "Régionalistes", "Divers",
  "Divers Souverainistes", "Union de la Droite", "Horizons", "Reconquête",
  "Extrême Droite", "Rassemblement National"
)


# Génération du contenu des popups
legislatives$popup_content <- apply(legislatives, 1, function(row) {
  # En-tête du tableau
  lignes <- sprintf("<strong>%s</strong><br>
                    <table style='border-collapse: collapse; width: 100%%;'>
                    <tr style='background-color: #333; color: white;'>
                      <th>Prénom</th><th>Nom</th><th>Parti</th>
                    </tr>", row["Libellé"])
  
  # Filtrage des candidats valides
  candidats <- lapply(1:19, function(i) {
    nom <- row[[paste("Nom candidat", i)]]
    prenom <- row[[paste("Prénom candidat", i)]]
    nuance <- row[[paste("Nuance candidat", i)]]
    
    if (!is.na(nom) && !is.na(prenom) && !is.na(nuance) && nom != "" && prenom != "" && nuance != "") {
      couleur <- if (nuance %in% names(couleurs)) couleurs[[nuance]] else "#FFFFFF"  # Assurer une couleur de fond même sans correspondance
      return(list(prenom = prenom, nom = nom, nuance = nuance, couleur = couleur))
    } else {
      return(NULL)  # Ignore les candidats incomplets
    }
  })
  
  # Suppression des NULL (candidats invalides)
  candidats <- Filter(Negate(is.null), candidats)
  
  # Tri des candidats selon l'ordre des partis
  if (length(candidats) > 0) {
    candidats_sorted <- candidats[order(match(sapply(candidats, function(x) x$nuance), ordre_partis))]
    
    # Génération des lignes du tableau avec couleurs
    lignes_candidats <- sapply(candidats_sorted, function(candidat) {
      sprintf("<tr style='background-color: %s; color: white;'>
                <td>%s</td><td>%s</td><td>%s</td></tr>", 
              candidat$couleur, candidat$prenom, candidat$nom, candidat$nuance)
    })
    
    # Fusionner les lignes des candidats dans le tableau
    popup_text <- paste0(lignes, paste(lignes_candidats, collapse = ""), "</table>")
  } else {
    popup_text <- paste0(lignes, "<tr><td colspan='3' style='text-align:center;'>Aucun candidat</td></tr></table>")
  }
  
  HTML(popup_text)
})


# Conversion des données géospatiales en format sf compatible avec leaflet
legislatives_sf <- st_as_sf(legislatives)

# Création de la carte avec leaflet
leaflet(legislatives_sf) |>
  addTiles() |>
  addPolygons(
    popup = legislatives_sf$popup_content,
    label = ~Libellé,  # Affiche le nom de la circonscription au survol
    labelOptions = labelOptions(
      direction = "auto",  # La direction du label
      style = list(
        "font-weight" = "bold",
        "color" = "black",
        "background" = "white",
        "padding" = "5px"
      )
    ),
    highlight = highlightOptions(
      weight = 5,
      color = "#666",  # Color of the border when highlighted
      fillOpacity = 0.7,  # Opacity of the fill on hover
      bringToFront = TRUE  # Bring the polygon to front when hovered
    )
  )

## Autres circonscriptions

legislatives_empty <- legislatives_empty %>%
  rowwise() %>%
  mutate(
    candidats_info = list(
      tibble(
        Prénom = c(`Prénom candidat 1`, `Prénom candidat 2`, `Prénom candidat 3`, 
                   `Prénom candidat 4`, `Prénom candidat 5`, `Prénom candidat 6`, 
                   `Prénom candidat 7`, `Prénom candidat 8`, `Prénom candidat 9`, 
                   `Prénom candidat 10`, `Prénom candidat 11`, `Prénom candidat 12`, 
                   `Prénom candidat 13`, `Prénom candidat 14`, `Prénom candidat 15`, 
                   `Prénom candidat 16`, `Prénom candidat 17`, `Prénom candidat 18`, 
                   `Prénom candidat 19`),
        Nom = c(`Nom candidat 1`, `Nom candidat 2`, `Nom candidat 3`, 
                `Nom candidat 4`, `Nom candidat 5`, `Nom candidat 6`, 
                `Nom candidat 7`, `Nom candidat 8`, `Nom candidat 9`, 
                `Nom candidat 10`, `Nom candidat 11`, `Nom candidat 12`, 
                `Nom candidat 13`, `Nom candidat 14`, `Nom candidat 15`, 
                `Nom candidat 16`, `Nom candidat 17`, `Nom candidat 18`, 
                `Nom candidat 19`),
        Parti = c(`Nuance candidat 1`, `Nuance candidat 2`, `Nuance candidat 3`, 
                  `Nuance candidat 4`, `Nuance candidat 5`, `Nuance candidat 6`, 
                  `Nuance candidat 7`, `Nuance candidat 8`, `Nuance candidat 9`, 
                  `Nuance candidat 10`, `Nuance candidat 11`, `Nuance candidat 12`, 
                  `Nuance candidat 13`, `Nuance candidat 14`, `Nuance candidat 15`, 
                  `Nuance candidat 16`, `Nuance candidat 17`, `Nuance candidat 18`, 
                  `Nuance candidat 19`)
      )
    )
  ) %>%
  ungroup()




# ----- ONGLET 3 : PARTIS POLITIQUES -----










# ----- ONGLET 4 : COMMENT VOTER ? -----










# ----- ONGLET 5 : RESULTATS - Juliette -----

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)

# Extraction des partis uniques
partis_uniques <- unique(unlist(legislatives[, grep("^Nuance candidat", names(legislatives))]))
print(partis_uniques)

# Nom, prénom et nuance de chaque élu dans chaque circonscription
legislatives_resultats <- legislatives |>
  select(-Géométrie) |>
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
  select(`Libellé circonscription législative`, Prenom, Nom, Parti) |>
  distinct()

print(legislatives_resultats)

# Exemple de données pour le graphique
resultats_assemblee <- data.frame(
  Parti = c("Nouveau Front Populaire", "Gauche", "Ensemble", "Centre", "Régionalistes", "Les Républicains", "Droite", "Rassemblement National", "Divers"),
  Sieges = c(182, 13, 168, 6, 4, 46, 14, 143, 1), # Remplacez par les valeurs réelles
  Couleur = c("firebrick4", "firebrick1", "gold", "gold3", "lightgoldenrod4", "dodgerblue", "dodgerblue3", "navy", "gray47") # Couleurs pour chaque parti
)

# Calcul des positions pour le demi-cercle
resultats_assemblee <- resultats_assemblee |>
  mutate(
    start = pi * cumsum(lag(Sieges, default = 0)) / sum(Sieges) - pi / 2, # Rotation -90°
    end = pi * cumsum(Sieges) / sum(Sieges) - pi / 2 # Rotation -90°
  )

# Création du graphique demi-circulaire
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

# Essai avec des points
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





#----- APP PRINCIPALE -----

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(stringi)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  theme = shinytheme("readable"),
  fluidRow(
    tabsetPanel(
      tabPanel("Informations générales"),
      tabPanel("Carte des circonscriptions",
               selectizeInput("search_input", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
               actionButton("search_btn", "Chercher"),
               leafletOutput("ma_carte", height = "500px"),
               selectizeInput("search_input2", "Rechercher une autre ville", choices = NULL, selected = "", multiple = FALSE),
               actionButton("show_missing_btn", "Chercher"),
               h3(textOutput("table_title")),
               dataTableOutput("circonscriptions_table")
      ),
      tabPanel("Partis politiques"),
      tabPanel("Comment voter ?"),
      tabPanel("Résultats")
    )
  ),
  tags$head(
    tags$style(HTML("
    .leaflet-control-zoom {
      position: absolute !important;
      bottom: 10px;
      left: 10px;
    }
    /* Changer la couleur de la police en blanc pour le tableau */
    .dataTable, .dataTable th, .dataTable td {
      color: white !important;
    }
  "))
  )
)


server <- function(input, output, session) {
  
  # Mise à jour de la liste des villes
  observe({
    updateSelectizeInput(session, "search_input", choices = villes$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Fonction pour afficher la carte
  output$ma_carte <- renderLeaflet({
    leaflet(legislatives_sf) %>%
      addTiles() %>%
      addPolygons(
        popup = ~popup_content,
        label = ~Libellé,
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Recherche et zoom sur la ville
  observeEvent(input$search_btn, {
    req(input$search_input)
    
    recherche <- tolower(stri_trans_general(input$search_input, "Latin-ASCII"))
    ville_trouvee <- villes %>%
      mutate(label_clean = tolower(stri_trans_general(`Libellé commune`, "Latin-ASCII"))) %>%
      filter(label_clean == recherche)
    
    if (nrow(ville_trouvee) > 0) {
      leafletProxy("ma_carte") %>%
        setView(lng = ville_trouvee$Longitude[1], lat = ville_trouvee$Latitude[1], zoom = 12)
    } else {
      showNotification("Ville non trouvée.", type = "error")
    }
  })
  
  # Mise à jour de la liste des villes pour le second champ
  observe({
    updateSelectizeInput(session, "search_input2", choices = legislatives_empty$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Transformation des données pour le tableau
  transform_legislatives <- function(df) {
    df_long <- df %>%
      pivot_longer(cols = starts_with("Prénom candidat"), names_to = "index", values_to = "Prénom") %>%
      mutate(
        index_num = as.integer(sub("Prénom candidat ", "", index)),
        Nom = sapply(index_num, function(i) df[[paste0("Nom candidat ", i)]]),
        Parti = sapply(index_num, function(i) df[[paste0("Nuance candidat ", i)]])
      ) %>%
      select(Prénom, Nom, Parti) %>%
      filter(!is.na(Prénom) & !is.na(Nom) & !is.na(Parti)) %>%
      arrange(match(Parti, ordre_partis))  # Tri en fonction de l'ordre des partis
    
    return(df_long)
  }
  
  # Affichage du tableau des candidats
  observeEvent(input$show_missing_btn, {
    req(input$search_input2)
    
    data_filtered <- legislatives_empty %>%
      filter(`Libellé commune` == input$search_input2)
    
    if (nrow(data_filtered) == 0) {
      showNotification("Aucune donnée trouvée pour cette circonscription.", type = "error")
      return()
    }
    
    table_data <- transform_legislatives(data_filtered)
    
    output$table_title <- renderText({ unique(data_filtered$`Libellé`) })
    
    # Ajout de la couleur de fond dans la table sans ajouter une colonne
    output$circonscriptions_table <- renderDataTable({
      datatable(table_data, options = list(pageLength = 10), 
                escape = FALSE,  # Permet d'afficher du HTML
                rownames = FALSE) %>%
        formatStyle(
          columns = c("Prénom", "Nom", "Parti"),
          target = "row",
          backgroundColor = styleEqual(names(couleurs), couleurs)  # Applique la couleur de fond selon le parti
        )
    })
  })
}

shinyApp(ui = ui, server = server)
