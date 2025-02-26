# ----- PACKAGES -----

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(htmltools)
library(stringi)
library(DT)
library(shinyjs)





# ----- IMPORTS -----

## Circonscriptions, candidats et résultats 1er et 2nd tour (candidats 20 à 23)
legislatives <- read_rds("data/legislatives.rds")

## Circonscriptions non affichées sur la carte et résultats 1er tour
legislatives_empty <- read_rds("data/legislatives_empty.rds")

## Villes et coordonnées géographiques
villes <- read_rds("data/villes.rds")

## Circonscriptions non affichées sur la carte et résultats 1er et 2nd tour (candidats 20 à 23)
legislatives_empty2 <- read_rds("data/legislatives_empty2.rds")





# ----- ONGLET 1 : INFORMATIONS GENERALES -----

intro_onglet1 <- "
Les élections législatives approchent à grand pas et vous n’y connaissez rien en politique ou souhaitez approfondir votre savoir élémentaire sur leur fonctionnement ? 
Ce site est fait pour vous ! Que ce soit la première fois que vous allez voter, ou même si vous êtes un habitué des urnes, ici, on vous explique tout ! 
Vous trouverez une brève présentation concernant l’utilité des élections législatives et de l’Assemblée Nationale, leur utilité, leur fonctionnement, et leur enjeux à l’échelle du pays. 
On vous offre également toutes les ressources nécessaires pour voter pour la première fois (ou pas), afin que votre expérience pour ces élections se déroule en toute sérénité : où voter, quoi apporter, comment voter de l’étranger, etc. Bonne navigation sur notre site !
"

partie1_onglet1 <- "<br>
<p>Les élections législatives servent à élire les députés. Les députés siègent à l'Assemblée nationale. Ils sont élus au suffrage universel direct par les électeurs français inscrits sur les listes électorales. Le mode de scrutin est un scrutin majoritaire à 2 tours.</p>
<p>La durée du mandat des députés est de 5 ans (sauf dissolution de l'Assemblée nationale).
Le 9 juin 2024, le chef de l'Etat a annoncé la dissolution de l'Assemblée nationale.
Les dernières élections législatives ont eu lieu les 29 et 30 juin 2024 pour le 1er tour, et les 6 et 7 juillet 2024 pour le 2d tour.</p>
<p>Sauf en cas de dissolution, les élections législatives sont organisées :</p>
<p> - dans les 70 jours avant la fin du mandat de l'Assemblée nationale précédemment élue </p>
<p> - et le 7e dimanche qui suit la publication du décret convoquant les électeurs. Mais les élections sont organisées le samedi en Guadeloupe, en Guyane et en Martinique. </p>
<br>"

partie2_onglet1 <- "<br>
<p> Le suffrage universel est un principe fondamental de la République. À l’origine, lors de la Révolution française (1789 et 1792), le vote se faisait en plusieurs étapes, mais c’est finalement la IIe République qui, en 1848, instaure le suffrage universel masculin pour tous les hommes âgés d’au moins 21 ans. La IIIe République le rend définitif. </p>
<p> Cependant, tout au long du XIXe siècle, le droit de vote connaît des restrictions, souvent liées à des critères comme le niveau d’impôt payé (en 1814, 1830, 1849, 1852). En 1879, une large victoire des républicains marque un tournant en ancrant durablement le suffrage universel dans le régime politique.</p>
<p> Mais ce système reste longtemps imparfait : les femmes, par exemple, n’obtiennent le droit de vote qu’en 1944 (et peuvent voter aux municipales dès 1945). De plus, le mode de scrutin fait l’objet de nombreux débats : faut-il un ou deux tours ? Un vote par liste ou individuel ? Une élection à la majorité absolue ou à la proportionnelle ? Différents systèmes sont testés au fil du temps, souvent sous l’influence des partis politiques et de leurs stratégies électorales.</p>
<p>Les grands partis, mieux organisés, prennent l’avantage sur les petites formations politiques. La démocratie, en imposant des compromis et des alliances entre partis (comme les coalitions, unions ou cartels), modifie la manière dont les élections sont jouées et gagnées.</p>
<p>La Constitution mise en place par le général de Gaulle avait pour but de donner plus de pouvoir au président et au gouvernement, tout en réduisant l’influence du Parlement. Cependant, depuis le début de la Ve République, le président, élu directement par les citoyens, a parfois partagé son pouvoir avec le Premier ministre, notamment en période de cohabitation (lorsque le président et la majorité parlementaire sont de bords politiques opposés).</p>
<p> Un élément clé du système politique français est le fait qu'une majorité claire à l'Assemblée nationale renforce la stabilité du gouvernement. En 1958, Michel Debré, un des principaux architectes de la Constitution, espérait justement qu'une majorité solide puisse émerger après chaque élection.</p>
<p>Ironiquement, alors que la Ve République voulait initialement limiter l’influence des partis politiques, elle a finalement renforcé leur rôle au fil des années. Aujourd’hui, même si le président a seul le pouvoir de nommer le Premier ministre, ce choix dépend toujours du parti ou de la coalition majoritaire à l’Assemblée nationale. Ce fonctionnement ressemble à celui des régimes parlementaires, où le chef du gouvernement est issu du parti vainqueur des élections.</p>
<br>"

partie3_onglet1 <- "<br>
<p>Les députés sont élus au suffrage universel direct, au scrutin majoritaire à 2 tours et par circonscriptions.</p>
<ul style=\"list-style-type:square;\">
  <li><p>1er tour :</p>
<p>Pour être élu au 1er tour, un candidat doit obtenir :</p>
<p>- Plus de 50 % des suffrages exprimés</p>
<p>- Et un nombre de voix au moins égal à 25 % du nombre des électeurs inscrits.</p>
<p>Si aucun candidat n'est élu dès le 1er tour, un 2d tour est organisé une semaine plus tard.
</p></li>
<br>
  <li><p>2d tour :</p>
<p>Seuls certains candidats peuvent se présenter au 2d tour :</p>
<p>- Les 2 candidats qui sont arrivés en tête</p>
<p>- Les candidats suivants, à condition d'avoir obtenu un nombre de voix au moins égal à 12,5 % du nombre des électeurs inscrits.</p>
<p>Au 2d tour, le candidat qui obtient le plus grand nombre de voix est élu. En cas d'égalité, le plus âgé des candidats est élu.</li>
</ul>
<br>

<ul><li><p><B>Qui peut voter ?</B></p>
</p>Le scrutin législatif est ouvert à tous les électeurs des scrutins nationaux inscrits sur les listes électorales, c’est-à-dire à toute personne :</p>
<p>- âgée de 18 ans au moins à la date du premier tour</p>
<p>- n'étant pas déchue de ses droits civiques</p>
<p>- de nationalité française</p>
<p>- jouissant de ses droits civils et politiques</p>
<p>- inscrite sur une liste électorale.</p></li>
<br>
<p><B><li>Comment sont réparties les circonscriptions ?</B></p>
<p>Depuis 2012, la France est découpée en 577 circonscriptions législatives, chacune élisant un député à l'Assemblée nationale. Les circonscriptions sont principalement formées de cantons, qui regroupent plusieurs communes ou, parfois, des parties d’une grande commune. 
Cependant, certaines circonscriptions ne respectent pas le seuil moyen de 120 000 habitants par député, notamment dans les territoires d'outre-mer et certaines collectivités spécifiques comme les Terres australes et antarctiques françaises (TAAF), en raison de leur faible population.
Il y a en tout 577 circonscriptions réparties de la sorte :</p>
<p>- <B>France métropolitaine </B> : 539</p>
<p>- <B>Départements d’outre-mer (DOM) </B> : 19</p>
<p>- <B>Collectivités d’outre-mer (COM) </B> : 8</p>
<p>- <B>Français établis hors de France </B> : 11</p>
<p>Pour des raisons logistiques, les élections se déroulent en plusieurs étapes : d'abord pour les Français de l'étranger, puis pour les territoires d’outre-mer, et enfin en métropole.</p></li></ul>
<br>"

partie4_onglet1 <- "<br>
<p>Le pouvoir législatif exercé par l’Assemblée nationale joue un rôle essentiel dans l’équilibre politique du pays. Tantôt un soutien direct au gouvernement élu lors des élections présidentielles, tantôt un contre-pouvoir, il requiert plus que jamais une participation citoyenne active. 
Bien que l’Assemblée serve généralement à constituer une majorité pour le président ou la présidente fraîchement élu(e), elle peut également entraîner des périodes de cohabitation et permettre aux partis d’opposition d’exercer une influence sur la représentation politique durant les cinq années du mandat.</p>
<br>
<ol>
<li><p><B>Un rôle législatif fondamental</B></p>
Votre député(e) a pour mission de proposer, encadrer et examiner les lois. Il ou elle vous représente en votant pour ou contre les textes soumis à l’Assemblée Nationale.</p></li>

<li><p><B>Un relais de la voix citoyenne</B></p>
Élu(e) national, votre député(e) est un porte-parole qui peut interpeller directement les membres du gouvernement sur des sujets qui préoccupent les citoyens. Son rôle est de défendre les intérêts de l’ensemble de la population.</p></li>

<li><p><B>Un moyen d’exprimer une opposition au gouvernement</B></p>
Si le gouvernement propose des mesures controversées, l’Assemblée nationale peut, avec le soutien de plusieurs députés, voter une motion de censure pour s’y opposer et ainsi bloquer certaines décisions.</p></li>

<li><p><B>Un représentant au service des citoyens</B></p>
La fonction principale d’un député est de représenter et défendre les intérêts des citoyens. Afin de garantir son indépendance, il bénéficie d’une indemnité mensuelle de plus de 5 500 euros net. De plus, pour éviter tout conflit d'intérêt, un député ne peut cumuler son mandat avec certaines autres fonctions, comme celle de maire.</p></li></ol>
<br>"


auteurs_onglet1 <- "<p> <strong> Qui sommes nous ? </strong> </p>
<p> Ce site a été conçu dans le cadre d’un projet R-Shiny du Master Économétrie et Statistiques de l’IAE Nantes <em>(promotion 2026)</em>.
<p> Des questions, des remarques, des suggestions ? </p>
<p>N’hésitez pas à nous contacter !</p>
<ul>
    <li> <a href='mailto:8flocht8@gmail.com'>Florian Crochet</a> </li>
    <li> <a href='mailto:juliette.grison@gmail.com'>Juliette Grison</a> </li>
</ul>
"


# ----- ONGLET 2 : CARTE DES CIRCONSCRIPTIONS - Florian -----

# Couleurs par parti
couleurs <- c(
  "Extrême Gauche"                     = "#A50026",  # Rouge très foncé  
  "France Insoumise"                    = "#D1191B",  # Rouge foncé  
  "Union de la Gauche"                  = "#D73027",  # Rouge vif  
  "Parti Socialiste"                    = "#F46D43",  # Rouge orangé  
  "Écologistes"                         = "#1A9850",  # Vert soutenu  
  "Divers Gauche"                       = "#FDAE61",  # Orange clair  
  "Ensemble"                            = "#FFD700",  # Jaune doré  
  "Divers Centre"                        = "#FEC44F",  # Jaune orangé  
  "Union des Démocrates et Indépendants" = "#92C5DE",  # Bleu ciel  
  "Les Républicains"                     = "#4575B4",  # Bleu moyen  
  "Divers Droite"                        = "#74ADD1",  # Bleu clair  
  "Régionalistes"                        = "#7F3B08",  # Brun foncé  
  "Divers"                               = "#BEBEBE",  # Gris moyen  
  "Divers Souverainistes"                = "#D9D9D9",  # Gris clair  
  "Union de la Droite"                   = "#313695",  # Bleu foncé  
  "Horizons"                             = "#542788",  # Violet  
  "Rassemblement National"               = "#041E42",   # Bleu marine presque noir
  "Reconquête"                           = "#2D004B",  # Violet très foncé
  "Extrême Droite"                       = "#081D58"  # Bleu nuit profond  
)

# Ordre des partis de l'extrême gauche à l'extrême droite
ordre_partis <- c(
  "Extrême Gauche", "France Insoumise", "Union de la Gauche", "Parti Socialiste",
  "Écologistes", "Divers Gauche", "Ensemble", "Divers Centre", 
  "Union des Démocrates et Indépendants", "Les Républicains", "Divers Droite", 
  "Régionalistes", "Divers", "Divers Souverainistes", "Union de la Droite", 
  "Horizons", "Rassemblement National", "Reconquête", "Extrême Droite"
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





# ----- ONGLET 3 : PARTIS POLITIQUES -----

# Couleurs par parti
couleurs2 <- c(
  "Lutte Ouvrière"                        = "#A50026",  # Rouge très foncé (extrême gauche)  
  "Nouveau Front Populaire"               = "#D73027",  # Rouge vif (gauche)  
  "Ensemble"                               = "#FFD700",  # Jaune doré (centre)  
  "Union des Démocrates et Indépendants"   = "#92C5DE",  # Bleu ciel (centre-droit)  
  "Horizons"                               = "#542788",  # Violet (centre-droit)  
  "Debout la France"                       = "#74ADD1",  # Bleu clair (droite souverainiste)  
  "Rassemblement National"                 = "#041E42",  # Bleu marine (extrême droite)  
  "Reconquête"                             = "#2D004B"   # Violet très foncé (extrême droite radicale)  
)


# Ordre des partis de l'extrême gauche à l'extrême droite
ordre_partis2 <- c(
  "Lutte Ouvrière",            # Extrême gauche  
  "Nouveau Front Populaire",   # Gauche  
  "Ensemble",                  # Centre  
  "Union des Démocrates et Indépendants",  # Centre-droit  
  "Horizons",                  # Centre-droit  
  "Debout la France",          # Droite souverainiste  
  "Rassemblement National",    # Extrême droite  
  "Reconquête"                 # Extrême droite radicale  
)

tetes_de_liste <- c(
  "Nathalie Arthaud",      # Lutte Ouvrière
  "Marine Tondelier",        # Nouveau Front Populaire
  "Gabriel Attal",     # Ensemble (La République En Marche)
  "François Sauvadet",     # Union des Démocrates et Indépendants
  "Édouard Philippe",     # Horizons
  "Nicolas Dupont-Aignan", # Debout la France
  "Jordan Bardella",      # Rassemblement National
  "Éric Zemmour"          # Reconquête
)

# Synthèse du programme des partis
programmes_partis <- c(
  "Lutte Ouvrière" = HTML("<strong>Programme de Lutte Ouvrière :</strong> 
                          <ul>
                            <li>Nationalisation des grandes entreprises, des banques et des secteurs stratégiques comme l'énergie.</li>
                            <li>Augmentation significative du <em>SMIC</em> et des retraites.</li>
                            <li>Réduction de la durée du travail et protection renforcée des droits des salariés.</li>
                            <li>Opposition aux politiques néolibérales et soutien à un système économique sans exploitation.</li>
                            <li>Priorité donnée à l'égalité des sexes, les droits des travailleurs migrants et la solidarité internationale.</li>
                            <li>Transition énergétique dirigée par l'État.</li>
                          </ul>"),
  
  "Nouveau Front Populaire" = HTML("<strong>Programme du Nouveau Front Populaire :</strong> 
                                   <ul>
                                     <li>Instaurer une VIᵉ République pour démocratiser les institutions et renforcer le pouvoir des citoyens.</li>
                                     <li>Augmentation du <em>SMIC</em>, des retraites et réforme fiscale pour une taxation équitable.</li>
                                     <li>Renforcement des services publics, notamment l'éducation, la santé et le logement.</li>
                                     <li>Transition énergétique ambitieuse, visant la neutralité carbone d'ici 2050.</li>
                                     <li>Réforme de la police et de la justice pour garantir l'égalité devant la loi.</li>
                                   </ul>"),
  
  "Ensemble" = HTML("<strong>Programme de Ensemble :</strong> 
                    <ul>
                      <li>Réduction des déficits publics par une gestion rigoureuse des finances publiques.</li>
                      <li>Réforme du marché du travail pour le rendre plus flexible.</li>
                      <li>Investissement dans les technologies numériques et la formation professionnelle pour moderniser l'éducation.</li>
                      <li>Simplification administrative et fiscale pour encourager l'entrepreneuriat.</li>
                      <li>Transition énergétique, avec une politique de décarbonation des transports et de l'industrie.</li>
                      <li>Renforcement de la construction européenne, notamment sur les enjeux de sécurité et d'immigration.</li>
                    </ul>"),
  
  "Union des Démocrates et Indépendants" = HTML("<strong>Programme de l'Union des Démocrates et Indépendants :</strong> 
                                                 <ul>
                                                   <li>Réduction des impôts sur les entreprises et les particuliers, et réforme du système de santé et de retraite.</li>
                                                   <li>Décentralisation des pouvoirs pour plus d'autonomie des collectivités locales.</li>
                                                   <li>Renforcement des forces de l'ordre et mise en place de politiques de prévention de la délinquance.</li>
                                                   <li>Création d'une Europe plus compétitive, tout en préservant l'intégrité de la souveraineté nationale.</li>
                                                   <li>Transition écologique respectueuse des équilibres économiques, avec un soutien à l'innovation dans les industries vertes.</li>
                                                 </ul>"),
  
  "Horizons" = HTML("<strong>Programme de Horizons :</strong> 
                    <ul>
                      <li>Simplification fiscale et réduction de la dette publique pour garantir la compétitivité économique.</li>
                      <li>Réforme du système de retraite pour en assurer la pérennité.</li>
                      <li>Renforcement de l'éducation avec des investissements dans les technologies numériques.</li>
                      <li>Respect des valeurs républicaines et renforcement de la laïcité dans les institutions publiques.</li>
                      <li>Politique migratoire stricte, avec un contrôle renforcé des frontières et une intégration des migrants respectant les valeurs françaises.</li>
                      <li>Transition énergétique tout en préservant la compétitivité des entreprises françaises.</li>
                    </ul>"),
  
  "Debout la France" = HTML("<strong>Programme de Debout la France :</strong> 
                            <ul>
                              <li>Souverainisme et sortie de certaines institutions européennes pour redonner la souveraineté à la France.</li>
                              <li>Protectionnisme économique pour favoriser la production française.</li>
                              <li>Rétablissement du contrôle des frontières et politique d'immigration stricte.</li>
                              <li>Réduction des charges sociales et suppression des impôts sur la production pour soutenir les entreprises françaises.</li>
                              <li>Renforcement de la famille traditionnelle et mise en place de mesures de sécurité publique.</li>
                              <li>Refondation des relations avec l'Europe, mais en préservant la souveraineté nationale.</li>
                            </ul>"),
  
  "Rassemblement National" = HTML("<strong>Programme du Rassemblement National :</strong> 
                                  <ul>
                                    <li>Réduction drastique de l'immigration et renforcement du contrôle des frontières.</li>
                                    <li>Sortie de l'Union européenne et de la zone euro pour retrouver la souveraineté monétaire.</li>
                                    <li>Protectionnisme économique pour favoriser les entreprises françaises.</li>
                                    <li>Lutte contre l'islamisme et réduction des dépenses publiques, notamment dans la justice et l'immigration.</li>
                                    <li>Création de nouvelles mesures pour renforcer la sécurité dans les quartiers sensibles et les banlieues.</li>
                                  </ul>"),
  
  "Reconquête" = HTML("<strong>Programme de Reconquête :</strong> 
                       <ul>
                         <li>Révision de la politique migratoire avec une réduction drastique de l'immigration et une politique d'assimilation plus stricte.</li>
                         <li>Révision de l'enseignement pour restaurer les valeurs républicaines et laïques dans les écoles.</li>
                         <li>Protectionnisme économique et réduction des impôts pour favoriser les entreprises françaises.</li>
                         <li>Renforcement de la famille traditionnelle et de la sécurité avec des mesures fermes contre l'insécurité et l'islamisme.</li>
                         <li>Redonner la priorité à la France en matière de culture, d'économie et de politique étrangère.</li>
                       </ul>")
)

# Vecteur des textes personnalisés pour le bouton de téléchargement
telechargement <- c(
  "Lutte Ouvrière"                        = "Télécharger le programme",
  "Nouveau Front Populaire"               = "Télécharger le programme",
  "Ensemble"                               = "Télécharger le programme",  
  "Union des Démocrates et Indépendants"   = "Télécharger le programme d'un des candidats",
  "Horizons"                               = "Télécharger le programme d'un des candidats",
  "Debout la France"                       = "Télécharger le programme d'un des candidats",
  "Rassemblement National"                 = "Télécharger le programme",
  "Reconquête"                             = "Télécharger le programme"
)





# ----- ONGLET 4 : COMMENT VOTER ? -----

intro_onglet4 <- "<p> <strong> Que faire pour se préparer aux élections ? </strong> </p>
<p>
        Avant toute chose, il faut vous assurer d'être inscrits sur les listes électorales. 
        Le jour de vos 18 ans, vous êtes automatiquement inscrits mais en cas de doute, 
        vous pouvez vérifier votre situation électorale en utilisant le service en ligne du 
        <a href=\"https://www.service-public.fr/particuliers/actualites/A15421\" target=\"_blank\">
            Service Public
        </a>.
    </p>
<br> <p> Une fois que vous êtes inscrits et que vous connaissez la circonscription dans laquelle 
vous devez aller voter, référez-vous à votre situation le jour du vote.</p> <br> 
"

# Que faire pour se préparer aux élections
presence <- '<p>
    Rendez-vous à votre bureau de vote en vous fournissant de :
</p>
<ul>
    <li><strong>Votre pièce d’identité :</strong> carte nationale d’identité (CNI), passeport, etc.</li>
    <li><strong>Votre carte électorale :</strong> il est recommandé de se présenter muni de sa carte électorale mais elle est facultative. 
        Si vous ne l’avez pas, vous pourrez quand même voter à condition de présenter une pièce d’identité.
    </li>
</ul>

<p>
    <span style="color: #ffe436; font-size: 1.2em;">&#9432;</span> <strong>À savoir</strong><br>
    Il est possible de voter en présentant uniquement sa carte électorale dans les communes de moins de 1 000 habitants. 
    Vous devez néanmoins pouvoir prouver votre identité si le président du bureau de vote vous le demande.
</p>'


absence <- '<p>
    Pas de panique ! Si vous êtes absent ou si vous n’êtes pas en mesure de vous déplacer en bureau de vote pour les élections législatives, 
    vous pouvez donner procuration à toute personne de votre choix à condition qu’elle soit inscrite sur une liste électorale 
    (pas nécessairement dans la même commune que vous).
</p>

<p>
    Vous pouvez effectuer une procuration pour un seul tour ou pour les deux. Le jour du vote, la personne désignée devra 
    se rendre dans votre bureau de vote pour voter à votre place.
</p>

<p>
    <strong>Comment faire une demande de procuration ?</strong><br>
    Rendez-vous sur le téléservice 
    <a href="https://www.service-public.fr/particuliers/vosdroits/R58939" target="_blank" rel="noopener noreferrer" style="color: #0077cc; font-weight: bold;">
        Maprocuration
    </a>.
</p>'

# Comment fonctionne le vote en fonction de votre lieu d’habitation ?

vote_france <- '<p>
    Vous pouvez voter :
</p>

<ul>
    <li>Soit en allant en personne au bureau de vote avec un justificatif d’identité</li>
    <li>Soit par procuration</li>
</ul>

<p>
    <span style="color: #ffe436; font-size: 1.2em;">&#9432;</span> <strong>À savoir</strong><br>
    Le bureau de vote est ouvert de 8h à 18h (heures légales locales), sauf exception.
</p>'


vote_etranger <- '<p>
    Vous pouvez voter :
</p>

<ul>
    <li>Soit en vous rendant en personne au bureau de vote (au consulat ou ambassade) avec un justificatif d’identité</li>
    <li>Soit par procuration</li>
    <li>Soit par correspondance</li>
    <li>Soit par internet. Pour cela, il peut être nécessaire de mettre à jour vos coordonnées figurant dans le registre des Français de l’étranger.</li>
</ul>'




# ----- ONGLET 5 : RESULTATS - Juliette -----

# Texte introductif
intro_onglet5 <- "<p>Les élections législatives sont terminées et vous souhaitez connaître les résultats ? 
Vous êtes au bon endroit ! 
On vous présente la répartition des 577 sièges dans l'hémicycle de l'Assemblée Nationale avec le programme des principaux partis présents. 
Vous souhaitez également savoir qui a été élu proche de chez vous ? 
Pas de soucis ! 
Vous trouverez sur cette page une carte intéractive avec l’élu de votre circonscription.</p>"

library(ggplot2)
library(ggiraph)

# Fonction pour créer le graphique
create_assemblee_graph <- function() {
  # Création du DataFrame des résultats de l'Assemblée
  resultats_assemblee <- data.frame(
    Parti = c("Nouveau Front Populaire", "Gauche", "Ensemble", "Centre", "Régionalistes", 
              "Les Républicains", "Droite", "Rassemblement National", "Divers"),
    Sieges = c(182, 13, 168, 6, 4, 46, 14, 143, 1),  # Nombre de sièges par parti
    Couleur = c("firebrick4", "firebrick1", "gold", "gold3", "lightgoldenrod4", 
                "dodgerblue", "dodgerblue3", "navy", "gray47") # Couleurs associées
  )
  
  # Nombre total de sièges et de rangées
  nb_sieges <- sum(resultats_assemblee$Sieges) # 577
  nb_rangs <- 14  # Nombre de rangées dans le demi-cercle
  
  # Définition du nombre de sièges par rangée (moins en bas, plus en haut)
  sieges_par_rang <- round(seq(20, 60, length.out = nb_rangs)) 
  sieges_par_rang[length(sieges_par_rang)] <- sieges_par_rang[length(sieges_par_rang)] + 
    (577 - sum(sieges_par_rang)) # Ajustement final
  
  # Génération des coordonnées pour chaque siège
  df_sieges <- data.frame()
  index <- 1
  rayon_depart <- 8  # Rayon initial pour laisser un espace vide au centre
  
  for (rang in 1:nb_rangs) {
    nb_points <- sieges_par_rang[rang]  
    angles <- seq(pi, 0, length.out = nb_points)  # Symétrie parfaite autour de l'axe vertical
    rayon <- rayon_depart + rang * 1.5  # Les rangées s'éloignent progressivement du centre
    
    # Calcul des positions x, y
    temp_df <- data.frame(
      Siege = index:(index + nb_points - 1),
      x = rayon * cos(angles),
      y = rayon * sin(angles),
      Rang = rang
    )
    
    df_sieges <- rbind(df_sieges, temp_df)
    index <- index + nb_points
  }
  
  # Trier les points de gauche à droite
  df_sieges <- df_sieges[order(df_sieges$x), ]  
  
  # Attribuer les partis dans cet ordre
  df_sieges$Parti <- rep(resultats_assemblee$Parti, resultats_assemblee$Sieges)
  df_sieges <- df_sieges %>%
    left_join(resultats_assemblee, by = "Parti")  # Jointure pour obtenir les couleurs
  
  # Création de la colonne pour les tooltips interactifs (seulement le nom du parti, pas le numéro de siège)
  df_sieges$tooltip <- df_sieges$Parti
  
  # Création du graphique interactif
  gg <- ggplot(df_sieges, aes(x, y, color = Parti, tooltip = tooltip, data_id = Parti)) +
    geom_point_interactive(size = 3, shape = 16, alpha = 1) +  # Points interactifs
    scale_color_manual(values = setNames(resultats_assemblee$Couleur, resultats_assemblee$Parti)) +
    coord_fixed() +  # Conserver l'aspect circulaire
    theme_void() +  # Supprimer les axes
    ggtitle("Répartition des sièges à l'Assemblée Nationale") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Centrer et mettre en gras le titre
      plot.margin = margin(1, 1, 1, 1)  # Ajouter un peu de marge pour éviter que l'annotation touche le graphique
    ) +  
    annotate("text", x = 0, y = -3, label = "Source : Le Monde (2024)", size = 3, hjust = 0.5) +  # Réduire la taille de l'annotation
    labs(color = "Tendance politique")  # Changer le titre de la légende
  
  # Rendu interactif avec ggiraph, sans effet de sélection
  girafe(ggobj = gg, options = list(
    opts_hover(css = "opacity: 1;"),  # Garde la couleur normale au survol
    opts_hover_inv(css = "opacity: 0.2;"),  # Reste des points deviennent plus transparents
    opts_selection(type = "none")  # Désactive le changement de couleur au clic
  ))
  
}

# Carte et tableau concernant le candidat élu

legislatives$popup_content2 <- apply(legislatives, 1, function(row) {
  # En-tête du tableau
  lignes <- sprintf("<strong>%s</strong><br>
                    <table style='border-collapse: collapse; width: 100%%;'>
                    <tr style='background-color: #333; color: white;'>
                      <th>Prénom</th><th>Nom</th><th>Parti</th>
                    </tr>", row["Libellé"])
  
  # Filtrage des candidats élus
  candidats <- lapply(1:23, function(i) {
    nom <- row[[paste("Nom candidat", i)]]
    prenom <- row[[paste("Prénom candidat", i)]]
    nuance <- row[[paste("Nuance candidat", i)]]
    
    # Chercher le candidat élu (où "Elu i" contient "élu")
    if (grepl("élu", row[[paste("Elu", i)]], ignore.case = TRUE)) {
      if (!is.na(nom) && !is.na(prenom) && !is.na(nuance) && nom != "" && prenom != "" && nuance != "") {
        couleur <- if (nuance %in% names(couleurs)) couleurs[[nuance]] else "#FFFFFF"  # Assurer une couleur de fond même sans correspondance
        return(list(prenom = prenom, nom = nom, nuance = nuance, couleur = couleur))
      } else {
        return(NULL)  # Ignore les candidats incomplets
      }
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





#----- APP PRINCIPALE -----

library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(stringi)
library(dplyr)
library(tidyr)

ui <- fluidPage(useShinyjs(),
                theme = shinytheme("readable"),
                
                # Bande jaune en haut
                div(
                  class = "bande-jaune",
                  "Les législatives 2024 POUR LES NULS"
                ),
                
                # Onglets
                navbarPage("Les législatives 2024 pour les nuls",
                           
                           # Onglet Informations générales
                           tabPanel("Informations générales",
                                    div(class = "contenu-onglet",
                                        
                                        # Colonne principale avec le texte
                                        div(class = "contenu-principal",
                                            div(class = "titre-page", h2("Les législatives 2024 POUR LES NULS")),
                                            div(class = "texte-informations", HTML(intro_onglet1)),
                                            
                                            tags$div(
                                              img(src = "https://www.savigny-le-temple.fr/Statics/Actus/Actus_2024/06_Juin_2024/300624-elections-legislatives-resultats.jpg", 
                                                  class = "image-intro")
                                            ),
                                            
                                            # Les sections déroulantes restent ici
                                            div(class = "texte-informations",
                                                actionButton("toggle_presentation", "Présentation des élections législatives ⬇", class = "btn btn-primary btn-deroulant")
                                            ),
                                            
                                            hidden(
                                              div(id = "texte_presentation", class = "texte-informations", HTML(partie1_onglet1))
                                            ),
                                            
                                            div(class = "texte-informations",
                                                actionButton("toggle_histoire", "Histoire des élections de l’Assemblée Nationale ⬇", class = "btn btn-primary btn-deroulant")
                                            ),
                                            
                                            hidden(
                                              div(id = "texte_histoire", class = "texte-informations", HTML(partie2_onglet1))
                                            ),
                                            
                                            div(class = "texte-informations",
                                                actionButton("toggle_fonctionnement", "Fonctionnement des élections ⬇", class = "btn btn-primary btn-deroulant")
                                            ),
                                            
                                            hidden(
                                              div(id = "texte_fonctionnement", class = "texte-informations", HTML(partie3_onglet1))
                                            ),
                                            
                                            div(class = "texte-informations",
                                                actionButton("toggle_important", "Pourquoi est-ce important d'aller voter ? ⬇", class = "btn btn-primary btn-deroulant")
                                            ),
                                            
                                            hidden(
                                              div(id = "texte_important", class = "texte-informations", HTML(partie4_onglet1))
                                            )
                                        ),
                                        
                                        # Colonne de droite avec la boîte "Qui sommes-nous ?"
                                        div(class = "auteurs-box",
                                            HTML(auteurs_onglet1)
                                        )
                                    )
                           ),
                           
                           # Onglet Carte des circonscriptions
                           tabPanel("Carte des circonscriptions",
                                    h2("Candidats et circonscriptions : faites votre recherche"),
                                    selectizeInput("recherche", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
                                    actionButton("bouton", "Chercher"),
                                    leafletOutput("ma_carte", height = "500px"),
                                    h3("Vous résidez dans une collectivité d'outre-mer ou êtes un ressortissant français vivant à l'étranger ? Retrouvez votre circonscription ici."),
                                    selectizeInput("recherche2", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
                                    actionButton("bouton2", "Chercher"),
                                    h3(textOutput("titre_tableau")),
                                    dataTableOutput("mon_tableau")
                           ),
                           
                           # Onglet Partis politiques
                           tabPanel("Partis politiques",
                                    h2("Tout savoir sur les partis politiques"),
                                    fluidRow(
                                      # Génération des boutons avec deux colonnes par ligne
                                      lapply(names(couleurs2), function(ordre_partis2) {
                                        column(
                                          width = 6,  # Chaque bouton occupe la moitié d'une ligne (2 par ligne)
                                          tagList(
                                            actionButton(
                                              inputId = paste0("parti_", gsub(" ", "_", ordre_partis2)), 
                                              label = HTML(paste0(
                                                "<div style='display: flex; flex-direction: column; align-items: center;'>",
                                                "<img src='/", gsub(" ", "_", ordre_partis2), ".png' ",
                                                "style='width: 75%; height: 75%; object-fit: cover; display: block; margin-bottom: 10px; border-radius: 10px;'>",
                                                "<b>", ordre_partis2, "</b>",
                                                "</div>"
                                              )),  
                                              style = paste0("width: 100%; height: 594px; font-size: 30px; font-weight: bold; color: white; margin: 10px; background-color: ", 
                                                             couleurs2[ordre_partis2], 
                                                             "; border: 2px solid #ddd; border-radius: 10px; white-space: normal; text-align: center; display: flex; align-items: center; justify-content: center;")
                                            )
                                          )
                                        )
                                      })
                                    )
                           ),
                           
                           # Onglet Comment voter ?
                           tabPanel("Comment voter ?",
                                    div(class = "titre-page", h2("Comment voter ?")),
                                    
                                    div(class = "texte-informations", HTML(intro_onglet4),
                                    
                                    # Conteneur principal
                                    div(style = "width: 100%; display: flex; flex-direction: column; gap: 20px;",
                                        
                                        # Présence / absence
                                        div(style = "display: flex; flex-direction: column; align-items: flex-start;",
                                            
                                            # Encadrer le label avec une div stylisée
                                            div(style = "background-color: yellow; border: 2px solid black; padding: 10px; width: fit-content; margin-bottom: 15px; border-radius: 10px; box-shadow: 3px 3px 10px rgba(0,0,0,0.2);",
                                                "Les 29/30 juin et/ou les 6/7 juillet :"
                                            ),
                                            
                                            div(style = "display: flex; align-items: center; gap: 10px;",
                                                radioButtons("vote_option", label = NULL,  # SUPPRESSION DU LABEL
                                                             choices = c("Vous êtes présents dans votre circonscription" = "presence", 
                                                                         "Vous êtes absents de votre circonscription" = "absence"),
                                                             selected = character(0), inline = TRUE
                                                )
                                            )),
                                            
                                            # Textes associés
                                            uiOutput("presence_text"),
                                            uiOutput("absence_text")
                                        ),
                                        
                                        # France / Etranger
                                        div(style = "display: flex; flex-direction: column; align-items: flex-start;",
                                            
                                            # Encadrer le label avec une div stylisée
                                            div(style = "background-color: yellow; border: 2px solid black; padding: 10px; width: fit-content; margin-bottom: 15px; border-radius: 10px; box-shadow: 3px 3px 10px rgba(0,0,0,0.2);",
                                                "Comment fonctionne le vote en fonction de votre lieu d’habitation ?"
                                            ),
                                            
                                            div(style = "display: flex; align-items: center; gap: 10px;",
                                                radioButtons("vote_lieu", label = NULL,  # SUPPRESSION DU LABEL
                                                             choices = c("Vous êtes en France métropolitaine ou dans un département d’Outre Mer" = "vote_france", 
                                                                         "Vous êtes un.e Français.e résidant à l’étranger" = "vote_etranger"),
                                                             selected = character(0), inline = TRUE
                                                )
                                            ),
                                            
                                            # Textes associés
                                            uiOutput("vote_france_text", style = "margin-bottom: 30px;"),
                                            uiOutput("vote_etranger_text", style = "margin-bottom: 30px;")
                                        )
                                    )
                                    ),
                           
                           # Onglet Résultats
                           tabPanel("Résultats", 
                                    tabPanel("Résultats",
                                             # Titre de la page
                                             h2("Résultats des élections législatives de 2024"),
                                             
                                             # Texte explicatif avec la classe "texte-informations"
                                             div(class = "texte-informations", HTML(intro_onglet5)),
                                             
                                             # Graphique
                                             girafeOutput("assemblee_graph", height = "600px"),
                                             
                                             # Carte et tableau
                                             selectizeInput("recherche3", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
                                             actionButton("bouton3", "Chercher"),
                                             leafletOutput("ma_carte2", height = "500px"),
                                             
                                             h3("Vous résidez dans une collectivité d'outre-mer ou êtes un ressortissant français vivant à l'étranger ? Retrouvez votre circonscription ici."),
                                             
                                             selectizeInput("recherche4", "Rechercher une ville", choices = NULL, selected = "", multiple = FALSE),
                                             actionButton("bouton4", "Chercher"),
                                             
                                             h3(textOutput("titre_tableau2")),
                                             dataTableOutput("mon_tableau2")
                                    )
                                    ),
                
                # Ajout des styles CSS
                tags$head(
                  tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Fredoka+One&display=swap"),
                  tags$style(HTML("
                                  /* Bande jaune */
                                  .bande-jaune {
    background-color: yellow;
    color: black;
    font-family: 'Fredoka One', sans-serif;
    text-align: center;
    font-size: 24px;
    padding: 10px;
    border-bottom: 2px solid black;
    position: fixed; /* Fixe la barre en haut */
    top: 0;
    left: 0;
    width: 100%;
    z-index: 9999;
  }

  body {
    padding-top: 60px; /* Ajoute un espace pour éviter que la barre cache le contenu */
  }
  
      /* Mettre le titre des onglets en noir */
                                  .navbar-default {
      background-color: black !important;
      border-color: black !important;
                                  }
      
      /* Cacher le titre de la navbar */
  .navbar-default .navbar-brand {
    display: none !important;
  }
    
    .navbar-default .navbar-brand,
    .navbar-default .navbar-nav > li > a {
      color: white !important;
    }
  
  /* Espacement des boutons déroulants de l'onglet 1 */
  .texte-informations .btn-deroulant {
    margin-bottom: 15px; /* Ajuste la valeur selon tes préférences */
  }

  /* Qui sommes nous */
  .contenu-onglet {
    display: flex; /* Active la mise en page en colonnes */
    align-items: flex-start; /* Aligne le contenu en haut */
    justify-content: space-between; /* Sépare les éléments */
}

.contenu-principal {
    flex: 2; /* Prend 2/3 de l'espace */
    max-width: 85%; /* Limite la largeur du texte pour laisser de la place à la boîte */
}

.auteurs-box {
    flex: 1; /* Prend 1/3 de l'espace */
    max-width: 30%; /* Taille maximale de la boîte */
    background-color: yellow;
    padding: 15px;
    border: 2px solid black;
    border-radius: 10px;
    font-family: Arial, sans-serif;
    font-size: 14px;
    box-shadow: 3px 3px 10px rgba(0,0,0,0.2);

/* Rendre la boîte fixe pendant le scroll */
    position: sticky;
    top: 250px; /* La boîte reste visible à partir de 20px du haut */
    align-self: flex-start; /* Important pour bien s'aligner */


}
    
      .leaflet-control-zoom {
        position: absolute !important;
        bottom: 10px;
        left: 10px;
      }
      /* Changer la couleur de la police en blanc pour le tableau */
      .dataTable, .dataTable th, .dataTable td {
        color: white !important;
      }
      
      /* Centrer le titre et justifier le texte */
      .titre-page h2 {
    font-family: 'Fredoka One', sans-serif;
    font-size: 50px; /* Augmenter la taille */
    text-align: center; /* Centrer */
    margin-bottom: 20px; /* Ajouter un espace en dessous */
    }
      
      .texte-informations {
        text-align: justify;
        max-width: 90%;
        margin: auto;
      }
      
    .image-intro {
    display: block;
    margin: 20px auto; /* Centre horizontalement et ajoute un peu d'espace */
    width: 50%; /* Ajuste la taille à 80% de la largeur de la page */
    max-width: 1000px; /* Limite la taille max pour éviter une image trop grande */
    }
  
      /* Espacer les boutons déroulants */
      .titre-page .btn {
        margin-bottom: 10px; /* Ajoute un espace entre les boutons */
      }

      /* Modifier le style des boutons */
      .btn.btn-primary {
        background-color: yellow !important; /* Fond jaune */
        color: black !important; /* Texte noir */
        border: 1px solid black !important; /* Bordure noire */
        
      }
      
      /* Modifier le style des boutons au survol */
      .btn.btn-primary:hover {
        background-color: gold !important; /* Jaune plus foncé au survol */
      }

/* Onglet 4 mise en page*/
function updateText() {
      // Cacher tous les textes d'abord
      document.getElementById('presence_text').style.display = 'none';
      document.getElementById('absence_text').style.display = 'none';
      
      // Vérifier si le bouton 'présence' est sélectionné
      if (document.getElementById('presence').checked) {
        document.getElementById('presence_text').style.display = 'block';
      }
      // Vérifier si le bouton 'absence' est sélectionné
      else if (document.getElementById('absence').checked) {
        document.getElementById('absence_text').style.display = 'block';
      }
    }
    "))
                )
)
)

server <- function(input, output, session) {
  
  # Onglet 1
  
  # Action pour le bouton "Présentation des élections législatives"
  observeEvent(input$toggle_presentation, {
    toggle("texte_presentation")  # Affiche ou cache le contenu de "texte_presentation"
  })
  
  # Action pour le bouton "Histoire des élections de l’Assemblée Nationale"
  observeEvent(input$toggle_histoire, {
    toggle("texte_histoire")  # Affiche ou cache le contenu de "texte_histoire"
  })
  
  # Action pour le bouton "Fonctionnement des élections"
  observeEvent(input$toggle_fonctionnement, {
    toggle("texte_fonctionnement")  # Affiche ou cache le contenu de "texte_fonctionnement"
  })
  
  # Action pour le bouton "Importance des élections"
  observeEvent(input$toggle_important, {
    toggle("texte_important")  # Affiche ou cache le contenu de "texte_important"
  })
  
  
  
  # Onglet 2
  
  # Mise à jour de la liste des villes
  observe({
    updateSelectizeInput(session, "recherche", choices = villes$`Libellé commune`, selected = "", server = TRUE)
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
  observeEvent(input$bouton, {
    req(input$recherche)
    
    recherche <- tolower(stri_trans_general(input$recherche, "Latin-ASCII"))
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
    updateSelectizeInput(session, "recherche2", choices = legislatives_empty$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Transformation des données pour le tableau
  legislatives_long <- function(df) {
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
  observeEvent(input$bouton2, {
    req(input$recherche2)
    
    data_filtre <- legislatives_empty %>%
      filter(`Libellé commune` == input$recherche2)
    
    if (nrow(data_filtre) == 0) {
      showNotification("Aucune donnée trouvée pour cette circonscription.", type = "error")
      return()
    }
    
    table_data <- legislatives_long(data_filtre)
    
    output$titre_tableau <- renderText({ unique(data_filtre$`Libellé`) })
    
    # Ajout de la couleur de fond dans la table sans ajouter une colonne
    output$mon_tableau <- renderDataTable({
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

  
  # Onglet 3

  # Observer des événements des boutons pour afficher le programme et ajouter le bouton de téléchargement
  observe({
    lapply(ordre_partis2, function(parti) {
      
      observeEvent(input[[paste0("parti_", gsub(" ", "_", parti))]], {
        
        showModal(modalDialog(
          title = paste(parti),
          renderUI({
            tagList(
              # Appliquer du style CSS pour agrandir la fenêtre modale
              tags$head(
                tags$style(HTML("
                .modal-dialog {
                  width: 80%;  /* Agrandir la largeur de la fenêtre modale */
                  height: 80%; /* Agrandir la hauteur de la fenêtre modale */
                  max-width: none; /* Désactiver la largeur maximale par défaut */
                }
                .modal-body {
                  font-size: 20px;  /* Agrandir la taille de la police du contenu */
                }
                .modal-title {
                  font-size: 40px;  /* Agrandir la taille de la police du titre */
                }
              "))
              ),
              # Afficher le programme détaillé du parti
              HTML(programmes_partis[[parti]]),  # Utiliser HTML pour rendre les balises HTML
              # Ajouter le bouton de téléchargement pour le programme en PDF
              downloadButton(outputId = paste0("download_", gsub(" ", "_", parti)),
                             label = telechargement[parti],  # Utiliser le texte personnalisé du vecteur
                             class = "btn-primary")
            )
          }),
          easyClose = TRUE,
          footer = modalButton("Fermer")
        ))
      })
    })
  })
  
  # Fonction pour gérer le téléchargement des fichiers PDF associés aux partis
  lapply(ordre_partis2, function(parti) {
    output[[paste0("download_", gsub(" ", "_", parti))]] <- downloadHandler(
      filename = function() {
        paste0(gsub(" ", "_", parti), "_programme.pdf")  # Nom du fichier PDF
      },
      content = function(file) {
        # Chemin vers le fichier PDF dans le dossier 'data'
        file.copy(paste0("data/", gsub(" ", "_", parti), "_programme.pdf"), file)
      }
    )
  })
  
  # Onglet 4
  
  # Texte pour "Présent"
  output$presence_text <- renderUI({
    if (!is.null(input$vote_option) && input$vote_option == "presence") {
      return(HTML(presence))
    }
    return(NULL)
  })
  
  # Texte pour "Absent"
  output$absence_text <- renderUI({
    if (!is.null(input$vote_option) && input$vote_option == "absence") {
      return(HTML(absence))
    }
    return(NULL)
  })
  
  # Texte pour "Vote en France"
  output$vote_france_text <- renderUI({
    if (!is.null(input$vote_lieu) && input$vote_lieu == "vote_france") {
      return(HTML(vote_france))
    }
    return(NULL)
  })
  
  # Texte pour "Vote à l'étranger"
  output$vote_etranger_text <- renderUI({
    if (!is.null(input$vote_lieu) && input$vote_lieu == "vote_etranger") {
      return(HTML(vote_etranger))
    }
    return(NULL)
  })
  
  # Texte pour "Vote à l'étranger"
  output$vote_etranger_text <- renderUI({
    if (!is.null(input$vote_lieu) && input$vote_lieu == "vote_etranger") {
      return(HTML(vote_etranger))
    }
    return(NULL)
  })
  
  
  # Onglet 5
  
  # Rendre le graphique interactif
  output$assemblee_graph <- renderGirafe({
    create_assemblee_graph()  # Appeler la fonction pour créer le graphique
  })
  
  
  # Mise à jour de la liste des villes
  observe({
    updateSelectizeInput(session, "recherche3", choices = villes$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Fonction pour afficher la carte
  output$ma_carte2 <- renderLeaflet({
    leaflet(legislatives_sf) %>%
      addTiles() %>%
      addPolygons(
        popup = ~popup_content2,
        label = ~Libellé,
        highlight = highlightOptions(weight = 5, color = "#666", fillOpacity = 0.7)
      )
  })
  
  # Recherche et zoom sur la ville
  observeEvent(input$bouton3, {
    req(input$recherche3)
    
    recherche <- tolower(stri_trans_general(input$recherche3, "Latin-ASCII"))
    ville_trouvee <- villes %>%
      mutate(label_clean = tolower(stri_trans_general(`Libellé commune`, "Latin-ASCII"))) %>%
      filter(label_clean == recherche)
    
    if (nrow(ville_trouvee) > 0) {
      leafletProxy("ma_carte2") %>%
        setView(lng = ville_trouvee$Longitude[1], lat = ville_trouvee$Latitude[1], zoom = 12)
    } else {
      showNotification("Ville non trouvée.", type = "error")
    }
  })
  
  # Mise à jour de la liste des villes pour le second champ
  observe({
    updateSelectizeInput(session, "recherche4", choices = legislatives_empty2$`Libellé commune`, selected = "", server = TRUE)
  })
  
  # Transformation des données pour le tableau (garder seulement les candidats élus)
  legislatives_long2 <- function(df) {
    df_long <- df %>%
      pivot_longer(cols = starts_with("Prénom candidat"), names_to = "index", values_to = "Prénom") %>%
      mutate(
        index_num = as.integer(sub("Prénom candidat ", "", index)),
        Nom = sapply(index_num, function(i) df[[paste0("Nom candidat ", i)]]),
        Parti = sapply(index_num, function(i) df[[paste0("Nuance candidat ", i)]]),
        Elu = sapply(index_num, function(i) df[[paste0("Elu ", i)]])  # Vérification de la colonne "Elu i"
      ) %>%
      # Garder uniquement les candidats élus
      filter(!is.na(Prénom) & !is.na(Nom) & !is.na(Parti) & grepl("élu", Elu, ignore.case = TRUE)) %>%
      select(Prénom, Nom, Parti) %>%
      arrange(match(Parti, ordre_partis))  # Tri en fonction de l'ordre des partis
    
    return(df_long)
  }
  
  # Affichage du tableau des candidats
  observeEvent(input$bouton4, {
    req(input$recherche4)
    
    data_filtre2 <- legislatives_empty2 %>%
      filter(`Libellé commune` == input$recherche4)
    
    if (nrow(data_filtre2) == 0) {
      showNotification("Aucune donnée trouvée pour cette circonscription.", type = "error")
      return()
    }
    
    table_data <- legislatives_long2(data_filtre2)
    
    output$titre_tableau2 <- renderText({ unique(data_filtre2$`Libellé`) })
    
    # Ajout de la couleur de fond dans la table sans ajouter une colonne
    output$mon_tableau2 <- renderDataTable({
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
  
  