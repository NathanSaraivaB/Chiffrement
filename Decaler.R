### Decaler ##

library(shiny)

texte <- "é"
decalage <- 2

decaler <- function(texte, decalage) {
  # Créer table correspondance
  table_corr <- c("A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6, "G" = 7, "H" = 8, "I" = 9, "J" = 10, 
                  "K" = 11, "L" = 12, "M" = 13, "N" = 14, "O" = 15, "P" = 16, "Q" = 17, "R" = 18, "S" = 19, 
                  "T" = 20, "U" = 21, "V" = 22, "W" = 23, "X" = 24, "Y" = 25, "Z" = 26, " " = 27, "?" = 28, 
                  "!" = 29, "." = 30, "," = 31) 
  
  # Séparer les caractères du texte
  caracteres_sep <- unlist(strsplit(texte, ""))
  
  caracteres_sep <- chartr("áàâäãåéèêëíìîïóòôöõúùûüýÿ", "aaaaaaeeeeiiiiooooouuuuyy", caracteres_sep)
  
  caracteres_sep <- toupper(caracteres_sep)
  
  # Convertir en chiffres avec gestion des espaces
  chiffres_modifies <- numeric(length(caracteres_sep))
  for (i in seq_along(caracteres_sep)) {
    if (caracteres_sep[i] == " ") {
      chiffres_modifies[i] <- 27  # 27 
    } else if (caracteres_sep[i] == "?") {
      chiffres_modifies[i] <- 28  # 28 
    } else if (caracteres_sep[i] == "!") {
      chiffres_modifies[i] <- 29  # 29 
    } else if (caracteres_sep[i] == ".") {
      chiffres_modifies[i] <- 30  # 30
    } else if (caracteres_sep[i] == "'") {
      chiffres_modifies[i] <- 27  # 27 considéré comme espace
    } else if (caracteres_sep[i] == "-") {
      chiffres_modifies[i] <- 27  # 27 considéré comme espace
    } else if (caracteres_sep[i] == ",") {
      chiffres_modifies[i] <- 31  # 31
    } else {
      chiffres_modifies[i] <- table_corr[[caracteres_sep[i]]]
    }
  }
  
  # Décalage modulo 26 sauf pour les espaces
  chiffres_modifies[chiffres_modifies <= 26] <- (chiffres_modifies[chiffres_modifies <= 26] + decalage) %% 26
  chiffres_modifies[chiffres_modifies == 0] <- 26
  
  # Repasser en lettres
  lettres_modifies <- names(table_corr)[chiffres_modifies]
  
  # Reformer le mot
  mot_reforme <- paste(lettres_modifies, collapse = "")
  
  return(mot_reforme)
}

#### Application ####

# Définition de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Décalage de texte"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("texte", "Entrez le texte à décaler :"),
      sliderInput("decalage", "Décalage :", min = -13, max = 13, value = 3)
    ),
    
    mainPanel(
      textOutput("resultat")
    )
  )
)

# Définition de la logique de l'application
server <- function(input, output) {
  output$resultat <- renderText({
    # Appliquer la fonction decaler avec les entrées utilisateur
    decaler(input$texte, input$decalage)
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)