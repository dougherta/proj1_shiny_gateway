

fluidPage(theme = shinytheme("flatly"),
        titlePanel("Gateways: Case Studies"),
            
        
        sidebarLayout(
        
            sidebarPanel(
                img(src="Citadel 2011-12-28 067.JPG", width = "75%"),

                radioButtons(inputId = "wave",
                             label = "Choose study wave I - IV",
                             choices = list("Wave I: 1994-95" = "H1",
                                            "Wave II: 1996" = "H2",
                                            "Wave III: 2001-02" = "H3",
                                            "Wave IV: 2007-09" = "H4"),
                             selected = "H1"),

                selectizeInput(inputId = "bio_sex",
                               label = "Sex",
                               choices = levels(add_data$BIO_SEX)),
                
                selectizeInput(inputId = "race",
                               label = "Race",
                               choices = levels(add_data$H1GI8_RACE),
                               selected = ""),
                
                selectizeInput(inputId = "yob",
                               label = "Year of birth",
                               choices = sort(unique(add_data$H1GI1Y)),
                               selected = "")
                
                
            ),
            
            mainPanel(plotlyOutput("count"))
        )
)
