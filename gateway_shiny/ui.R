

fluidPage(theme = shinytheme("flatly"),
        titlePanel("Gateway Theory: Excerpt from the National Longitudinal Study 
                   of Adolescent to Adult Health (Add Health), 1994-2008"),
        hr(),
            
        
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
            ),
            
            mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                
                                tabPanel("General Information",
                                         h4("What is the Gateway Theory?"),
                                         p("The gateway theory proposes that early experimentation with tobacco or alcohol escalates to more addictive
                                           illicit substances later in adulthood. This theory has been studied consistently since the 1970s with 
                                           more or less confirmation depending on the source. Regardless of the level of support achieved by investigators, 
                                           societally the theory has been generally accepted and influenced much of the public health policy instituted 
                                           in the last several decades."),
                                         h4("Data source for this Shiny application"),
                                         p("This Shiny application excerpts from the National Longitudinal Study of Adolescent to Adult Health (Add Health). 
                                           The Add Health study is a national longitudinal survey of representative sample of students in grades 7-12 in the 1994 
                                           school year in the United States. This sample has been followed with three further in-home interviews in 1996 Wave II 
                                           (11-21 years), 2001-2002 Wave III (aged 18-26 years), and 2007-2008 Wave IV (aged 24-32 years). This excerpt used the 
                                           restricted Add Health datasets as downloaded from the ICPSR Institute of Social Research, hosted by the University of 
                                           Michigan."),
                                         h4("How to use this app"),
                                         p("The analysis shown within the two tabs above, labeled Marijuana and Cocaine, demonstrate the relationship
                                           over time of experimentation with tobacco cigarettes and marijuana or cocaine. The sidepanel shows which Wave of data 
                                           is being visualized. As you click through the waves you will see the overlay of new responses to having tried marijuana 
                                           or cocaine as compared to the individual's first response at their Baseline interview in 1995.")
                                         ),
                                
                                tabPanel("Marijuana", 
                                         br(),
                                         p("These graphs convey the correlation between individuals who have or have not \"tried\" 
                                           cigarette(s) across the waves of the study with those who have tried marijuana."),
                                         p("The top graph is only individuals who answered Yes to having tried cigarettes during 
                                           the time period before or between each wave of the study."),
                                         p("The bottom graph is individuals who answered No to having tried cigarettes during 
                                           the time period before or between each wave of the study."),
                                         em("Please note: In neither case do these results cover whether or not these individuals 
                                            became regular users of either cigarettes or marijuana. "),
                                         br(),
                                         
                                         plotlyOutput("countF"),
                                         br(),
                                         plotlyOutput("countM")),
                                
                                tabPanel("Cocaine",
                                         br(),
                                         p("These graphs convey the correlation between individuals who have or have not \"tried\" 
                                           cigarette(s) across the waves of the study with those who have tried cocaine."),
                                         p("The top graph is only individuals who answered Yes to having tried cigarettes during 
                                           the time period before or between each wave of the study."),
                                         p("The bottom graph is individuals who answered No to having tried cigarettes during 
                                           the time period before or between each wave of the study."),
                                         em("Please note: In neither case do these results cover whether or not these individuals 
                                            became regular users of either cigarettes or cocaine. "),
                                         br(),
                                         
                                         plotlyOutput("countCY"),
                                         br(),
                                         plotlyOutput('countCN'))
                    )
                )
        )
)
