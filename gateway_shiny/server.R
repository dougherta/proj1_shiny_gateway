

function(input, output) {

    marijF <- reactive({
        req(input$wave)
        
        if(input$wave == "H1")
            marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Female') %>% 
                ggplot(aes(x = H1TO30_TRYmj)) +
                labs(title = "Female, Wave I Baseline: Age first tried marijuana", subtitle = "") 
        else
            if(input$wave == "H2")
                marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Female') %>% 
                    ggplot(aes(x = H1TO30_TRYmj, fill = H2TO44)) +
                    labs(title = "Female, Age first tried marijuana. Compared to Wave I: \n\tUsed marijuana since last interview?\n\n", fill = "") 
            else
                if(input$wave == "H3")
                    marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Female') %>% 
                        ggplot(aes(x = H1TO30_TRYmj, fill = H3TO108)) +
                        labs(title = "Female, Age first tried marijuana. Compared to Wave I: \nUsed marijuana since June, 1995?\n\n", fill = "") 
                else 
                    if(input$wave == "H4")
                        marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Female') %>% 
                            ggplot(aes(x = H1TO30_TRYmj, fill = H4TO65B)) +
                            labs(title = "Female, Age first tried marijuana. Compared to Wave I: \nEver used marijuana?\n\n", fill = "") 
                    
    })
    
    
    marijM <- reactive({
        req(input$wave)
        
            if(input$wave == "H1")
                marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Male') %>% 
                    ggplot(aes(x = H1TO30_TRYmj)) +
                    labs(title = "Male, Wave I Baseline: Age first tried marijuana", subtitle = "") 
            else
                if(input$wave == "H2")
                    marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Male') %>% 
                        ggplot(aes(x = H1TO30_TRYmj, fill = H2TO44)) +
                        labs(title = "Male, Age first tried marijuana. Compared to Wave I: \n\tUsed marijuana since last interview?\n\n", fill = "") 
            else
                if(input$wave == "H3")
                    marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Male') %>% 
                        ggplot(aes(x = H1TO30_TRYmj, fill = H3TO108)) +
                        labs(title = "Male, Age first tried marijuana. Compared to Wave I: \nUsed marijuana since June, 1995?\n\n", fill = "") 
            else 
                if(input$wave == "H4")
                    marij <- add_data %>% filter(H1GI8_RACE==input$race, BIO_SEX=='Male') %>% 
                        ggplot(aes(x = H1TO30_TRYmj, fill = H4TO65B)) +
                        labs(title = "Male, Age first tried marijuana. Compared to Wave I: \nEver used marijuana?\n\n", fill = "") 
        
    })


    # marijuana

    output$countF <- renderPlotly({
        
        p <- marijF()
        
        p <- p + 
            # ggtitle("Self-report: Age for trying marijuana") +
            xlab("Age") + ylab("Count") +
            geom_bar() + coord_flip() +
            scale_fill_brewer(palette = "Accent") + 
            theme_bw()
        
        
    })
    
    
        output$countM <- renderPlotly({

        p <- marijM()
            
        p <- p + 
            # ggtitle("Self-report: Age for trying marijuana") +
            xlab("Age") + ylab("Count") +
            geom_bar() + coord_flip() +
            scale_fill_brewer(palette = "Dark2") + 
            theme_bw()
        

    })
}


# function(input, output) {
# 
#     output$count <- renderPlotly(
# 
#         add_data %>%
#             filter(BIO_SEX == input$bio_sex & H1GI8_RACE == input$race) %>%
# 
#             ggplot(aes(x = H1TO30_TRYmj)) +
#             ggtitle("Self-report: Age for trying marijuana") +
#             xlab("Age") +
#             ylab("Count") +
# 
#             geom_bar(fill="steelblue") + coord_flip() +
#             theme_bw()
# 
#     )
# }

# {if (input$bio_sex==T) dplyr::group_by(., BIO_SEX) else .} %>% 
# {if (input$bio_sex==F) dplyr::group_by(.) else .} %>% 
    