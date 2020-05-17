

function(input, output) {

    marij <- reactive({
        req(input$wave)

            if(input$wave == "H1")
                marij <- add_data %>% filter(BIO_SEX==input$bio_sex, H1GI8_RACE==input$race) %>% 
                    group_by(H1TO30_TRYmj) %>% ggplot(aes(x = H1TO30_TRYmj)) +
                    labs(title = "Age first tried marijuana", subtitle = "") 
            else
                if(input$wave == "H2")
                    marij <- add_data %>% filter(BIO_SEX==input$bio_sex, H1GI8_RACE==input$race) %>% 
                        group_by(H2TO44) %>% ggplot(aes(x = H1TO30_TRYmj, fill = H2TO44)) +
                        labs(title = "Age first tried marijuana. Compared to Wave I: \nUsed marijuana since your last interview?\n\n", fill = "") 
                
            else
                if(input$wave == "H3")
                    marij <- add_data %>% filter(BIO_SEX==input$bio_sex, H1GI8_RACE==input$race) %>% 
                        group_by(H3TO108) %>% ggplot(aes(x = H1TO30_TRYmj, fill = H3TO108)) +
                        labs(title = "Age first tried marijuana. Compared to Wave I: \nUsed marijuana since June, 1995?\n\n", fill = "") 
            else 
                if(input$wave == "H4")
                    marij <- add_data %>% filter(BIO_SEX==input$bio_sex, H1GI8_RACE==input$race) %>% 
                        group_by(H4TO65B) %>% ggplot(aes(x = H1TO30_TRYmj, fill = H4TO65B)) +
                        labs(title = "Age first tried marijuana. Compared to Wave I: \nEver used marijuana?\n\n", fill = "") 
        
            return(marij)
    })


    # marijuana

    output$count <- renderPlotly({

        p <- marij()
            
        p <- p + 
            # ggtitle("Self-report: Age for trying marijuana") +
            xlab("Age") +
            ylab("Count") +
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