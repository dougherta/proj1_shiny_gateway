

function(input, output) {

    
    # Reactive expression that generates graphs comparing history of trying cigarettes to trying marijuana
    marijF <- reactive({
        req(input$wave)
        
        if(input$wave == "H1")
            marij <- add_data %>% filter(H1TO1_TRYcig=='Yes') %>% 
                ggplot(aes(x = H1TO30_TRYmj)) +
                labs(title = "Baseline: Yes, tried cigarettes and Age first tried marijuana") 
        else
            if(input$wave == "H2")
                marij <- add_data %>% filter(H2TO1=='Yes') %>% 
                    ggplot(aes(x = H1TO30_TRYmj, fill = H2TO44)) +
                    labs(title = "Tried cigarettes since last interview. Age first tried marijuana. Compared to Wave I: \n\tUsed marijuana since last interview?\n\n", fill = "") 
        else
            if(input$wave == "H3")
                marij <- add_data %>% filter(H3TO1=='Yes') %>% 
                    ggplot(aes(x = H1TO30_TRYmj, fill = H3TO108)) +
                    labs(title = "Have tried cigarettes. Age first tried marijuana. Compared to Wave I: \nUsed marijuana since June, 1995?\n\n", fill = "") 
        else 
            if(input$wave == "H4")
                marij <- add_data %>% filter(H4TO1=='Yes') %>% 
                    ggplot(aes(x = H1TO30_TRYmj, fill = H4TO65B)) +
                    labs(title = "Have tried cigarettes. Age first tried marijuana. Compared to Wave I: \nEver used marijuana?\n\n", fill = "") 
    })
    
    # Reactive expression that generates graphs comparing history of NOT trying cigarettes to trying marijuana
    marijM <- reactive({
        req(input$wave)
        
            if(input$wave == "H1")
                marij <- add_data %>% filter(H1TO1_TRYcig=="No (skip to Q.9)") %>% 
                    ggplot(aes(x = H1TO30_TRYmj)) +
                    labs(title = "Baseline: Never tried cigarettes and Age first tried marijuana", subtitle = "") 
            else
                if(input$wave == "H2")
                    marij <- add_data %>% filter(H2TO1=="No (skip to Q9)") %>% 
                        ggplot(aes(x = H1TO30_TRYmj, fill = H2TO44)) +
                        labs(title = "Haven't tried cigarette since last interview. Age first tried marijuana compared to Wave I: \n\tUsed marijuana since last interview?\n\n", fill = "") 
            else
                if(input$wave == "H3")
                    marij <- add_data %>% filter(H3TO1=="No (skip to Q.27)") %>% 
                        ggplot(aes(x = H1TO30_TRYmj, fill = H3TO108)) +
                        labs(title = "Never tried cigarettes and Age first tried marijuana compared to Wave I: \nUsed marijuana since June, 1995?\n\n", fill = "") 
            else 
                if(input$wave == "H4")
                    marij <- add_data %>% filter(H4TO1=='No') %>% 
                        ggplot(aes(x = H1TO30_TRYmj, fill = H4TO65B)) +
                        labs(title = "Never tried cigarettes and Age first tried marijuana compared to Wave I: \nEver used marijuana?\n\n", fill = "") 
    })


    # marijuana 
    output$countF <- renderPlotly({
        
        p <- marijF()
        
        p <- p + 
            xlab("Age") + ylab("Count") +
            geom_bar() + coord_flip() +
            scale_fill_brewer(palette = "Accent") + 
            theme_bw()
    })


    output$countM <- renderPlotly({

        p <- marijM()
            
        p <- p + 
            xlab("Age") + ylab("Count") +
            geom_bar() + coord_flip() +
            scale_fill_brewer(palette = "Dark2") + 
            theme_bw()
    })

########################################################################################################################
    # Reactive expression that generates graphs comparing history of trying cigarettes to trying cocaine
    cocaY <- reactive({
        req(input$wave)
        
        if(input$wave == "H1")
            add_data %>% filter(H1TO1_TRYcig=='Yes') %>% 
                ggplot(aes(x = H1TO34_TRYcoc)) +
                labs(title = "Baseline: Yes, tried cigarettes and Age first tried cocaine") 
        else
            if(input$wave == "H2")
                add_data %>% filter(H2TO1=='Yes') %>% 
                    ggplot(aes(x = H1TO34_TRYcoc, fill = H2TO50)) +
                    labs(title = "Tried cigarettes since last interview. Age first tried cocaine. Compared to Wave I: \n\tUsed cocaine since last interview?\n\n", fill = "") 
            else
                if(input$wave == "H3")
                    add_data %>% filter(H3TO1=='Yes') %>% 
                        ggplot(aes(x = H1TO34_TRYcoc, fill = H3TO111)) +
                        labs(title = "Have tried cigarettes. Age first tried cocaine. Compared to Wave I: \nUsed cocaine since June, 1995?\n\n", fill = "") 
                else 
                    if(input$wave == "H4")
                        add_data %>% filter(H4TO1=='Yes') %>% 
                            ggplot(aes(x = H1TO34_TRYcoc, fill = H4TO65C)) +
                            labs(title = "Have tried cigarettes. Age first tried cocaine. Compared to Wave I: \nEver used cocaine?\n\n", fill = "") 
    })
    
    # Reactive expression that generates graphs comparing history of NOT trying cigarettes to trying cocaine
    cocaN <- reactive({
        req(input$wave)
        
        if(input$wave == "H1")
            add_data %>% filter(H1TO1_TRYcig=="No (skip to Q.9)") %>% 
                ggplot(aes(x = H1TO34_TRYcoc)) +
                labs(title = "Baseline: Never tried cigarettes and Age first tried cocaine", subtitle = "") 
        else
            if(input$wave == "H2")
                add_data %>% filter(H2TO1=="No (skip to Q9)") %>% 
                    ggplot(aes(x = H1TO34_TRYcoc, fill = H2TO50)) +
                    labs(title = "Haven't tried cigarette since last interview. Age first tried cocaine compared to Wave I: \n\tUsed cocaine since last interview?\n\n", fill = "") 
            else
                if(input$wave == "H3")
                    add_data %>% filter(H3TO1=="No (skip to Q.27)") %>% 
                        ggplot(aes(x = H1TO34_TRYcoc, fill = H3TO111)) +
                        labs(title = "Never tried cigarettes and Age first tried cocaine compared to Wave I: \nUsed cocaine since June, 1995?\n\n", fill = "") 
                else 
                    if(input$wave == "H4")
                        add_data %>% filter(H4TO1=='No') %>% 
                            ggplot(aes(x = H1TO34_TRYcoc, fill = H4TO65C)) +
                            labs(title = "Never tried cigarettes and Age first tried cocaine compared to Wave I: \nEver used cocaine?\n\n", fill = "") 
    })
    
    
    # cocaine
    output$countCY <- renderPlotly({
        
        p <- cocaY()
        
        p <- p + 
            xlab("Age") + ylab("Count") +
            geom_bar() + coord_flip() +
            scale_fill_brewer(palette = "Accent") + 
            theme_bw()
    })
    
    
    output$countCN <- renderPlotly({
        
        p <- cocaN()
        
        p <- p + 
            xlab("Age") + ylab("Count") +
            geom_bar() + coord_flip() +
            scale_fill_brewer(palette = "Dark2") + 
            theme_bw()
    })    
}

