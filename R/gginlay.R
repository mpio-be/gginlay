#' gginlay
#' gginlay
#' @export
#' @examples
#' g = ggplot(data=iris,aes(y=Sepal.Width, x= Species)) +
#'     geom_boxplot()
#' versicolor = system.file("iris", "versicolor.jpg", package = "gginlay") |> jpeg::readJPEG(native = TRUE)
#' gginlay(g, versicolor)


gginlay <- function(g, p) {

    ui <- grid_page(

      layout = c(
        ".       topbar",
        "leftbar plotarea", 
        "results results"
      ),
      row_sizes = c(
        "100px",
        "500px", 
        "200px"
      ),
      col_sizes = c(
        "100px",
        "500px"
      ),

      gap_size = "0px", 

      grid_card(
        area = "topbar",
        card_body_fill(
         
        noUiSliderInput(
          inputId = "lr",
          label = NULL,
          width = "450px",
          margin = 0.02,
          min = 0,
          max = 1,
          value = c(0.2, 0.4),
          step = 0.01,
          color = "#035f94"
        ) 
        
        )
      ),

      grid_card_plot(
        area = "plotarea"
      ), 

      grid_card(
        area = "leftbar",
        card_body_fill(
          noUiSliderInput(
            inputId = "bt",
            label = NULL,
            height = "450px",
            orientation = "vertical",
            direction = "rtl",
            margin = 0.02,
            min = 0,
            max = 1,
            value = c(0.2, 0.4),
            step = 0.01,
            color = "#e9bc40"
          ) 
        ) 
        ), 

        grid_card_text(area = "results", textOutput("inlayInfo"))
   
    )


    server <- function(input, output, session) {

      observe( on.exit( assign('input', reactiveValuesToList(input) , envir = .GlobalEnv)) )
      

        output$plotarea <- renderPlot({
          g + theme(
            plot.margin = margin(t = 0,r = 0,b = 0,l = 0)) + 
          
          patchwork::inset_element(p,
            align_to = "plot", 
            left   = input$lr[1],
            right  = input$lr[2],
            bottom = input$bt[1],
            top    = input$bt[2]
          )
        }) |>
        bindEvent(list(input$lr, input$bt))
        




      output$inlayInfo <- renderText({

        paste(
          'patchwork::inset_element("your_inset",',
          input$lr[1],",", input$bt[1],",",input$lr[2],",",input$bt[2],
          ")"
        )


      }) |> bindEvent(list(input$lr, input$bt))
    
    }

    shinyApp(ui = ui, server = server)





}