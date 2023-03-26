#' gginlay
#' Add Inlays on a ggplot Interactively.
#' @param g a ggplot object
#' @param p an inlay object supported by [patchwork::inset_element()]
#' @export
#' @examples
#' if(interactive()) {
#' g = ggplot(data=iris,aes(y=Sepal.Width, x= Species)) +
#'     geom_boxplot()
#' 
#' versicolor = system.file("iris", "versicolor.jpg", package = "gginlay") |> jpeg::readJPEG(native = TRUE)
#' setosa = system.file("iris", "setosa.png", package = "gginlay") |> png::readPNG(native = TRUE)
#' 
#' gginlay(g, versicolor)
#' gginlay(g, setosa)
#' 
#' img = matrix(0:1, ncol = 3, nrow = 2) |> as.raster()
#' gginlay(g, img)
#' 
#' g2 = ggplot(cars, aes(y = speed, x = dist)) + geom_smooth()
#' gginlay(g, g2)
#' 
#' g = ggplot(data=iris,aes(y=Sepal.Width, x= Petal.Length )) +
#'     facet_wrap(~Species) +  
#'     geom_point()
#' 
#' gginlay(g, versicolor)

#' }



gginlay <- function(g, p) {

    fnargs = as.list(match.call(expand.dots = FALSE))

    if(missing(p)) p_missing = TRUE

    ui <- grid_page(
      theme = bs_theme(version = 5, bootswatch = "journal"), 
      layout = c(
        ".       topbar    controls",
        "leftbar plotarea  controls", 
        "results results   controls"
      ),
      row_sizes = c(
        "100px",
        "500px", 
        "200px"
      ),
      col_sizes = c(
        "100px",
        "450px",
        "150px"
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
          value = c(0.25,0.75),
          step = 0.001,
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
            value = c(0.25,0.75),
            step = 0.001,
            color = "#24828f"
          ) 
        ) 
        ), 

      grid_card_text(area = "results", uiOutput("inlayInfo")), 

      grid_card(
        area = "controls",

        card_header("<i>inset_element()</i><br>arguments:" |> HTML()),
        card_body_fill(
          radioButtons(
            inputId = "align_to",
            label = "align_to:",
            choices = c("panel", "plot", "full")
          ), 

        checkboxInput(
          inputId = "on_top",
          label = "on_top:",
          value = TRUE
        ), 
        
        checkboxInput(
          inputId = "ignore_tag",
          label = "ignore_tag:",
          value = TRUE
        )


        ),
        
        card_header("Inlay object:" |> HTML()),
        card_body_fill(

          "Show outline:",
          switchInput(
            inputId = "show_inlay"
          )

        )
      
      )

   
    )


    server <- function(input, output, session) {

      observe( on.exit( assign('input', reactiveValuesToList(input) , envir = .GlobalEnv)) )
      

        output$plotarea <- renderPlot({

          if(input$show_inlay) # TODO only for rasters
            p1 =  emptyGG(p) else p1 = p
 
          g = g + theme(plot.margin = margin(t = 0,r = 0,b = 0,l = 0)) 

          if (!input$on_top) g = g + theme(panel.background = element_blank())

          g = g + patchwork::inset_element(p1,
              align_to = input$align_to,
              on_top   = input$on_top,
              left     = input$lr[1],
              right    = input$lr[2],
              bottom   = input$bt[1],
              top      = input$bt[2]
            )
          
          if(!input$ignore_tag)  g = g + plot_annotation(tag_levels = "A")

          plot(g)
                    
        }) |>
        bindEvent(list(input$lr, input$bt, input$align_to, input$on_top, input$ignore_tag, input$show_inlay))
        
      output$inlayInfo <- renderUI({

         o = paste(
          fnargs$g, "+<br>",
          'patchwork::inset_element(', fnargs$p, ",",
          input$lr[1], ",", input$bt[1], ",", input$lr[2], ",", input$bt[2],
          ',align_to =', shQuote(input$align_to), 
          ',on_top =', input$on_top,
          'ignore_tag =', input$ignore_tag,
          ")"
        )
        
        o |>
          HTML() |>
          tags$code(style = "font-size:0.5em;")
        


      }) |> bindEvent(list(input$lr, input$bt, input$align_to,input$on_top,input$ignore_tag))
    
    }

    shinyApp(ui = ui, server = server)





}