library(shiny)

# Increase the file upload limit to 100 MB
options(shiny.maxRequestSize = 500 * 1024^2)

ui <- fluidPage(
  titlePanel("Interactive EAG Trace Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose Files", multiple = TRUE, accept = c(".ASC", ".txt")),
      
      uiOutput("file_selector"),
      
      sliderInput("offset", "Offset (s):",
                  min = -1, max = 10, value = 0, step = 0.1),
      
      sliderInput("integral_length", "Integral Length (s):",
                  min = 0.1, max = 50, value = 10, step = 0.1),
      
      uiOutput("event_selector"),
      
      radioButtons("yscale", h3("Y axis scale"),
                   choices = list("max full EAG" = 1, 
                                  "selected y max value" = 2),
                   selected = 1),
      
      numericInput("ymax", label ="selected max value", 
                   value = 10000,step=1),
      numericInput("ymin", label ="selected min value", 
                   value = -10000,step=1),
      
      actionButton("save_areas", "Save Integrated Areas to CSV"),
      
      actionButton("save_area", "Save Integrated Area to CSV"),
      
      checkboxInput("Inv", "Invert Signal", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("main_plot"),
      plotOutput("zoom_plot")
    )
  )
)

server <- function(input, output, session) {
  #Define Noise time
  Noisetime<-4
  #COL<-palette.colors(palette = "Okabe-Ito")[c(7,4)]
  # Reactive to filter only every second file for selection
  
  
  filtered_files <- reactive({
    req(input$files)
    files <- input$files$name
    files[seq(1, length(files), by = 2)]
  })
  
  # Generate the dropdown for file selection dynamically
  output$file_selector <- renderUI({
    req(filtered_files())
    selectInput("selected_file", "Select a File to Analyze:", choices = filtered_files())
  })
  
  # Reactive to read and process the selected file
  selected_data <- reactive({
    req(input$selected_file)
    files <- input$files
    trace_path <- files$datapath[match(input$selected_file, files$name)]
    digital_path <- files$datapath[match(input$selected_file, files$name) + 1]
    
    # Reading trace data
    RawData <- read.delim(trace_path, skip = 5)
    colnames(RawData) <- c("time", "value")
    
    # Invert data if peaks are positive
    MEAN <- mean(RawData$value)
    MAX <- max(RawData$value)
    MIN <- min(RawData$value)
    if (abs(MAX - MEAN) > abs(MIN - MEAN)) {
      RawData$value <- -RawData$value
    }
    
    # Reading digital data
    DigData <- read.delim(digital_path, skip = 5)
    DigData$X <- seq_len(nrow(DigData))
    mr <- max(RawData$time)
    md <- max(DigData$X)
    cor <- md / mr
    DigData$cor <- cor / DigData$In1
    DigData$cor[is.infinite(DigData$cor)] <- 0
    DigData$Time <- (mr / nrow(DigData)) * DigData$X

    
    # Reduce trace data for spline
    N <- length(RawData$time) / 10
    Spline.RawData <- spline(RawData$time, RawData$value, n = N)
    
    # Detect start times
    closure_start_indices <- which(diff(DigData$In1) == 1) + 1
    closure_start_times <- DigData$Time[closure_start_indices]
    min_gap <- 1
    filtered_start_times <- closure_start_times[c(TRUE, diff(closure_start_times) > min_gap)]
    
    list(
      RawData = RawData,
      DigData = DigData,
      Spline = Spline.RawData,
      StartTimes = filtered_start_times,
      MIN = MIN
    )
  })
  
  # Update event selector dynamically based on detected start times
  output$event_selector <- renderUI({
    req(selected_data())
    selectInput("selected_event", "Select an Event to Zoom In:",
                choices = seq_along(selected_data()$StartTimes))
  })
  
  # Reactive value to store calculated areas
  integrated_areas <- reactiveVal(data.frame(File = character(), Event = integer(), Area = numeric(),Height =numeric()))
  
  # Generate the main plot dynamically
  output$main_plot <- renderPlot({
    req(selected_data())
    data <- selected_data()
    Offset <- input$offset
    integral_length <- input$integral_length
    RawData <- data$RawData
    DigData <- data$DigData
    Spline <- data$Spline
    StartTimes <- data$StartTimes
    MIN <- data$MIN
    
    
    # Plot the trace
    if (input$Inv){
      Spline$y<--Spline$y
      MEAN <- mean(Spline$y)
      MAX <- max(Spline$y)
      MIN <- min(Spline$y)
    }
    
    # --------------------------------max y-scale---------------------------
    if(input$yscale ==1){
    plot.new()
    plot(Spline, type = "l", xlab = "Time (s)", ylab = "Value (nV)", main = "Trace and Digital Data")
    MIN <- min(Spline$y)

    DeltaSpline<-max(Spline$y)-min(Spline$y)
    print(paste("DeltaSpline",DeltaSpline))
    CorDig<-DeltaSpline/max(DigData$cor)
    DigData$cor2<-DigData$cor*CorDig/10
    
    points((DigData$cor2 + MIN) ~ DigData$Time, data = DigData, type = "s", col = "red", lwd = 2)

    for (k in seq_along(StartTimes)) {
      polygon_x <- c(
        Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$x[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        StartTimes[k] + Offset + integral_length
      )
      polygon_y <- c(
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$y[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2))][length(which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2)))]
      )
      polygon_y[length(polygon_y)]<-polygon_y[1]
      
      # Handle polygon dimension mismatches
      if (length(polygon_x) != length(polygon_y)) {
        if (length(polygon_x) > length(polygon_y)) {
          polygon_x <- polygon_x[-(length(polygon_x) - 1)]
        } else {
          polygon_y <- polygon_y[-(length(polygon_y) - 1)]
        }
      }
      polygon_y[length(polygon_y)]<-polygon_y[1]
      # Draw the polygon
      polygon(polygon_x, polygon_y, col = "blue", border = NA)
    #plot(polygon_x, polygon_y,type="l")
    }
    }
    # --------------------------------independend y-scale---------------------------
    if(input$yscale ==2){
      y_min <- as.numeric(input$ymin)
      y_max <- as.numeric(input$ymax)
      plot(Spline, type = "l", xlab = "Time (s)", ylab = "Value (nV)", main = "Trace and Digital Data",ylim = c(y_min, y_max))
      
      Deltay<-y_max-y_min
      CorDig<-Deltay/max(DigData$cor)
      DigData$cor2<-DigData$cor*CorDig/10
      points((DigData$cor2 + y_min) ~ DigData$Time, data = DigData, type = "s", col = "red", lwd = 2)
      
      for (k in seq_along(StartTimes)) {
        polygon_x <- c(
          Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
          Spline$x[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
          StartTimes[k] + Offset + integral_length
        )
        polygon_y <- c(
          Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
          Spline$y[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
          Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2))][length(which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2)))]
        )
        polygon_y[length(polygon_y)]<-polygon_y[1]
        
        # Handle polygon dimension mismatches
        if (length(polygon_x) != length(polygon_y)) {
          if (length(polygon_x) > length(polygon_y)) {
            polygon_x <- polygon_x[-(length(polygon_x) - 1)]
          } else {
            polygon_y <- polygon_y[-(length(polygon_y) - 1)]
          }
        }
        polygon_y[length(polygon_y)]<-polygon_y[1]
        # Draw the polygon
        polygon(polygon_x, polygon_y, col = "blue", border = NA)
      }
    }
    
  })
  
  # Generate the zoomed-in plot dynamically
  output$zoom_plot <- renderPlot({
    req(selected_data(), input$selected_event)
    data <- selected_data()
    Offset <- input$offset
    integral_length <- input$integral_length
    Spline <- data$Spline
    StartTimes <- data$StartTimes
    k <- as.numeric(input$selected_event)
    if (input$Inv){
      Spline$y<--Spline$y
      MEAN <- mean(Spline$y)
      MAX <- max(Spline$y)
      MIN <- min(Spline$y)
    }
    # Define the polygon points
    # --------------------------------max y-scale---------------------------
    if(input$yscale ==1){
    polygon_x <- c(
      Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
      Spline$x[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
      StartTimes[k] + Offset + integral_length
    )
    polygon_y <- c(
      Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
      Spline$y[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
      Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2))][length(which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2)))]
    )
    polygon_y[length(polygon_y)]<-polygon_y[1]
    
    # Handle polygon dimension mismatches
    if (length(polygon_x) != length(polygon_y)) {
      if (length(polygon_x) > length(polygon_y)) {
        polygon_x <- polygon_x[-(length(polygon_x) - 1)]
      } else {
        polygon_y <- polygon_y[-(length(polygon_y) - 1)]
      }
    }
    polygon_y[length(polygon_y)]<-polygon_y[1]
    
    # Get y-axis limits for zoom
    y_min <- min(Spline$y[Spline$x > (StartTimes[k] + Offset - 5) & Spline$x < (StartTimes[k] + integral_length + 5)])
    y_max <- max(Spline$y[Spline$x > (StartTimes[k] + Offset - 5) & Spline$x < (StartTimes[k] + integral_length + 5)])
    
    # Generate zoomed-in plot
    plot(Spline, type = "l", 
         xlim = c(StartTimes[k] + Offset - 5, StartTimes[k] + integral_length + 5), 
         ylim = c(y_min, y_max), 
         main = paste("Zoomed Plot: Event", k), xlab = "Time (s)", ylab = "Value (nV)")
    polygon(polygon_x, polygon_y, col = "blue", border = NA)
    
    # Add a red line to indicate the filtered start time
    abline(v = StartTimes[k], col = "red", lwd = 2)

    
    } # --------------- end of max yscale ----------------------------
      # -------------- beginning of independend scale ----------------
    if(input$yscale ==2){
      polygon_x <- c(
        Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$x[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        StartTimes[k] + Offset + integral_length
      )
      polygon_y <- c(
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$y[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2))][length(which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2)))]
      )
      polygon_y[length(polygon_y)]<-polygon_y[1]
      
      # Handle polygon dimension mismatches
      if (length(polygon_x) != length(polygon_y)) {
        if (length(polygon_x) > length(polygon_y)) {
          polygon_x <- polygon_x[-(length(polygon_x) - 1)]
        } else {
          polygon_y <- polygon_y[-(length(polygon_y) - 1)]
        }
      }
      polygon_y[length(polygon_y)]<-polygon_y[1]
      # Get y-axis limits for zoom
      y_min <- as.numeric(input$ymin)
      y_max <- as.numeric(input$ymax)
      
      # Generate zoomed-in plot
      plot(Spline, type = "l", 
           xlim = c(StartTimes[k] + Offset - 5, StartTimes[k] + integral_length + 5), 
           ylim = c(y_min, y_max), 
           main = paste("Zoomed Plot: Event", k), xlab = "Time (s)", ylab = "Value (nV)")
      polygon(polygon_x, polygon_y, col = "blue", border = NA)
      
      # Add a red line to indicate the filtered start time
      abline(v = StartTimes[k], col = "red", lwd = 2)
      
    } 
    
    #### Signal to noise ####
    height<-min(polygon_y)-polygon_y[1]
    #print(height)
    abline(h=min(polygon_y))
    abline(h=polygon_y[1])
    lines(x=c(head(polygon_x,1),tail(polygon_x,1)),y=c(head(polygon_y,1),tail(polygon_y,1)),lwd=3,col="darkblue")

    MaxNoise<-max(Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))],
                  Spline$y[which(Spline$x > StartTimes[k] -Noisetime & Spline$x < (StartTimes[k]))])
    MinNoise<-min(Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))],
                  Spline$y[which(Spline$x > StartTimes[k] -Noisetime & Spline$x < (StartTimes[k]))])
    
    lines(x=c(Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))[1]],
              Spline$x[which(round(Spline$x, 2) == round(StartTimes[k], 2))[length(which(round(Spline$x, 2) == round(StartTimes[k], 2)))]]),
          y=c(MaxNoise,MaxNoise),col="darkgreen",lwd=1)
    lines(x=c(Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))[1]],
              Spline$x[which(round(Spline$x, 2) == round(StartTimes[k], 2))[length(which(round(Spline$x, 2) == round(StartTimes[k], 2)))]]),
          y=c(MinNoise,MinNoise),col="darkgreen",lwd=1)
    Noise<-MaxNoise-MinNoise
    #print(paste("Noise =",Noise))
    
    SignalToNoise<-abs(height)/abs(Noise)
    #print(paste("signal to Noise =",SignalToNoise))
    text(x=StartTimes[k] -Noisetime/2,y=MinNoise+Noise/2,labels =paste("Signal to Noise =",round(SignalToNoise)),lwd=3)
    ########################## Here is the problem ###########################
    line_length <- sqrt(((tail(polygon_x,1)) - head(polygon_x,1))^2 + (tail(polygon_y,1) - head(polygon_y,1))^2)
    #print(c(tail(polygon_x,1), head(polygon_x,1),tail(polygon_y,1), head(polygon_y,1)))
    #print("line length")
    #print(line_length)
    
    
    
    
  })
  
  # Save integrated areas when the button is clicked
  observeEvent(input$save_areas, {
    req(selected_data())
    data <- selected_data()
    Offset <- input$offset
    integral_length <- input$integral_length
    Spline <- data$Spline
    StartTimes <- data$StartTimes
    if (input$Inv){
      Spline$y<--Spline$y}
    # Calculate areas for each event
    areas<-NULL
    heights<-NULL
    SignalToNoises<-NULL
    MaxNoises<-NULL
    MinNoises<-NULL
    Noises<-NULL
    
    for (k in 1:length(StartTimes)){
      # Define the polygon points
      #print("Starttimes:")
      #print(StartTimes[k])
      polygon_x <- c(
        Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$x[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        StartTimes[k] + Offset + integral_length
      )
      polygon_y <- c(
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$y[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2))][length(which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2)))]
      )
      polygon_y[length(polygon_y)]<-polygon_y[1]
      
      # Handle polygon dimension mismatches
      if (length(polygon_x) != length(polygon_y)) {
        if (length(polygon_x) > length(polygon_y)) {
          polygon_x <- polygon_x[-(length(polygon_x) - 1)]
        } else {
          polygon_y <- polygon_y[-(length(polygon_y) - 1)]
        }
      }
      polygon_y[length(polygon_y)]<-polygon_y[1]
    area <- abs(sum(diff(polygon_x) * (head(polygon_y, -1) + tail(polygon_y, -1)) / 2))
    #print(area)
    areas<-c(areas,area)
    height<-min(polygon_y)-polygon_y[1]
    #print(height)
    heights<-c(heights,height)
    
    
    MaxNoise<-max(Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))],
                  Spline$y[which(Spline$x > StartTimes[k] -Noisetime & Spline$x < (StartTimes[k]))])
    MinNoise<-min(Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))],
                  Spline$y[which(Spline$x > StartTimes[k] -Noisetime & Spline$x < (StartTimes[k]))])
    

    Noise<-MaxNoise-MinNoise
    #print(paste("Noise =",Noise))
    
    SignalToNoise<-abs(height)/abs(Noise)
    SignalToNoises<-c(SignalToNoises,SignalToNoise)
    MaxNoises<-c(MaxNoises,MaxNoise)
    MinNoises<-c(MinNoises,MinNoise)
    Noises<-c(Noises,Noise)
    
    
    }
    height<-min(polygon_y)-polygon_y[1]
    #print(height)

    # Store results in a reactive value
    integrated_areas(data.frame(
      File = input$selected_file,
      Event = seq_along(areas),
      Offset = Offset,
      Integral_Length = integral_length,
      Height = heights,
      Area = areas,
      SignalToNoise = SignalToNoises,
      MaxNoise = MaxNoises,
      MinNoise = MinNoises,
      Noise = Noises
     
    ))
    #print(integrated_areas())
    # Save to CSV
    write.table(integrated_areas(), 
                file = "integrated_areas.csv", row.names = FALSE,col.names = F,
                append=T, sep=",",dec=".")
  }
  
  )
  
  
  # Save integrated area when the button is clicked
  observeEvent(input$save_area, {
    req(selected_data())
    data <- selected_data()
    Offset <- input$offset
    integral_length <- input$integral_length
    Spline <- data$Spline
    StartTimes <- data$StartTimes
    
    if (input$Inv){
      Spline$y<--Spline$y}
    
    areas<-NULL
    heights<-NULL
    k<-as.numeric(input$selected_event)
      # Define the polygon points
      polygon_x <- c(
        Spline$x[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$x[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        StartTimes[k] + Offset + integral_length
      )
      polygon_y <- c(
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset, 2))],
        Spline$y[which(Spline$x > StartTimes[k] + Offset & Spline$x < (StartTimes[k] + Offset + integral_length))],
        Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2))][length(which(round(Spline$x, 2) == round(StartTimes[k] + Offset + integral_length, 2)))]
      )
      polygon_y[length(polygon_y)]<-polygon_y[1]
      
      # Handle polygon dimension mismatches
      if (length(polygon_x) != length(polygon_y)) {
        if (length(polygon_x) > length(polygon_y)) {
          polygon_x <- polygon_x[-(length(polygon_x) - 1)]
        } else {
          polygon_y <- polygon_y[-(length(polygon_y) - 1)]
        }
      }
      polygon_y[length(polygon_y)]<-polygon_y[1]
      area <- sum(diff(polygon_x) * (head(polygon_y, -1) + tail(polygon_y, -1)) / 2)
      #print(area)
      areas<-c(areas,area)
      height<-min(polygon_y)-polygon_y[1]
      #(height)
      heights<-c(heights,height)
      #### Signal to noise for saving ####
      
      MaxNoise<-max(Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))],
                    Spline$y[which(Spline$x > StartTimes[k] -Noisetime & Spline$x < (StartTimes[k]))])
      MinNoise<-min(Spline$y[which(round(Spline$x, 2) == round(StartTimes[k] -Noisetime, 2))],
                    Spline$y[which(Spline$x > StartTimes[k] -Noisetime & Spline$x < (StartTimes[k]))])
      Noise<-MaxNoise-MinNoise
      SignalToNoise<-abs(height)/abs(Noise)
      #### Signal to noise saving end ####
    
        
    # Calculate areas for each event
    # areas <- sapply(seq_along(StartTimes), function(k) {
    #   indices <- which(Spline$x >= StartTimes[k] + Offset & Spline$x <= StartTimes[k] + Offset + integral_length)
    #   sum(Spline$y[indices]) * diff(Spline$x[indices[1:2]])
    # })
    # 
    # heights<- sapply(seq_along(StartTimes), function(k) {
    #   indices <- which(Spline$x >= StartTimes[k] + Offset & Spline$x <= StartTimes[k] + Offset + integral_length)
    #   abs(min(Spline$y[indices])-Spline$y[indices][1])
    # })
    
    # Store results in a reactive value
    integrated_areas(data.frame(
      File = input$selected_file,
      Event = k,
      Offset = Offset,
      Integral_Length = integral_length,
      Height = heights,
      Area = areas,
      SignalToNoise = SignalToNoise,
      MaxNoise = MaxNoise,
      MinNoise = MinNoise,
      Noise = Noise
    ))
    #print("input selected event")
    #print(input$selected_event)
    #print(str(input$selected_event))
    #print(integrated_areas())
    #print(integrated_areas()[input$selected_event,])
    # Save to CSV
    write.table(integrated_areas(), 
                file = "integrated_areas.csv", row.names = FALSE,col.names = F,
                append=T, sep=",",dec=".")
  }

  )
}

shinyApp(ui = ui, server = server)
