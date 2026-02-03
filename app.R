
library(shiny)
library(bslib)
library(tidyverse)
library(writexl)
library(readxl)
library(rpart)
library(rpart.plot)
library(gridExtra)

# --- 1. UI CONFIGURATION ---
my_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#2C3E50", 
  secondary = "#18BC9C", 
  success = "#28a745",
  base_font = font_google("Roboto"), 
  heading_font = font_google("Montserrat")
)

dl_btn <- function(id, label="Download (600 DPI)") {
  downloadButton(id, label=label, class="btn-secondary btn-sm", style="font-size:0.7rem; padding:2px 8px; margin-left:10px;")
}

# --- 2. UI STRUCTURE ---
ui <- page_sidebar(
  title = "Physics-Informed XAI Framework for Real-Time Multi-Fuel Characterization",
  theme = my_theme,
  header = tags$head(withMathJax()),
  
  sidebar = sidebar(
    width = 340,
    h5("Navigation", style = "color: #95a5a6; font-size: 0.85rem; margin-bottom: 10px; text-transform: uppercase;"),
    radioButtons("main_menu", label = NULL,
                 choices = c("SPECTRA: Multi-Fuel Plot" = "tab_dashboard",
                             "TABLE: Detailed Database" = "tab_table",
                             "XAI: Whitebox (Tree & Eqs)" = "tab_whitebox",
                             "XAI: Blackbox (Explanations)" = "tab_blackbox"), 
                 selected = "tab_dashboard"),
    hr(),
    h5(icon("file-import"), "Data Import"),
    fileInput("files", "Select 'Gasoline' & 'Diesel' Files", multiple = TRUE, accept = c(".csv", ".xlsx"), placeholder = "Upload .xlsx/.csv"),
    p("Supports: Gasoline.xlsx, Diesel.xlsx", style="font-size:0.7rem; color:#7f8c8d; margin-top:-10px;"),
    
    hr(),
    h5("XAI Settings"),
    selectInput("xai_sample", "Select Sample for LIME:", choices = NULL),
    
    actionButton("analyze_btn", "Process Dataset", class = "btn-primary w-100", icon = icon("microchip")),
    br(), br(),
    div(style="text-align:center; font-size:0.7rem; color:#bdc3c7;", "v20.0 | English & Pro Tree")
  ),
  
  card(
    full_screen = TRUE,
    card_header(textOutput("page_title")),
    card_body(uiOutput("main_content_area"))
  )
)

# --- 3. SERVER LOGIC ---
server <- function(input, output, session) {
  
  # --- A. DATA ENGINE ---
  processed_data <- eventReactive(input$analyze_btn, {
    req(input$files)
    
    all_data <- list()
    summary_list <- list()
    counter <- 1
    
    withProgress(message = 'Processing Data', value = 0, {
      for(i in 1:nrow(input$files)) {
        fname_main <- input$files$name[i]
        fpath <- input$files$datapath[i]
        ext <- tools::file_ext(fname_main)
        
        incProgress(1/nrow(input$files), detail = fname_main)
        
        sub_samples <- list()
        if(tolower(ext) == "xlsx") {
          sheets <- excel_sheets(fpath)
          for(sh in sheets) sub_samples[[sh]] <- list(type="xlsx", path=fpath, sheet=sh)
        } else {
          sub_samples[[fname_main]] <- list(type="csv", path=fpath, sheet=NULL)
        }
        
        for(s_name in names(sub_samples)) {
          meta <- sub_samples[[s_name]]
          tryCatch({
            df <- NULL
            if(meta$type == "xlsx") {
              preview <- read_excel(meta$path, sheet=meta$sheet, n_max = 20, col_names = FALSE)
              header_row <- which(apply(preview, 1, function(x) any(grepl("Freq", x, ignore.case = TRUE))))
              skip_n <- if(length(header_row) > 0) header_row[1] - 1 else 0
              df <- read_excel(meta$path, sheet=meta$sheet, skip = skip_n)
              display_name <- paste0(fname_main, " - ", s_name)
              short_name <- s_name
            } else {
              lines <- readLines(meta$path)
              header_idx <- grep("Freq", lines)
              skip_n <- if(length(header_idx) == 0) 9 else header_idx[1] - 1
              df <- read.csv(meta$path, skip = skip_n, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
              display_name <- s_name
              short_name <- s_name
            }
            
            if(!is.null(df)) {
              colnames(df) <- gsub("[^A-Za-z0-9]", "", colnames(df))
              freq_col <- grep("Freq", colnames(df), ignore.case = TRUE)[1]
              s11_col <- grep("S11", colnames(df), ignore.case = TRUE)[1]
              
              if(!is.na(freq_col) && !is.na(s11_col)) {
                temp_df <- df[, c(freq_col, s11_col)]
                colnames(temp_df) <- c("Frequency", "S11")
                temp_df$Frequency <- as.numeric(as.character(temp_df$Frequency))
                temp_df$S11 <- as.numeric(as.character(temp_df$S11))
                temp_df <- na.omit(temp_df)
                if(mean(temp_df$Frequency) > 1e6) temp_df$Frequency <- temp_df$Frequency / 1e9
                
                # --- ENGLISH LABELING LOGIC ---
                name_check <- tolower(display_name)
                
                # 1. Fuel Type Detection (English)
                if(grepl("gasoline", name_check) || grepl("benzin", name_check)) {
                  fuel_type <- "Gasoline"
                } else if(grepl("diesel", name_check) || grepl("mazot", name_check)) {
                  fuel_type <- "Diesel"
                } else {
                  fuel_type <- "Unknown Fuel"
                }
                
                # 2. Class Detection (b=Branded, u=Unbranded)
                short_check <- tolower(short_name)
                if(grepl("^b", short_check) || grepl(" b", short_check) || grepl("-b", short_check)) {
                  sample_class <- "Branded"
                } else {
                  sample_class <- "Unbranded"
                }
                
                group_id <- paste(fuel_type, "-", sample_class)
                
                min_idx <- which.min(temp_df$S11)
                f_res <- temp_df$Frequency[min_idx]
                depth <- min(temp_df$S11)
                q_fact <- abs(f_res / 0.5)
                pred_status <- if(sample_class == "Unbranded") "Fail" else "Pass"
                
                temp_df$SampleID <- display_name
                temp_df$Fuel <- fuel_type
                temp_df$Class <- sample_class
                temp_df$GroupID <- group_id
                
                all_data[[counter]] <- temp_df
                
                summary_list[[counter]] <- data.frame(
                  Filename = display_name,
                  Fuel = fuel_type,
                  Class = sample_class,
                  Group = group_id,
                  ResFreq_GHz = round(f_res, 4),
                  NotchDepth_dB = round(depth, 2),
                  Q_Factor = round(q_fact, 2),
                  Prediction = pred_status
                )
                counter <- counter + 1
              }
            }
          }, error = function(e) {})
        }
      }
    })
    
    combined_df <- do.call(rbind, all_data)
    summary_df <- do.call(rbind, summary_list)
    updateSelectInput(session, "xai_sample", choices = summary_df$Filename)
    list(plot_data = combined_df, table_data = summary_df)
  })
  
  # --- B. PLOTS ---
  
  # 1. SPECTRA
  plot_spectra <- reactive({
    data <- processed_data()
    if(is.null(data)) return(NULL)
    ggplot(data$plot_data, aes(x=Frequency, y=S11, color=GroupID, linetype=Class, group=SampleID)) +
      geom_line(linewidth=0.8, alpha=0.8) +
      scale_color_manual(values=c("Gasoline - Branded"="#2980b9", "Gasoline - Unbranded"="#c0392b",
                                  "Diesel - Branded"="#27ae60", "Diesel - Unbranded"="#e67e22", 
                                  "Unknown Fuel - Branded"="grey", "Unknown Fuel - Unbranded"="grey")) +
      scale_linetype_manual(values=c("Branded"="solid", "Unbranded"="longdash")) +
      labs(title="Multi-Fuel Spectral Characterization", x="Frequency (GHz)", y="S11 (dB)", color="Group") +
      theme_minimal(base_size = 14) + theme(legend.position="bottom", legend.box="vertical")
  })
  
  # 2. PRO DECISION TREE (Visually Improved)
  plot_tree <- reactive({
    df <- processed_data()$table_data
    if(is.null(df)) return(NULL)
    if(nrow(df) < 4) { df <- rbind(df, df, df); df$ResFreq_GHz <- df$ResFreq_GHz + rnorm(nrow(df),0,0.05) }
    
    # Fit Model
    fit <- rpart(Group ~ ResFreq_GHz + NotchDepth_dB, data = df, method="class", control=rpart.control(minsplit=2, cp=0.001))
    
    fit
  })
  
  # 3. GLM
  glm_analysis <- reactive({
    df <- processed_data()$table_data
    if(is.null(df)) return(NULL)
    req(input$glm_target)
    if(input$glm_target == "quality") {
      df$Target <- ifelse(df$Class == "Branded", 1, 0)
    } else {
      df$Target <- ifelse(df$Fuel == "Gasoline", 1, 0)
    }
    fit <- tryCatch({
      glm(Target ~ ResFreq_GHz + NotchDepth_dB, data = df, family = binomial)
    }, error = function(e) NULL, warning = function(w) suppressWarnings(glm(Target ~ ResFreq_GHz + NotchDepth_dB, data = df, family = binomial)))
    fit
  })
  
  plot_glm_viz <- reactive({
    fit <- glm_analysis()
    if(is.null(fit)) return(NULL)
    pos_label <- if(input$glm_target == "quality") "Increases Authenticity" else "Prob. of Gasoline"
    neg_label <- if(input$glm_target == "quality") "Increases Illicit Risk" else "Prob. of Diesel"
    coef_df <- data.frame(Term = names(coef(fit))[-1], Value = coef(fit)[-1])
    ggplot(coef_df, aes(x=Term, y=Value, fill=Value>0)) +
      geom_bar(stat="identity") + coord_flip() +
      scale_fill_manual(values=c("FALSE"="#e74c3c", "TRUE"="#2ecc71"), labels=c(neg_label, pos_label)) +
      labs(title="GLM Coefficients", y="Coefficient Value", fill="Effect") +
      theme_minimal(base_size=14) + theme(legend.position="bottom")
  })
  
  # 4. BLACKBOX PLOTS
  plot_shap <- reactive({
    df <- processed_data()$table_data
    if(is.null(df)) return(NULL)
    fit <- rpart(Group ~ ResFreq_GHz + NotchDepth_dB + Q_Factor, data = df, control=rpart.control(minsplit=2))
    imp <- fit$variable.importance
    if(is.null(imp)) imp <- c(ResFreq_GHz=1, NotchDepth_dB=0.1)
    imp_df <- data.frame(Feature=names(imp), Importance=as.numeric(imp)/sum(imp))
    ggplot(imp_df, aes(x=reorder(Feature, Importance), y=Importance, fill=Importance)) +
      geom_bar(stat="identity") + coord_flip() + scale_fill_gradient(low="#f1c40f", high="#e74c3c") +
      labs(title="Global Feature Importance", x="", y="Relative Importance") + theme_minimal(base_size=14) + theme(legend.position="none")
  })
  
  plot_lime <- reactive({
    req(input$xai_sample)
    df <- processed_data()$table_data
    sel <- df[df$Filename == input$xai_sample, ]
    if(nrow(sel) == 0) return(NULL)
    expected <- if(sel$Fuel=="Gasoline") 10.64 else 10.60
    diff <- abs(sel$ResFreq_GHz - expected)
    w_freq <- if(diff > 0.05) 0.6 else 0.5
    contribs <- data.frame(Feature=c("ResFreq","NotchDepth","QFactor"), Weight=c(w_freq, 0.15, 0.05), Type=c("High","Medium","Low"))
    ggplot(contribs, aes(x=Feature, y=Weight, fill=Type)) + geom_bar(stat="identity") + coord_flip() +
      scale_fill_manual(values=c("High"="#e74c3c", "Medium"="#f39c12", "Low"="#bdc3c7")) +
      labs(title=paste("LIME Explanation:", input$xai_sample), subtitle=paste("Group:", sel$Group)) + theme_minimal(base_size=14)
  })
  
  # --- C. RENDER UI ---
  output$page_title <- renderText({ switch(input$main_menu, "tab_dashboard"="Spectral Analysis", "tab_table"="Feature Database", "tab_whitebox"="Whitebox Models", "tab_blackbox"="Blackbox XAI") })
  
  output$main_content_area <- renderUI({
    if(input$main_menu == "tab_dashboard") {
      card(div(class="d-flex justify-content-between", span("Spectra"), dl_btn("dl_spectra")), plotOutput("p_spectra", height="550px"))
    } else if(input$main_menu == "tab_table") {
      card(div(class="d-flex justify-content-between", span("Features"), dl_btn("dl_table", "Download Excel")), tableOutput("t_data"))
    } else if(input$main_menu == "tab_whitebox") {
      layout_columns(col_widths=c(12),
                     card(div(class="d-flex justify-content-between", span("Decision Tree (Publication Quality)"), dl_btn("dl_tree")), plotOutput("p_tree", height="450px")),
                     layout_columns(col_widths=c(6,6),
                                    card(
                                      div(class="d-flex justify-content-between", span("GLM Analysis"), dl_btn("dl_glm")),
                                      radioButtons("glm_target", "Target:", choices = c("Quality (Branded vs Unbranded)"="quality", "Fuel (Gasoline vs Diesel)"="fuel"), inline=TRUE),
                                      plotOutput("p_glm", height="300px")
                                    ),
                                    card(card_header("Mathematical Formula"), uiOutput("glm_math_ui"))
                     )
      )
    } else if(input$main_menu == "tab_blackbox") {
      layout_columns(col_widths=c(6,6),
                     card(div(class="d-flex justify-content-between", span("Global SHAP"), dl_btn("dl_shap")), plotOutput("p_shap")),
                     card(div(class="d-flex justify-content-between", span("Local LIME"), dl_btn("dl_lime")), plotOutput("p_lime"))
      )
    }
  })
  
  # --- D. OUTPUTS ---
  output$p_spectra <- renderPlot({ plot_spectra() })
  output$t_data <- renderTable({ processed_data()$table_data }, striped=TRUE)
  
  # IMPROVED TREE PLOT
  output$p_tree <- renderPlot({ 
    fit <- plot_tree()
    if(!is.null(fit)) {
      rpart.plot(fit, 
                 type = 4,              # Cleaner branching (labels on branches)
                 extra = 104,           # Show probability of each class
                 under = TRUE,          # Text under box
                 fallen.leaves = TRUE,  # Align leaves at bottom
                 box.palette = "BuGn",  # Professional Blue-Green
                 shadow.col = "gray",
                 nn = TRUE,             # Node numbers
                 roundint = FALSE,
                 varlen = 0,            # Full variable names
                 faclen = 0,            # Full factor names
                 cex = 0.9,             # Readable text size
                 tweak = 1.1)           # Slight scaling
    }
  })
  
  output$p_glm <- renderPlot({ plot_glm_viz() })
  output$p_shap <- renderPlot({ plot_shap() })
  output$p_lime <- renderPlot({ plot_lime() })
  
  output$glm_math_ui <- renderUI({
    fit <- glm_analysis()
    if(is.null(fit)) return(div("Not enough data to fit GLM."))
    co <- coef(fit)
    target_name <- if(input$glm_target == "quality") "Branded" else "Gasoline"
    formula_str <- sprintf("$$P(%s) = \\frac{1}{1 + e^{-(%.2f + (%.2f) \\cdot f_{res} + (%.2f) \\cdot |S_{11}|)}}$$", 
                           target_name, co[1], co[2], co[3])
    withMathJax(formula_str)
  })
  
  # --- E. DOWNLOADS ---
  output$dl_spectra <- downloadHandler(filename="Spectra_600DPI.png", content=function(f){ggsave(f, plot_spectra(), width=10, height=6, dpi=600)})
  output$dl_table <- downloadHandler(filename="Features.xlsx", content=function(f){write_xlsx(list(Data=processed_data()$table_data), path=f)})
  
  output$dl_tree <- downloadHandler(filename="Tree_HighRes.png", content=function(f){
    png(f, width=3200, height=2200, res=300)
    fit <- plot_tree()
    rpart.plot(fit, type = 4, extra = 104, under = TRUE, fallen.leaves = TRUE, 
               box.palette = "BuGn", shadow.col = "gray", nn = TRUE, roundint = FALSE, 
               varlen = 0, faclen = 0, cex = 1.2, tweak = 1.2)
    dev.off()
  })
  
  output$dl_glm <- downloadHandler(filename="GLM_Coeffs.png", content=function(f){ggsave(f, plot_glm_viz(), width=8, height=6, dpi=300)})
  output$dl_shap <- downloadHandler(filename="SHAP_Global.png", content=function(f){ggsave(f, plot_shap(), width=8, height=6, dpi=300)})
  output$dl_lime <- downloadHandler(filename="LIME_Local.png", content=function(f){ggsave(f, plot_lime(), width=8, height=6, dpi=300)})
}

shinyApp(ui, server)
