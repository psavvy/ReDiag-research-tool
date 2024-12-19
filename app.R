################################################################################
# Title: ReDiag: a teaching tool to assess the linearity and normality regression assumptions
# Author: Perseverence Savieri
# Date: Sys.Date()
# Version: 003
################################################################################


options(shiny.maxRequestSize = 10000 * 1024^2)

###############
# Load packages
###############
library(shiny)
library(ggplot2)
library(qqplotr)
library(dplyr)
library(memisc)
library(formatR)
# library(xlsx)
library(readxl)
library(lmtest)
library(DT)
library(knitr)
library(haven)
library(car)
library(MASS)
library(lindia)
library(summarytools)
library(shinymeta)
library(shinyBS)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(kableExtra)
library(shinyFeedback)
library(shinyAce)
library(bslib)

################################################################################
# Define UI layout
##################
ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  tags$head(
    # Google Analytics tracking code for GA4
    HTML(
      "<!-- Google tag (gtag.js) -->
      <script async src='https://www.googletagmanager.com/gtag/js?id=G-1VX5NGXWDD'></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
      
        gtag('config', 'G-1VX5NGXWDD');
        </script>"
    )
  ),
  
  # Open navbarPage for main headings
  navbarPage(
    tags$p("ReDiag", style = "color:#003399"),
    # Open the Home tabPanel
    tabPanel(
      tags$p("Home", style = "color:#FF6600"),
      sidebarLayout(
        sidebarPanel(
          width = 4,
          # img(src = "vub_square_rgb.jpg", width = 420, height = 100),
          # br(),
          img(src = "logo_bisi_rgb.jpg", width = 420, height = 100),
          br(),
        ), # End Home sidebarPanel

        mainPanel(
          width = 8,
          tags$h4("Regression Diagnostics web-application", style = "color:#003399"),
          tags$p("The ReDiag app assists students and researchers with their residual analysis for linear regression models. It offers visualization tools to assess the normality of residuals and the often-overlooked assumption of linearity. The user-friendly interface enables the user to critically assess their model and correct any violations. A dynamic report can be downloaded with the results."),
          tags$p("This app was developed by the", tags$a(href = "https://square.research.vub.be/", "Support for Quantitative and Qualitative Research (SQUARE)"), "to provide the research community with complimentary support in statistics. The SQUARE core facility is part of the", tags$a(href = "https://bisi.research.vub.be/", "Biostatistics and Medical Informatics research group (BISI)"), "at the Vrije Universiteit Brussel (VUB)."),
          br(),
          tags$h4("Terms of use", style = "color:#003399"),
          tags$p("ReDiag is not designed for model building or variable selection. It is distributed in the hope that it may be useful in assessing regression assumptions."),
          tags$p("Uploaded data and outputs from analyses won't be kept on our servers. Therefore, you must refrain from uploading any sensitive information because the research tool is supplied WITHOUT ANY WARRANTY. Instead, you can download the reports and any modified data. If you submit any data to this application, you are solely responsible for its confidentiality, availability, security, loss, abuse, and misappropriation."),
          br(),
          tags$h4("Data statement", style = "color:#003399"),
          tags$p("Data for the analysis of behavioural outcomes ", tags$a(href = "https://doi.org/10.1080/09553002.2023.2283092", "(Janssen et al., 2024)"), "and to assess the relationship between occupancy and ammonia build-up ", tags$a(href = "https://doi.org/10.1038/s41684-023-01179-0", "(Eskandarani et al., 2023)"), "were obtained from published studies."),
          br(),
          tags$h4("Feedback", style = "color:#003399"),
          tags$p("We would love to hear your thoughts, suggestions, concerns or problems you encountered while using ReDiag so that we can improve. To do this, kindly evaluate the web-application via this", tags$a(href = "https://vub.fra1.qualtrics.com/jfe/form/SV_0lcDxYTeMgcP5oG", "link.")),
          br(), br(),
          br(), br(),
          # tags$h4("Note: This is not the final version of the app. It is still under development!", style = "color:#FF0000"),
          br(), br(),
          tags$hr(),
          tags$h6(em("Copyright 2024, Support for Quantitative and Qualitative Research, Version 01.03.24"), align = "center"),
          br()
        ) # End Home mainPanel
        # position = "right"
      ) # End Home sidebarLayout
    ), # Close the Home tabPanel

    ############################################################################
    # Open the Model tabPanel
    #########################
    tabPanel(
      tags$p("Model", style = "color:#FF6600"),
      # Open Model fluidPage
      fluidPage(
        tags$style(HTML("
                                .col-sm-4 > .nav > li[class=active] > a {
                                   background-color: #FFF;
                                }")),
        tags$style(HTML("
                               .tabs-above > .nav > li > a {
                                   background-color: #FFF;
                                   color: #2DB20A;
                                }")),
        #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        # INPUT ---------------------------------------------------------------
        # Open sidebar Layout for Model tab
        sidebarLayout(
          # Open sidebarPanel
          sidebarPanel(
            width = 3,
            # Open navlistPanel
            navlistPanel(
              fluid = T,
              id = "sidetabs",
              # Select data ----------------------------------------------------
              tabPanel(
                tags$p("Data input", style = "color:#003399"),
                value = "spanel1",
                # Buttons to select options
                selectizeInput(
                  inputId = "exdata", label = strong("Choose example data"), selected = "",
                  choices = c("", "CS1: Open field test", "CS2: Occupancy"),
                  options = list(placeholder = "Choose mice data")
                ),
                tags$hr(),
                # For reading in the data + which types are all accepted
                tags$h5(em("OR load your own data file")),
                radioButtons("ext",
                  label = strong("Select file extension"),
                  choices = list(
                    "Text file (.txt)" = "txt",
                    "CSV file (.csv)" = "csv",
                    "Excel file (.xlsx)" = "xlsx",
                    "SPSS file (.sav)" = "sav",
                    "Stata Dataset (.dta)" = "dta"
                  ),
                  selected = ""
                ),
                conditionalPanel(
                  condition = "input.ext == 'txt' | input.ext == 'csv' | input.ext == 'xlsx' | input.ext == 'sav' | input.ext == 'dta'",
                  tryCatch(
                    fileInput("file1", strong("Select file"),
                      accept = c(
                        ".txt", ".csv", ".xlsx", ".sav", ".dta",
                        ".CSV", ".TXT", ".XLSX", ".SAV", ".DTA"
                      )
                    )
                  )
                ),
                tags$hr(),

                # Change data types
                uiOutput("edit_vars"),
              ), # Close Data input tabPanel

              # Define model ---------------------------------------------------
              tabPanel(
                tags$p("Define Model", style = "color:#003399"),
                value = "spanel2",
                selectizeInput(
                  inputId = "outcome", label = strong("Select outcome variable"),
                  choices = NULL
                ),
                selectizeInput(
                  inputId = "predictor", label = strong("Select predictor variable(s)"),
                  choices = NULL,
                  multiple = TRUE
                ),
                tags$hr(),
                selectizeInput(
                  inputId = "interaction", label = strong("Select interaction term(s)"),
                  choices = NULL,
                  multiple = TRUE
                ),
                actionButton("actionBtnAdd", "Create and add term(s)"),
                uiOutput("uiAdded"),
                tags$hr(),
                actionButton("runmodel", "Run analysis")
              ), # Close Define model tabPanel


              # Transform variables --------------------------------------------
              tabPanel(
                tags$p("Transform", style = "color:#003399"),
                value = "spanel3",
                # Select variable to transform
                checkboxInput(
                  inputId = "qqcox", label = strong("Box-Cox transformation"), value = FALSE
                ),
                conditionalPanel(
                  condition = "input.qqcox == 1",
                  tryCatch(
                    sliderInput("lambda", "Select lambda (power)",
                      value = 1,
                      min = -3,
                      max = 3,
                      step = 0.25
                    )
                  )
                ),
                tags$hr(),
                uiOutput("data_management"),
                tags$hr(),
                radioButtons("downloadType", "Save transformed dataset",
                  choices = c(
                    "CSV" = ".csv",
                    "Excel" = ".xlsx",
                    "TSV" = ".tsv"
                  ),
                  inline = TRUE
                ),
                downloadButton("downloadData", "Download")
                # actionButton("savevar", "Save variable")
              ), # Close Transform tabPanel

              # Reset data -----------------------------------------------------
              tabPanel(
                tags$p("Reset", style = "color:#003399"),
                value = "spanel4",
                tags$p("To start a new session please click this button:"),
                actionButton("newanalysis", "Reset"),
                uiOutput("reload"),
                br()
              ) # Close Reset tabPanel
            ) # Close nvalistPanel
          ), # Close sidebarPanel for Model tab

          #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          # OUTPUT ---------------------------------------------------------------
          # Open mainPanel
          mainPanel(
            width = 9,
            # Open tabsetPanel
            tabsetPanel(
              id = "maintabs",
              # Data Table ----------------------------------------------------
              tabPanel(
                tags$p("View Data", style = "color:#003399"),
                value = "mpanel1",
                br(), br(),
                fluidRow(
                  column(5, uiOutput("powerUI")),
                  column(5, uiOutput("boxcoxUI"))
                ),
                br(),
                tags$hr(),
                dataTableOutput("mytable1"),
                br(), br(),
                # uiOutput("transcoxUI")
              ), # Close View data tabPanel
              # Data Summary ---------------------------------------------------
              tabPanel(
                tags$p("Data Summary", style = "color:#003399"),
                htmlOutput("data_summ"),
                br(), br(),
              ),
              # Model summary --------------------------------------------------
              tabPanel(
                tags$p("Model Summary", style = "color:#003399"),
                value = "mpanel2",
                tags$h3("Statistical Analysis"),
                tags$p("Selected variables for analysis", style = "color:#FF6600"),
                # tags$p("Selected variables for analysis"),
                verbatimTextOutput("input"),
                br(),
                tags$p("The table shows the results from the fitted linear regression model", style = "color:#FF6600"),
                # tags$p("The table shows the results from the fitted linear regression model"),
                br(),
                uiOutput("table"),
                br()
              ), # Close Model Summary tabPanel

              # Model Diagnostics: Linearity -------------------------------------
              tabPanel(
                tags$p("Linearity Assumption", style = "color:#003399"),
                value = "mpanel3",
                tags$h3("Linearity and consequences of non-linearity"),
                checkboxInput("show_LR_info", "Show background information", value = FALSE),
                conditionalPanel(
                  condition = "input.show_LR_info == true",
                  tags$p("One of the assumptions of a linear regression model is that the mean of the outcome Y is a linear function of the predictor variable X. In multiple linear regression, the relationship between every predictor variable and the mean of outcome, is assumed to be linear when the other variables are held constant. This implies that the model is linear in the regression parameters/coefficients meaning the conditional mean of the residuals is assumed to be zero for any given combination of values of the predictor variables."),
                  tags$p("This assessment aims to check the assumption of linearity on the fitted model. We investigate if there are any deviations or specification errors which may result in underfitting."),
                  tags$p("Violation of the linearity assumption implies that the model fails to represent the pattern of the relationship between the mean response and the predictor variables. The estimates of the regression parameters will be biased (fail to estimate the true value), inconsistent (convergence is not guaranteed) and inefficient (estimator has a large sampling variance)."),
                  br(),
                  tags$h4("How to assess the linearity assumption?"),
                  tags$p(em("Prior knowledge: used terminology.")),
                  tags$p("A", em("fitted value", style = "color:#FF6600"), "is a value of the outcome variable estimated from the OLS regression line. Fitted values can also be referred to as predicted values.", tipify(em("Residuals", style = "color:#FF6600"), img(src = "residex.png", height = 200), placement = "left", trigger = "hover"), "are the differences between observed values and their corresponding fitted values that lie on the regression line."),
                  br(),
                  tags$p(em("Graphical methods that can be used")),
                  withMathJax(
                    tags$p("Plots of the residuals based on the fitted model can be used to check the assumption of linearity."),
                    tags$ol(
                      tags$li("A scatter plot of the observed values against the fitted values gives an overview of the marginal relationships between \\(Y\\) and \\(X\\). It is plotted with a loess curve (locally estimated scatter plot smoother) which does not assume the form of the relationship between \\(Y\\) and \\(X\\) (e.g. linear model) but rather produces a smooth line that follows the trend in the data."),
                      tags$li("A plot of the residuals versus the fitted values can be examined to complement the information from the scatter plot.")
                    )
                  )
                ),
                br(),
                tags$h4("Acceptable appearance of residual plots"),
                br(),
                fluidRow(
                  column(1, ),
                  column(
                    5, img(src = "ovpsample.png", height = 450, align = "center"),
                    br(), br(),
                    tags$p("The scatter plot of the observed values against fitted values measures the accuracy of the fitted model and assesses any strong deviations from the regression line. The linearity assumption is met if the loess curve (red) approximately follows the regression line (blue) and remains in the confidence interval bounds (in grey).")
                  ),
                  column(
                    5, img(src = "rvpsample.png", height = 450, align = "center"),
                    br(), br(),
                    tags$p("When the linear regression model is correct, the points should be randomly scattered around the zero-line (the horizontal line where residuals equal zero), with no systematic pattern. This zero-line represents the situation where there is no difference between observed and fitted values of Y. The loess curve should approximately follow the zero-line and a curvature could indicate model misspecification and nonlinearity.")
                  )
                ), # Close fluidRow for ideal appearances
                # br(),
                # h4("Residual plots when non-linearity exists"),
                br(),
                uiOutput("linearityUI"),
                br(),
                tags$h4("Remedies"),
                tags$p("When non-linearity is detected, it is recommended to use procedures that account for the model misspecification."),
                br(),
                tags$p(em("Approach 1: Polynomial regression")),
                tags$p("The core principle behind polynomial regression is to use a non-linear function to transform the predictor variable. For example, a simple and commonly used transformation is to square the predictor variable (second-order polynomial) to model a U-shaped relationship."),
                withMathJax(
                  tags$p("After this adjustment, the fitted model will follow the structure in the data. Therefore, if the relationship between \\(X\\) and mean response of \\(Y\\) is U-shaped (curvelinear), the appropriate model is a quadratic regression model which has a second-order polynomial in X (i.e., \\(\\hat{Y} = β_0+ β_1 X_{i1}+ β_2 X_{i1}^2\\)). In a cubic regression model, a third-order polynomial in X is introduced such that, \\(\\hat{Y} = β_0+ β_1 X_{i1}+ β_2 X_{i1}^2+ β_3 X_{i1}^3\\)."),
                  tags$p("Variables can be transformed under the", strong("Transform"), "sidebar tab to incorporate such high order terms, and users can then redifine the model in the", strong("Define Model"), "sidebar tab."),
                  tags$p(strong("Note: Polynomial regression models are still linear models because they are linear in their parameters.")),
                  br(),
                  tags$p(em("Approach 2: Piecewise regression")),
                  tags$p("This form of regression allows multiple linear models to be fitted to the data for different ranges of \\(X\\). For example, if the data follow different linear trends over various regions of the data. We should model the regression function in “pieces” which can be connected. In this version of ReDiag, we will not be using this approach.")
                ),
                br(), br()
              ), # Close Linearity tabPanel

              # Model Diagnostics: C+R Plots -------------------------------------
              tabPanel(
                tags$p("Linearity Assumption: C+R Plots", style = "color:#003399"),
                value = "mpanel4",
                tags$h3("Component-plus-Residual plots"),
                tags$p("When your model includes multiple predictor variables, component-plus-residual (partial residual) plots are important to inspect in addition to the plots described in the", strong("Linearity Assumption"), "tab. This is because the residuals are determined by several predictor variables and it becomes easier to link any deviations from linearity to a specific predictor variable."),
                withMathJax(
                  tags$p("A scatter plot matrix is useful in multiple linear regression. This is a two-dimensional scatter diagram of \\(y\\) versus each \\(X\\) (i.e y versus \\(X_1\\), y versus \\(X_2\\),\\(...\\), y versus \\(X_k\\)). However, to check the assumption of linearity, these plots do not paint the whole picture (and can be misleading) because our interest centres on the partial relationship between \\(y\\) and each \\(X\\), controlling for the other \\(X\\)s, not on the marginal relationship between \\(y\\) and a single \\(X\\).")
                ),
                tags$p("Component-plus-residual plots become relevant in checking the linearity assumption in cases where there is more than one predictor variable. These partial residual plots display the residuals of one predictor variable against the outcome variable."),
                tags$p("The linearity assumption is met when the loess curve (solid puple line) follows the regression line (dashed blue line). Deviations between the loess curve and regression line are indicative of deviations from linearity in the partial relationship between X and Y. Box plots are used for categorical variables instead of scatter plots. This is because the linearity assupmtion is not required for the relationship between a categorical predictor and a continuous outcome."),
                br(),
                fluidRow(
                  # column(1, ),
                  column(12, outputCodeButton(plotOutput("crplot", height = "100%", width = "100%")))
                  # align = "center"
                ),
                br()
              ), # Close C+R Plots tabPanel

              # Model Diagnostics: Normality -------------------------------------
              tabPanel(
                tags$p("Normality Assumption", style = "color:#003399"),
                value = "mpanel5",
                tags$h3("Normality and consequences of non-normality"),
                checkboxInput("show_Norm_info", "Show background information", value = FALSE),
                conditionalPanel(
                  condition = "input.show_Norm_info == true",
                tags$p("Another assumption of a linear regression model is that the residuals are normality distributed. In short, this means the normal density plot should have a single peak (i.e. be unimodal) and be symmetric instead of skewed. Note that the outcome Y is not required to be normally distributed because normality in Y does not guarantee normality in residuals. It is the outcome, controlling for the predictor variables, that needs to fulfill the requirement of normality. However, the outcome has to be continuous (not discrete) for us to assess this assumption."),
                tags$p("When the errors are non-normally distributed, the least-squares estimator has a large sampling variance (inefficient) and this distorts the interpretation of the model. This is because the conditional mean of Y given the X's, is a sensitive measure in skewed distributions."),
                br(),
                tags$h4("How to assess the normality assumption?"),
                tags$p(em("Graphical methods that can be used")),
                tags$ol(
                  tags$li("A plot of the theoretical quantiles versus sample quantiles (QQ-plot) can be used to compare observed values to a theoretical distribution. The ordered quantiles of the observed residuals are plotted against the quantiles of the standard normal distribution."),
                  tags$li("A histogram of the residuals can also be used to visualise the distribution, but caution needs to be taken in small sample sizes as the plot may be inconclusive.")
                )
                ),
                br(),
                tags$h4("Acceptable appearance of residual plots"),
                br(),
                fluidRow(
                  column(1, ),
                  column(
                    5, img(src = "qqsample.png", height = 450, align = "center"),
                    br(), br(),
                    tags$p("The ordered residuals are plotted against theoretical expected values for a standard normal sample. To meet the normality assumption, the residuals should follow the diagonal straight-line (which represents the normal distribution) without devaiting from the confidence interval bound (grey).")
                  ),
                  column(
                    5, img(src = "histsample.png", height = 450, align = "center"),
                    br(), br(),
                    tags$p("To provide a decent indicator of normality of the residuals, the histogram should have some symmetry and a bell-shape. The main objective is to avoid seeing histograms that are very irregularly shaped (e.g., heavily skewed).")
                  )
                ),
                # br(),
                br(),
                uiOutput("normalityUI"),
                br(),
                br(),
                tags$h4("Remedies"),
                tags$p(em("Approach 1: Transformations")),
                tags$p("Data transformation is one strategy to solve the problem of non-normality. It involves adapting the data to the model by altering the outcome or predictor variable’s distribution. We perform data transformations for three main reasons:"),
                tags$ol(
                  tags$li("normalise the residuals"),
                  tags$li("stabilise variance of the outcome"),
                  tags$li("linearise the regression model")
                ),
                tags$p("A well-chosen transformation can help satisfy these concerns; in some instances, the same transformation often helps accomplish the first two goals. Interpretation of the model depends on the transformed variable (s). In practice, the commonly used transformations on the outcome are:"),
                tags$ol(
                  tags$li("The log transformation: (a) to stabilise the variance if it increases markedly with increasing Y; (b) to normalise a positively skewed distribution of the residuals; and (c) to linearise the regression model if the relationship of Y to some predictor variable suggests a model with consistently increasing slope (e.g., an exponential relationship)."),
                  tags$li("The square transformation: (a) to stabilise the variance if it decreases with the mean of Y; (b) to normalise a negatively skewed distribution of the residuals; and (c) to linearise the model if the original relationship with some predictor variable is curvilinear downward (i.e. if the slope consistently decreases as the predictor variable increases)."),
                  tags$li("The square root transformation: (a) stabilises the variance if it is proportional to the mean of Y. This is particularly appropriate if the dependent variable has the Poisson distribution (i.e., count data).")
                ),
                tags$p("Another common approach is to use Box-Cox transformations, a family of power transformations. The procedure uses the maximum likelihood method to find the optimal power transformation for some variables. More information on these transformations can be found in the", strong("Manual"), "tab."),
                br(),
                tags$p(em("Approach 2: Generalized linear models")),
                tags$p("When the model assumptions are violated even after applying transformations, this implies a multiple linear regression model poorly describes the data. The solution is to adapt the regression model to the data and model the non-normality. The generalised linear model is a generalisation of the basic regression model that makes it possible to relax the normality assumption and assume other error distributions instead. Logistic regression (binomial and multinomial data) and Poisson regression (count data) are good options."),
                br(),
                tags$p(em("Approach 3: Addition of omitted discrete variables and deletion of outliers")),
                tags$p("A multimodal (i.e. more than one peak) error distribution implies that the model omitted one or more discrete predictor variables that naturally divide the data into groups. Adding these variables can help normalise the distribution of the residuals."),
                tags$p("Checking for outliers using Cook’s Distance (an estimate of the influence of a data point) and deleting them can help normalise the distribution of the residuals and stabilise their variance."),
                br(),
                br()
              ), # Close Normality tabPanel
              tabPanel(
                tags$p("Homoscedasticity Assumption", style = "color:#003399"),
                value = "mpanel5",
                tags$h3("Homoscedasticity and consequences of heteroscedasticity"),
                checkboxInput("show_HS_info", "Show background information", value = FALSE),
                conditionalPanel(
                  condition = "input.show_HS_info == true",
                tags$p("Homoscedasticity is an assumption of the linear regression model that states the variance of the residuals (or errors) should be constant for all levels of the predictor variables. When this assumption is violated, the residual variance differs at various levels of the predictor variables, which is known as heteroscedasticity."),
                tags$p("If the assumption of homoscedasticity is violated, the model's estimates will remain unbiased, but they will be inefficient (i.e., they will have a larger variance than necessary). Furthermore, the standard errors of the coefficients will be incorrect, leading to unreliable confidence intervals and significance tests, ultimately affecting the validity of any conclusions drawn from the model."),
                br(),
                tags$h4("How to assess the homoscedasticity assumption?"),
                tags$p(em("Graphical methods that can be used")),
                tags$ol(
                  tags$li("A residuals vs. fitted values plot."),
                  tags$li("Scale-location plot (also known as spread-location plot). It plots the square root of the absolute standardized residuals against the fitted values.")
                )
                ),
                br(),
                tags$h4("Acceptable appearance of residual plots"),
                br(),
                fluidRow(
                  column(1, ),
                  column(
                    5, img(src = "rvpsample.png", height = 450, align = "center"),
                    br(), br(),
                    tags$p("In a residuals vs. fitted values plot, homoscedasticity is evident if the residuals are randomly scattered around the horizontal line at zero without any systematic pattern. If the spread of residuals increases or decreases across levels of fitted values, this suggests heteroscedasticity.")
                  ),
                  column(
                    5, img(src = "scalesample.png", height = 450, align = "center"),
                    br(), br(),
                    tags$p("In a scale-location plot, the homoscedasticity assumption holds if the points are scattered randomly around a horizontal line. If the points exhibit a pattern or trend (such as a funnel shape), heteroscedasticity is likely present.")
                  )
                ),
                br(),
                uiOutput("homoscedasticityUI"),
                br(),
                br(),
                tags$h4("Remedies"),
                tags$p(em("Approach 1: Transformation of variables")),
                tags$p("One of the most common ways to address heteroscedasticity is to transform the dependent variable. For example, applying a logarithmic, square root, or inverse transformation can help stabilize the variance."),
                withMathJax(
                  tags$p("For example, if the residual variance increases with the magnitude of \\(Y\\), taking the logarithm of \\(Y\\) can reduce this issue. The model becomes \\(\\log(Y) = β_0 + β_1 X + \\varepsilon\\).")
                ),
                br(),
                tags$p(em("Approach 2: Weighted least squares")),
                tags$p("Another approach is to use weighted least squares (WLS), which assigns a weight to each data point based on the inverse of the variance of its residual. This technique reduces the impact of data points with higher variance, resulting in a model with more constant variance."),
                br(),
                tags$p(strong("Note: Both transformations and WLS address the heteroscedasticity issue, but they should be applied with caution as they can also alter the interpretation of the model coefficients.")),
                br(),
                br()
              ),
              # Download Report --------------------------------------------
              tabPanel(
                tags$p("Download Report", style = "color:#003399"),
                value = "mpanel6",
                # INPUT
                sidebarLayout(
                  sidebarPanel(
                    tags$h4("Generate dyanamic report"),
                    tags$p("After running your analysis you can download the output to your own computer."),
                    tags$p(em("Please select the desired output format:")),
                    radioButtons("format", NULL, c("PDF", "Word", "HTML"), inline = TRUE),
                    downloadButton("downloadReport")
                  ), # Close sidebarPanel

                  # OUTPUT
                  mainPanel(
                    tags$h3("R Script"),
                    tags$p("You will be able to re-run this analysis in R by copying the R codes on the plots."),
                    tags$p("It is highly recommended to run the codes in RStudio to be able to edit the script, view and interact with the objects stored in your environment during your analysis."),
                    br()
                    # downloadLink("downloadScript", "Download R Script"),
                    # verbatimTextOutput("code")
                  ) # Close mainPanel
                ) # Close sidebarLayout
              ) # Close Download Report tabePanel
            ) # Close tabsetPanel in mainPanel for Model tab
          ) # Close mainPanel for Model tab
        ) # Close sidebarLayout
      ) # Close Model fluidPage
    ), # Close the Model tabPanel

    ############################################################################
    # Open the Manual tabPanel
    ##########################
    tabPanel(
      tags$p("Manual", style = "color:#FF6600"),
      fluidPage(
        img(src = "logoVUB.png", height = 50, align = "right"),
        titlePanel(h3("Documentation", style = "color:#003399")),
        tags$hr(),
        tags$h3("Steps on how to use the app"),
        br(),
        tags$h4("The app has input tabs on the", em("sidebar panel"), "and outputs are displayed on the", em("main panel."), style = "color:#FF6600"),
        br(),
        tags$h4("Step 1: Data input"),
        tags$p("Select an example dataset or load your data file by choosing the correct file extension in the", strong("Data Input"), "sidebar tab. To change a data type, select a variable to edit, choose the ", em("New data type"), "and apply changes using the ", em("Change data type "), "button."),
        tags$p("The", strong("View Data"), "tab shows a preview of the data, and by default, only 10 rows of data are shown at a time. You can change this setting through the", em("Show entries"), "dropdown. The", strong("Data Summary"), "tab displays descriptive statistics of the data including the distributions of variables."),
        br(),
        tags$h4("Step 2: Define the model"),
        tags$p("The", strong("Define Model"), "sidebar tab allows users to select the outcome variable and one or more predictor variables. If the model contains interaction terms, one can create and add them to the model by marking the checkbox. The model is then run by clicking the ", em("Run Analysis"), "button and viewing the fitted regression model results under the ", strong("Model Summary"), "tab."),
        br(),
        tags$h4("Step 3: Model diagnostics"),
        tags$p("The", strong("Linearity Assumption"),",", strong("Normality Assumption"), "and", strong("Homoscedasticity Assumption"), "tabs provide visualisations and diagnostics plots to validate the regression model."),
        tags$ol(
          tags$li("The", em("'observed vs fitted values'"), "scatter plot and the", em("'residuals vs fitted values'"), "plot assess the linearity assumption. Both illustrate the relationship between the outcome and the predictors."),
          tags$li("The QQ-plot and the histogram of residuals provide visualisations to assess the normality assumption."),
          tags$li("The Scale-Location plot of residuals provide additional visualisations to assess the homoscedasticity assumption.")
        ),
        tags$p("Recommendations for the ideal plots are provided together with suggestions for remedies if there are violations."),
        br(),
        tags$h4("Step 4: Data transformation"),
        tags$p("After assessing the plots, the next step is to make any data transformations necessary. If there were no violations, skip to Step 6."),
        tags$p("Switch to the", strong("View Data"), "tab by clicking on the", strong("Transform"), "sidebar tab."),
        tags$p("Select a variable to transform from the drop-down menu and choose a transformation type from the list. Type in the extension to the transformed variable before clicking", em("Apply Changes."), "Data will automatically update with the transformed variable in the last column. It is possible to save the updated dataset by choosing a file extension and clicking the", em("Download"), "button."),
        br(),
        tags$p("Below, we describe the transformation functions:"),
        tags$ol(
          withMathJax(),
          tags$li(em("Ln (natural log):"), "takes the natural logarithm of the variable, which helps reduce skewness and makes data more normally distributed."),
          tags$li(em("Ln (X+1):"), " takes the natural logarithm of (X+1), used for data containing zeros to avoid undefined values."),
          tags$li(em("Exp:"), " calculates the exponential function of the variable, which helps reverse the natural logarithm transformation."),
          tags$li(em("Square:"), " raises the variable to the power of 2, often used to capture quadratic relationships in data."),
          tags$li(em("Cube:"), " cubes the variable, capturing cubic relationships in data."),
          tags$li(em("Square root:"), " takes the square root of the variable, helpful for stabilising variance and reducing skewness."),
          tags$li(em("Standardise:"), " centres the variable around its mean and scales it to have a standard deviation of 1, ensuring all variables are on the same scale."),
          tags$li(em("Centre:"), " shifts the variables' values to have a mean of zero, useful for comparing variables on different scales"),
          tags$li(em("Inverse:"), " takes the reciprocal of the variable, useful for transforming ratios or proportions back to their original scale."),
        ),
        br(),
        tags$em("Box-Cox Transformation"),
        tags$p("When users tick the Box-Cox transformation checkbox, a slider pops up. Below the data preview, a QQ plot shows how the residuals change as users alter the slider inputs, representing the new power of the outcome variable. The partial log-likelihood shows the estimated lambda and its 95% confidence interval. If this estimated lambda (Est_lambda), from the slider input, is close to one of the Exact_lambda values in the table below, use the latter value as it is easier to interpret."),
        tableOutput("kablebox"),
        br(),
        tags$h4("Step 5: Reassemble the model"),
        tags$p("After data transformation, return to Step 2 and rerun the analysis with the transformed variables."),
        br(),
        tags$h4("Step 6: Generate a report"),
        tags$p("Users can generate an analysis report in the ", strong("Download Report"), "tab. Reports can either be in PDF, Word or HTML format."),
        tags$p("There is also an option to save the R Code for the plots generated from the study to reproduce the analysis or recreate the results. Users can then edit the code and interact with the objects stored in your environment during your analysis in RStudio."),
        br(),
        tags$h4("Note"),
        tags$p("At any stage of the analysis, users can refresh and start a new session by clicking the", strong("Reset"), "button."),
        br(),
        br(),
        br(),
        br(),
        br()
        # )
        # )
      )
    ), # Close the Manual tabPanel

    ############################################################################
    # Open the Contact tabpanel
    ###########################
    tabPanel(
      tags$p("Contact us", style = "color:#FF6600"),
      fluidPage(
        img(src = "logo_bisi_rgb.jpg", height = 60, align = "right"),
        titlePanel(h3("Developers", style = "color:#003399")),
        hr(),
        # h3("Perseverence Savieri"),
        fluidRow(
          column(2,
            img(src = "HR_BIBI_Percy2.jpg", height = 200),
            br(), br(),
            style = "text-align: center;"
          ),
          column(
            8,
            br(), br(),
            tags$p("Perseverence Savieri is a doctoral researcher in the", tags$a(href = "https://bisi.research.vub.be/", "Biostatistics and Medical Informatics research group (BISI)"), "at the Vrije Universiteit Brussel medical campus Jette. He is also a statistical consultant for the humanities and social sciences at campus Etterbeek through the", tags$a(href = "https://square.research.vub.be/", "Support for Quantitative and Qualitative Research (SQUARE)"), "core facility. Here, he offers statistical and methodological quantitative support in the form of consultations, statistical coaching, data analyses and workshops.", style = "text-align: center;"),
            h5(tags$a(href = "mailto:perseverence.savieri@vub.be", "Perseverence.Savieri@vub.be"), style = "text-align: center;"),
            br(),
            h4("Supervisors", style = "text-align: center;"),
            h5("dr. Lara Stas & Prof. dr. Kurt Barbe", style = "text-align: center;")
            # Kurt Barb\xe9
          )
        )
      )
    ),
    position = "fixed-top", tags$style(type = "text/css", "body {padding-top: 70px;}") # Close the Contact tabPanel
  ) # Close navbarPage for main headings
) # Close fluidPage

################################################################################
# Load datasets and expressions used in the server
##################################################
load("occupancy.RData")
load("labdata_aged.RData")
load("boxcox.RData")

input2txt <- function(x) {
  if (!is.null(x$added)) {
    res <- sprintf("Outcome variable: %s
Predictor variable(s): %s", x$outcome, paste0((paste(x$predictor, collapse = " + ")), " + ", (paste(x$added, collapse = " + "))))
    return(res)
  } else {
    res1 <- sprintf("Outcome variable: %s
Predictor variable(s): %s", x$outcome, paste(x$predictor, collapse = " + "))
    return(res1)
  }
}


################################################################################
# Open server
######################
server <- function(input, output, session) {
  # INPUT ----------------------------------------------------------------------
  # Inner Tab 2:  Reactive Data Input
  data_input <- reactive({
    inFile <- input$file1
    exdata <- input$exdata

    # req(c(input$exdata, input$file1))
    # Example dataset
    if (is.null(inFile)) {
      if (exdata == "") {
        return(NULL)
      } else if (exdata == "CS1: Open field test") {
        return(as.data.frame(labdata_aged))
      } else if (exdata == "CS2: Occupancy") {
        return(as.data.frame(occupancy))
      }
    }

    # Load own dataset
    if (!is.null(inFile)) {
      if (input$ext == "txt") {
        return(read.csv(inFile$datapath, sep = "", header = T))
      } else if (input$ext == "csv") {
        return(read.csv(inFile$datapath, header = T))
      } else if (input$ext == "xlsx") {
        return(as.data.frame(read_excel(inFile$datapath, sheet = 1)))
      } else if (input$ext == "sav") {
        # return(read.spss(inFile$datapath, to.data.frame = T, use.value.labels = F))
        return(as.data.frame(read_sav(inFile$datapath)))
      } else if (input$ext == "dta") {
        # return(read.dta(inFile$datapath))
        return(as.data.frame(read_dta(inFile$datapath)))
      }
    }
  })

  # Reset button ---------------------------------------------------------------
  output$reload <- renderUI({
    if (input$newanalysis > 0) {
      tags$script("window.location.reload();")
    }
  })

  # Update main panel tabs when sidebar tabs are selected ----------------------
  observeEvent(input$sidetabs, {
    if (input$sidetabs == "spanel2") {
      updateTabsetPanel(session, "maintabs",
        selected = "mpanel2"
      )
    } else if (input$sidetabs == "spanel3") {
      updateTabsetPanel(session, "maintabs",
        selected = "mpanel1"
      )
    } else if (input$sidetabs == "spanel1") {
      updateTabsetPanel(session, "maintabs",
        selected = "mpanel1"
      )
    }
  })

  # Update variables and change data types -------------------------------------
  dataset <- reactiveVal(NULL)

  # Read data
  observeEvent(data_input(), {
    dataset(data_input())
  })

  output$edit_vars <- renderUI({
    req(dataset())
    var_names <- colnames(dataset())
    tagList(
      selectInput(
        inputId = "select_class_vars", label = strong("Select variable to edit"),
        choices = c(" ", c(var_names)), multiple = FALSE
      ),
      selectizeInput("new_data_type", label = strong("New data type"), choices = c("", "Factor", "Numeric", "Integer", "Character", "Date (ddmmyy)")),
      actionButton("change_class", "Change data type")
    )
  })

  observeEvent(input$change_class, {
    d <- dataset()

    d <- d %>%
      mutate(!!input$select_class_vars := switch(input$new_data_type,
        "Factor" = as.factor(!!sym(input$select_class_vars)),
        "Numeric" = as.numeric(!!sym(input$select_class_vars)),
        "Integer" = as.integer(!!sym(input$select_class_vars)),
        "Character" = as.character(!!sym(input$select_class_vars)),
        "Date (dmy)" = as.Date(!!sym(input$select_class_vars), format = "%d/%m/%Y")
      ))

    dataset(d)
  })

  # Transform data -------------------------------------------------------------
  output$data_management <- renderUI({
    req(dataset())
    var_names <- colnames(dataset())
    tagList(
      selectInput(inputId = "tvar", label = strong("Select variable to transform"), choices = c(" ", c(var_names))),
      selectizeInput("transform_type", label = strong("Transformation type"), choices = c("", "Ln (natural log)", "Ln (X+1)", "Exp", "Square", "Cube", "Square root", "Standardize", "Centre", "Inverse")),
      textInput("var_name_extension", label = strong("Variable name extension"), placeholder = "Optional"),
      actionButton("apply_changes", "Apply Changes")
    )
  })

  observeEvent(input$apply_changes, {
    req(input$transform_type)
    new_data <- dataset()

    transform_func <- switch(input$transform_type,
      "Ln (natural log)" = log,
      "Ln (X+1)" = function(x) log(x + 1),
      "Exp" = exp,
      "Square" = function(x) x^2,
      "Cube" = function(x) x^3,
      "Square root" = sqrt,
      "Standardize" = scale,
      "Centre" = function(x) x - mean(x),
      "Inverse" = function(x) 1 / x
    )

    new_var <- paste(input$tvar, input$var_name_extension, sep = "_")

    new_data <- new_data %>%
      mutate(!!new_var := transform_func(!!sym(input$tvar)))

    dataset(new_data)
  })

  # Save transformed dataset ---------------------------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("Updated", "_Table", input$downloadType)
    },
    content = function(file) {
      if (input$downloadType == ".csv") {
        write.csv(dataset(), file, row.names = FALSE)
      } else if (input$downloadType == ".xlsx") {
        write.xlsx(dataset(), file,
          sheetName = "Sheet1", row.names = FALSE
        )
      } else if (input$downloadType == ".tsv") {
        write.table(dataset(), file,
          quote = FALSE,
          sep = "\t", row.names = FALSE
        )
      }
    }
  )

  # Dynamic UI for the linearity & normality tabs ------------------------------
  output$linearityUI <- renderUI({
    req(dataset())
    tagList(
      tags$h4("Using your own data: Residual plots from your model"),
      fluidRow(
        column(1, ),
        column(
          5, outputCodeButton(plotOutput("ovfplot")),
          tags$p("1. Scatter plot between the observed and fitted values of the outcome with the linear regression line (blue) and the loess curve (red).", align = "center"),
          downloadButton("dnldovf", label = "")
        ),
        column(
          5, outputCodeButton(plotOutput("rvfplot")),
          tags$p("2. Plot of residuals against fitted values of the outcome with the zero-line (blue) and the loess curve (red).", align = "center"),
          downloadButton("dnldrvf", label = "")
        )
      )
    )
  })

  output$normalityUI <- renderUI({
    req(dataset())
    tagList(
      tags$h4("Using your own data: Residual plots from your model"),
      fluidRow(
        column(1, ),
        column(
          5, outputCodeButton(plotOutput("qqplot")),
          tags$p("1. Normal probability plot of the residuals.", align = "center"),
          downloadButton("dnldqq", label = "")
        ),
        column(
          5, outputCodeButton(plotOutput("histplot")),
          tags$p("2. Histogram of the residuals.", align = "center"),
          downloadButton("dnldhist", label = "")
        )
      ),
      br(),
      tags$h4("Formal tests for normality and equal variances"),
      fluidRow(
        column(
          4, tags$p("Shapiro-Wilk Test"),
          textOutput("shapiro")
        ),
        column(
          4, tags$p("Breusch-Pagan Test"),
          textOutput("breusch")
        )
      ),
      br(),
      br(),
      tags$p(strong("Note:"), "If the p-value is less than 0.05 in the test, we reject the null hypotheses.", em("Interpretation:"), "(1) In the Shapiro-Wilk test for normality, the residuals are not normally distributed and (2) in the Breusch-Pagan test for equal variances, the residual do not have constant variance.")
    )
  })

  output$homoscedasticityUI <- renderUI({
    req(dataset())
    tagList(
      tags$h4("Using your own data: Residual plots from your model"),
      fluidRow(
        column(1, ),
        column(
          5, #plotOutput("rvpplot")
          outputCodeButton(plotOutput("rvpplot")),
          tags$p("1. Plot of residuals against fitted values of the outcome with the zero-line (blue) and the loess curve (red).", align = "center"),
          downloadButton("dnldrvp", label = "")
        ),
        column(
          5, #plotOutput("scalelocplot")
          outputCodeButton(plotOutput("scalelocplot")),
          tags$p("2. Scatter plot between the observed and fitted values of the outcome with the linear regression line (blue) and the loess curve (red).", align = "center"),
          downloadButton("dnldscl", label = "")
        )
      )
    )
  })


  # Box cox transformation UI --------------------------------------------------
  output$powerUI <- renderUI({
    req(input$qqcox == 1)
    tagList(
      h5("QQ-plot of residuals after applying Box-Cox transformation"),
      plotOutput("qqtrans")
    )
  })

  output$boxcoxUI <- renderUI({
    req(input$qqcox == 1)
    tagList(
      h5("Profile Log-likelihood"),
      plotOutput("qqloglike")
    )
  })

  # OUTPUTS --------------------------------------------------------------------
  # Data Table -----------------------------------------------------------------

  # data <- reactive({
  #   if (is.null(input$change_class & input$apply_changes)) {
  #     return(dataset(n))
  #   } else {
  #     return(dataset(new_data))
  #   }
  # })

  output$mytable1 <- renderDataTable({
    validate(need(dataset(), "Please, upload own dataset or select one from the examples."))
    m <- dataset()
    # m <- updated_dset()
    dprint <- format(m, digits = 3)
    dprint
  })

  # Data summary using dfSummary() from summarytools
  output$data_summ <- renderUI({
    req(dataset())
    print(
      dfSummary(dataset(),
        graph = TRUE, valid.col = FALSE, graph.magnif = 0.75,
        style = "grid"
      ),
      max.tbl.height = 800, method = "render",
      headings = FALSE, bootstrap.css = FALSE
    )
  })

  # Inner Tab 3: Define Model --------------------------------------------------
  observe({
    value <- names(dataset())
    updateSelectizeInput(session, "outcome", choices = c(" ", value), server = TRUE)
    updateSelectizeInput(session, "predictor", choices = c(" ", value), server = TRUE)
    updateSelectizeInput(session, "interaction", choices = c(" ", value), server = TRUE)
  })

  # Create list of interaction terms --------------------------------------------
  listN <- reactiveValues()
  makeReactiveBinding("listN")

  # Rendering the list to the ui
  output$uiAdded <- renderUI({
    checkboxGroupInput("added", "",
      choices = names(listN)
    )
    # multiple = TRUE
    # selectize = FALSE)
  })

  observe({
    # Trigger Add actions
    input$actionBtnAdd
    isolate({
      new_selections <- c(input$interaction)
      new_selections_name <- new_selections %>% paste(collapse = "*")

      if (new_selections_name != "") {
        listN[[new_selections_name]] <- new_selections
      }
    })
  })

  # Run the regression model ---------------------------------------------------
  regModel <- metaReactive2({
    req(input$runmodel)

    isolate({
      predictors <- paste(input$predictor, collapse = "+")

      if (!is.null(input$added)) {
        metaExpr({
          lm(as.formula(paste(..(input$outcome), " ~ ", ..(predictors), paste(" + ", ..(input$added), collapse = " + "))), data = dataset())
        })
      } else {
        metaExpr({
          lm(as.formula(sprintf("%s ~ %s", ..(input$outcome), ..(predictors))), data = dataset())
        })
      }
    })
  })


  # Generate a summary of the regression model ---------------------------------
  output$table <- renderUI({
    # req(input$runmodel, input$predictor, input$outcome)
    tab <- tab_model(regModel())
    HTML(tab$knitr)
  })

  # Save formal test results
  output$shapiro <- renderText({
    validate(need(nrow(dataset()) < 5000, "Sample size must be between 3 and 5000."))
    paste("p-value = ", round(shapiro.test(regModel()$residuals)$p.value, 3))
  })

  output$breusch <- renderText({
    paste("p-value = ", round(bptest(regModel())$p.value, 3))
  })

  # Box-Cox transformations ----------------------------------------------------
  tsmodel <- reactive({
    if (input$lambda == 0) {
      update(regModel(), log(.) ~ ., data = dataset())
    } else {
      update(regModel(), (.^input$lambda - 1) / input$lambda ~ ., data = dataset())
    }
  })

  # Transfomed QQ-plot
  output$qqtrans <- renderPlot({
    # req(input$runmodel)
    validate(need(input$runmodel, "Please, fit the model to obtain QQ plot."))
    tsmodel() %>%
      ggplot(aes(sample = .resid)) +
      stat_qq_line() +
      stat_qq_band() +
      stat_qq_point() +
      xlab("Theoretical Quantiles") +
      ylab("Sample Quantiles") +
      theme_bw()
  })

  # Partial log-likelihood
  output$qqloglike <- renderPlot({
    # req(input$runmodel)
    # dataset()[[input$outcome]]
    validate(need(input$runmodel, "Please, fit the model to view residual plot"))
    validate(need(dataset()[[input$outcome]], "Please, refit the model to obtain log-likehood."))
    d <- dataset()
    predictors <- paste(input$predictor, collapse = "+")
    if (!is.null(input$added)) {
      boxcox(as.formula(paste(input$outcome, " ~ ", predictors, paste(" + ", input$added, collapse = " + "))), lambda = seq(-2, 5, 1 / 4), data = d)
    } else {
      boxcox(as.formula(sprintf("%s ~ %s", input$outcome, predictors)), lambda = seq(-2, 5, 1 / 4), data = d)
    }
  })

  # Box-Cox help file
  output$kablebox <- function() {
    dt <- boxcox
    dt %>%
      kable("html") %>%
      kable_styling("striped", full_width = F)
  }


  # Inner Tab 4: Model Diagnostics ---------------------------------------------
  # Observed vs fitted values
  ovf_plot <- metaReactive2({
    validate(need(dataset()[[input$outcome]], "Please, fit model or update observed values with tranformed data."))
    metaExpr({
      ggplot(dataset(), aes(predict(..(regModel())), dataset()[[..(input$outcome)]])) +
        geom_point() +
        stat_smooth(method = "lm", formula = y ~ x) +
        stat_smooth(method = "loess", formula = y ~ x, se = F, col = "red") +
        xlab(paste("Fitted values for", ..(input$outcome))) +
        ylab(paste("Observed values for", ..(input$outcome))) +
        theme_bw()
    })
  })

  output$ovfplot <- metaRender(renderPlot,
    {
      ..(ovf_plot())
    },
    res = 96
  )

  observeEvent(input$ovfplot_output_code, {
    code <- expandChain(quote(library(ggplot2)), output$ovfplot())
    displayCodeModal(code)
  })

  # Residuals vs fitted values
  rvf_plot <- metaReactive({
    ..(regModel()) %>%
      ggplot(aes(.fitted, .resid)) +
      geom_point() +
      stat_smooth(method = "loess", formula = y ~ x, col = "red") +
      geom_hline(yintercept = 0, col = "blue", linetype = "dashed") +
      # ggtitle("Residual vs Fitted Plot") +
      xlab(paste("Fitted values for", ..(input$outcome))) +
      ylab("Residuals") +
      theme_bw()
  })

  output$rvfplot <- metaRender(renderPlot,
    {
      # req(input$predictor)
      ..(rvf_plot())
    },
    res = 96
  )

  observeEvent(input$rvfplot_output_code, {
    code <- expandChain(quote(library(dplyr)), quote(library(ggplot2)), output$rvfplot())
    displayCodeModal(code)
  })

  # crPlots for multpiple linear regression
  output$crplot <- metaRender2(renderPlot,
    {
      ind.vars <- c(input$predictor)
      validate(
        need(length(ind.vars) > 1, "Partial residual plots are only displayed for multiple linear regression."),
        need(is.null(input$added), "C+R plots not available for models with interactions.")
      )
      metaExpr({
        crPlots(..(regModel()), main = "")
      })
    },
    res = 96,
    width = 1000,
    height = 750
  )

  observeEvent(input$crplot_output_code, {
    code <- expandChain(quote(library(ggplot2)), quote(library(car)), output$crplot())
    displayCodeModal(code)
  })

  # Normal QQ plot
  qq_plot <- metaReactive({
    ..(regModel()) %>%
      ggplot(aes(sample = .resid)) +
      stat_qq_line() +
      stat_qq_band() +
      stat_qq_point() +
      # ggtitle("Normal Q-Q Plot") +
      xlab("Theoretical Quantiles") +
      ylab("Sample Quantiles") +
      theme_bw()
  })

  output$qqplot <- metaRender(renderPlot,
    {
      # req(input$predictor)
      ..(qq_plot())
    },
    res = 96
  )

  observeEvent(input$qqplot_output_code, {
    code <- expandChain(quote(library(dplyr)), quote(library(ggplot2)), output$qqplot())
    displayCodeModal(code)
  })

  # Histogram of residuals
  hist_plot <- metaReactive({
    ..(regModel()) %>%
      ggplot(aes(.resid)) +
      geom_histogram(bins = 10, aes(y = after_stat(density)), color = "steelblue4", fill = "grey80") +
      geom_density(kernel = "gaussian") +
      # ggtitle("Histogram of Residuals") +
      xlab("Residuals") +
      ylab("Density") +
      theme_bw()
  })

  output$histplot <- metaRender(renderPlot,
    {
      # req(input$predictor)
      ..(hist_plot())
    },
    res = 96
  )

  observeEvent(input$histplot_output_code, {
    code <- expandChain(quote(library(dplyr)), quote(library(ggplot2)), output$histplot())
    displayCodeModal(code)
  })

  # Residuals vs fitted values
  rvp_plot <- metaReactive({
    ..(regModel()) %>%
      ggplot(aes(.fitted, .resid)) +
      geom_point() +
      stat_smooth(method = "loess", formula = y ~ x, col = "red") +
      geom_hline(yintercept = 0, col = "blue", linetype = "dashed") +
      # ggtitle("Residual vs Fitted Plot") +
      xlab(paste("Fitted values for", ..(input$outcome))) +
      ylab("Residuals") +
      theme_bw()
  })

  output$rvpplot <- metaRender(renderPlot,
    {
      # req(input$predictor)
      ..(rvp_plot())
    },
    res = 96
  )

  observeEvent(input$rvpplot_output_code, {
    code <- expandChain(quote(library(dplyr)), quote(library(ggplot2)), output$rvpplot())
    displayCodeModal(code)
  })

  # Scale-Location plot (Spread-Location plot)
  scaleloc_plot <- metaReactive({
    ..(regModel()) %>%
      ggplot(aes(.fitted, sqrt(abs(.stdresid)))) +
      geom_point() +
      stat_smooth(method = "loess", formula = y ~ x, col = "red") +
      # geom_hline(yintercept = 0, col = "blue", linetype = "dashed") +
      geom_hline(yintercept = 0.822179) +
      xlab(paste("Fitted values for", ..(input$outcome))) +
      ylab(expression(sqrt("|Standardised Residuals|"))) +
      theme_bw()
  })

  # Render the Scale-Location plot in the app
  output$scalelocplot <- metaRender(renderPlot,
    {
      ..(scaleloc_plot())
    },
    res = 96
  )

  # Display code for the Scale-Location plot in the modal
  observeEvent(input$scalelocplot_output_code, {
    code <- expandChain(quote(library(dplyr)), quote(library(ggplot2)), output$scalelocplot())
    displayCodeModal(code)
  })


  ## Variables in the model
  output$input <- renderPrint({
    req(input$predictor)

    cat(input2txt(input))
  })

  ## Download plots

  output$dnldovf <- downloadHandler(
    filename = function() {
      paste("output$ovfplot", ".png")
    },
    content = function(file) {
      ggsave(file, ovf_plot())
    }
  )

  output$dnldrvf <- downloadHandler(
    filename = function() {
      paste("output$rvfplot", ".png")
    },
    content = function(file) {
      ggsave(file, rvf_plot())
    }
  )

  output$dnldqq <- downloadHandler(
    filename = function() {
      paste("output$qqplot", ".png")
    },
    content = function(file) {
      ggsave(file, qq_plot())
    }
  )

  output$dnldhist <- downloadHandler(
    filename = function() {
      paste("output$histplot", ".png")
    },
    content = function(file) {
      ggsave(file, hist_plot())
    }
  )

  output$dnldrvp <- downloadHandler(
    filename = function() {
      paste("output$rvpplot", ".png")
    },
    content = function(file) {
      ggsave(file, rvp_plot())
    }
  )

  output$dnldscl <- downloadHandler(
    filename = function() {
      paste("output$scalelocplot", ".png")
    },
    content = function(file) {
      ggsave(file, scaleloc_plot())
    }
  )

  # Inner Tab 5: Download Report -----------------------------------------

  # Download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".", switch(input$format,
        PDF = "pdf",
        HTML = "html",
        Word = "docx"
      ))
    },
    content = function(file) {
      src <- normalizePath("report.Rmd")

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)

      library(rmarkdown)
      out <- render("report.Rmd", switch(input$format,
        PDF = pdf_document(),
        HTML = html_document(),
        Word = word_document()
      ))
      file.rename(out, file)
    }
  )
} # Close server


# Run the application
shinyApp(ui = ui, server = server)