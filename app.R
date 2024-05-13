library(shiny)
library(Mcomp)
library(forecast)
library(tseries)
library(tidyverse)
library(shinydashboard)
library(DT)
library(kableExtra)
library(readxl)
library(lubridate)
library(writexl)

ui <- dashboardPage(
  dashboardHeader(title = "Modeling Time Series",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "João Pupe (Author)",
                                 message = "My LinkedIn to ask questions. Just click!",
                                 icon = icon("linkedin"),
                                 href = "https://www.linkedin.com/in/joao-pedro-moreira-pupe/"
                               )
                               )),
  dashboardSidebar(sidebarMenu(
    textOutput("inputs_area"),
    radioButtons("opt_inputs","I want to analyze",
                 choices = c("AirPassengers time serie","A serie from my computer"),
                 selected = "AirPassengers time serie"),
    fileInput('file1', 'Upload xlsx file',
              accept = c(".xlsx")),
    numericInput("tsfreq",
                 "Serie frequency:",
                 min = 1,
                 max = 12,
                 value = 12),
    numericInput("tsh",
                 "Number of forecast horizons:",
                 min = 5,
                 max = 50,
                 value = 12),
    numericInput("tshsw",
                 "Number of horizons for the sliding window study:",
                 min = 5,
                 max = 15,
                 value = 12),
    sliderInput("tshswinitial",
                 "% of your time serie to start the sliding window study:",
                 min = 25,
                 max = 100,
                 value = 33,step=1,ticks = FALSE),
    actionButton("calculatebtn","Analyze selected series")
  )),
  dashboardBody(
        tabsetPanel(
          tabPanel("Guide",
                   HTML('
                   textOutput("text_xlsx_default"),
                   downloadButton("download_xlsx","Download"),
                   textOutput("text_xlsx_default2"),
                   HTML('<img alt="img_xlsx_default.png" src="https://github.com/jpupe/ce2py/blob/main/img_xlsx_default.png?raw=true" data-hpc="true" class="Box-sc-g0xbh4-0 kzRgrI">')
                   ),
          tabPanel("Serie + MSTL",
             box(skin = "blue",
                 status = 'primary',
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 title = "Serie",
                 plotOutput("tsPlot"),
                 width= 6),
             box(skin = "blue",
                 status = 'primary',
                 solidHeader = FALSE,
                 collapsible = TRUE ,
                 title = "MSTL Decomposition",
                 plotOutput("MSTLPlot"),
                 width= 6)
             ),
        tabPanel("auto.arima()",
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Selected Model",
                     textOutput("arima_text1"),
                     tableOutput("arima_fit_parameter"),
                     width= 12),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Residuals",
                     plotOutput("arima_residuals"),
                     textOutput("arima_text2"),
                     tableOutput("arima_residualstests"),
                     width= 6),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Forecasting",
                     plotOutput("arima_forecast"),
                     plotOutput("arima_forecast_test"),
                     width= 6)
                 ),
        tabPanel("ses()",
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Selected Model",
                     textOutput("ses_text1"),
                     tableOutput("ses_fit_parameter"),
                     width= 12),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Residuals",
                     plotOutput("ses_residuals"),
                     textOutput("ses_text2"),
                     tableOutput("ses_residualstests"),
                     width= 6),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Forecasting",
                     plotOutput("ses_forecast"),
                     plotOutput("ses_forecast_test"),
                     width= 6)
                 ),
        tabPanel("holt()",
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Selected Model",
                     textOutput("holt_text1"),
                     tableOutput("holt_fit_parameter"),
                     width= 12),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Residuals",
                     plotOutput("holt_residuals"),
                     textOutput("holt_text2"),
                     tableOutput("holt_residualstests"),
                     width= 6),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Forecasting",
                     plotOutput("holt_forecast"),
                     plotOutput("holt_forecast_test"),
                     width= 6)
        ),
        tabPanel("holt(damped=TRUE)",
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Selected Model",
                     textOutput("holtd_text1"),
                     tableOutput("holtd_fit_parameter"),
                     width= 12),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Residuals",
                     plotOutput("holtd_residuals"),
                     textOutput("holtd_text2"),
                     tableOutput("holtd_residualstests"),
                     width= 6),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Forecasting",
                     plotOutput("holtd_forecast"),
                     plotOutput("holtd_forecast_test"),
                     width= 6)
                 
        ),
        tabPanel("hw()",
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Selected Model",
                     textOutput("holtw_text1"),
                     tableOutput("holtw_fit_parameter"),
                     tableOutput("holtw_fit_parameter2"),
                     width= 12),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Residuals",
                     plotOutput("holtw_residuals"),
                     textOutput("holtw_text2"),
                     tableOutput("holtw_residualstests"),
                     width= 6),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Forecasting",
                     plotOutput("holtw_forecast"),
                     plotOutput("holtw_forecast_test"),
                     width= 6)
        ),
        tabPanel("ets()",
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Selected Model",
                     textOutput("ets_text1"),
                     tableOutput("ets_fit_parameter"),
                     tableOutput("ets_fit_parameter2"),
                     width= 12),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Residuals",
                     plotOutput("ets_residuals"),
                     textOutput("ets_text2"),
                     tableOutput("ets_residualstests"),
                     width= 6),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Forecasting",
                     plotOutput("ets_forecast"),
                     plotOutput("ets_forecast_test"),
                     width= 6)
        ),
        tabPanel("Comparing the Forecast Powers",
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Sliding window forecast performance study",
                     plotOutput("swplot"),
                     textOutput("swinitialtext"),
                     width= 12),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Forecasts",
                     plotOutput("forecasts_togheter"),
                     width= 9),
                 box(skin = "blue",
                     status = 'primary',
                     solidHeader = FALSE,
                     collapsible = TRUE ,
                     title = "Accuracies (MAE)",
                     tableOutput("accuracies"),
                     width= 3)
        ),tabPanel("Methodology",
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "MSTL",
                       textOutput("mstl_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "auto.arima()",
                       textOutput("arima_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "ses()",
                       textOutput("ses_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "holt()",
                       textOutput("holt_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "holt(damped=TRUE)",
                       textOutput("holtd_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "hw()",
                       textOutput("holtw_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "ets()",
                       textOutput("ets_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "Slide Window Study",
                       textOutput("sw_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "Shapiro-Wilk test",
                       textOutput("shap_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,collapsed=TRUE,
                       title = "KPSS test",
                       textOutput("kpss_explain"),
                       width= 4),
                   box(skin = "blue",
                       status = 'primary',
                       solidHeader = FALSE,
                       collapsible = TRUE ,
                       title = "Ljung-box test",collapsed=TRUE,
                       textOutput("ljbox_explain"),
                       width= 4)
        )
  )
)
)

server <- function(input, output,session) {

  
  output$text_xlsx_default = renderText({
    "The button below allows you to download the standard xlsx file that follows the format required for the upload in the upload area."
    })
  
  datasetairpassengers = reactive({
    data.frame(
      Date = seq.Date(from = as_date("1949-01-01"),
                      to = as_date("1960-12-01"),
                      by = "1 month"),
      AirPassengers = AirPassengers)
  })
  output$download_xlsx<-downloadHandler(
    filename = function(){"default.xlsx"},
    content = function(file){write_xlsx(datasetairpassengers(),path=file)})
  
  output$text_xlsx_default2 = renderText({
  'You can fill in the file with your series, but ensure that the dates are always in the first column, following the "YYYY-MM-DD" pattern, and the series data in the second column. Like in the image below:'
    })
  
  output$mstl_explain = renderText({
    "The Multiple Seasonal Decomposition of Time Series by Loess (MSTL) is a method used for decomposing time series data into multiple seasonal components, handling complex seasonal patterns. It employs a technique called Loess, which stands for locally estimated scatterplot smoothing, to identify and isolate various seasonal effects within the data. MSTL is effective in capturing multiple seasonalities, such as daily, weekly, and yearly patterns, allowing for their separate analysis. By decomposing the series into its trend, seasonal, and remainder components, MSTL assists in better understanding and modeling the underlying structure of time series, aiding in forecasting and anomaly detection within the data."
    })
  
  output$arima_explain = renderText({
    "The auto.arima() function in the R package forecast automatically identifies the best SARIMA (Seasonal Autoregressive Integrated Moving Average) model for a given time series without manual intervention. It systematically tests various combinations of SARIMA parameters (p, d, q)(P, D, Q) using statistical and heuristic methods, aiming to find the model that best fits the data. By employing criteria such as AIC or BIC, it selects the optimal model based on its ability to capture the data's patterns while balancing model complexity, enabling efficient forecasting for time series data."
  })
  output$ses_explain = renderText({
    "The ses() function in the R package forecast performs time series forecasting using the Simple Exponential Smoothing method (SES). It estimates future values by applying a weighted average of past observations, assuming a constant trend over time without considering seasonal effects. By adjusting the smoothing parameter (alpha), it calculates forecasts based on an exponentially decreasing weight given to previous observations, providing straightforward predictions for time series data but might not accommodate complex patterns like non-linear trends or seasonality."
  })
  output$holt_explain = renderText({
    "The holt() function in the R package forecast implements the Holt's method for time series forecasting, extending Simple Exponential Smoothing by considering both level and trend components. This method forecasts future values by incorporating exponential smoothing for the level (constant) and a trend component, allowing for varying trends over time. It suits data with a consistent trend, providing more flexibility than SES by accounting for changing trends but assumes the trends to be linear, potentially posing limitations when dealing with nonlinear patterns or sudden shifts in the data."
  })
  output$holtd_explain = renderText({
    "The holt(damped=TRUE) function in the R package forecast applies Holt's method with damping for time series forecasting, introducing a damping parameter that restrains the influence of past observations on the forecasted values over time. This modification aims to impose a bounded, diminishing effect of historical data, allowing for forecasting with trends that gradually flatten out. It combines exponential smoothing for level and trend components, damping the impact of historical observations, suitable for data exhibiting trends that attenuate or stabilize in the long run, yet assumes linear trends and might not capture abrupt changes or nonlinear patterns effectively."
  })
  output$holtw_explain = renderText({
    "The hw() function in the R package forecast implements the Holt-Winters method, a forecasting technique that extends Holt's method to accommodate seasonal variations in addition to level and trend components. This method utilizes triple exponential smoothing, incorporating exponential smoothing for level, trend, and seasonality. It captures the patterns in the data by considering both the seasonality and trend, providing forecasts that adjust to seasonal fluctuations, making it suitable for time series with consistent seasonal patterns. However, it assumes the seasonality to be constant over time and might not perform optimally with irregular or changing seasonal behaviors."
  })
  output$ets_explain = renderText({
    "The ets() function in the R package forecast implements the Exponential Smoothing State Space Model, offering a versatile framework that automatically selects between various exponential smoothing methods (Simple Exponential Smoothing, Holt's method, Holt-Winters method) and potentially incorporates additional components like trend, seasonality, and error structures. ETS models can adapt to diverse time series patterns, handling level shifts, trends, and seasonality, providing flexibility for both additive and multiplicative data patterns. However, while powerful in capturing various patterns, ETS models might not handle complex nonlinearities or sudden changes in data behavior as effectively."
  })
  output$sw_explain = renderText({
    "Sliding window analysis in time series involves repeatedly fitting models on sequential subsets of the data to evaluate their performance across different horizons. The tsCV() function in R aids in this analysis by implementing time series cross-validation. It facilitates model validation by dividing the time series into training and testing sets, where the testing set slides along the time series. For each iteration, tsCV() fits the model on the training data, forecasts the test horizon, and evaluates the forecast accuracy. This function is valuable for assessing how a model performs over varying periods of the time series, aiding in understanding its robustness and generalizability across different time frames."
    })
  output$mae_explain = renderText({
    "In the context of time series, the Mean Absolute Error (MAE) measures the average magnitude of errors between predicted and actual values. It calculates the absolute differences between predicted and observed values for each point in the time series, then averages these differences. MAE provides a straightforward assessment of the model's forecasting accuracy, indicating how far, on average, the predictions deviate from the actual values across the entire time series. As a metric, it's easy to interpret since it gives a direct average error magnitude, making it useful for understanding the model's overall predictive performance without considering the direction of the errors."
    })
  output$shap_explain = renderText({
    "In the context of time series residuals, the Shapiro-Wilk test is a statistical tool employed to assess the normality of the residuals' distribution. It works by examining whether the observed residuals conform to a normal distribution or not. A p-value < 0.05 from this test suggests that the residuals deviate significantly from normality. Assessing the normality of residuals is crucial in time series analysis as many statistical methods rely on the assumption of normally distributed errors for accurate modeling and reliable inference. If the residuals fail the normality test, it indicates potential issues in the model's assumptions, guiding further refinement or exploration of the underlying data patterns."
    })
  output$kpss_explain = renderText({
    "In the realm of time series residuals, the KPSS (Kwiatkowski-Phillips-Schmidt-Shin) test serves as a diagnostic tool to examine the stationarity of residuals. Unlike tests like ADF (Augmented Dickey-Fuller) that assess for unit roots in the series itself, KPSS evaluates if the residuals exhibit stationarity, implying that their mean and variance remain constant over time. If the test detects significant trends or changes in variance within the residuals, it indicates that the model might not have sufficiently captured the underlying patterns, highlighting potential issues that may require further model adjustments or exploration of the data dynamics. The p-value < 0.05 indicates the non-stationarity of the residuals."
  })
  output$ljbox_explain = renderText({
    "The Ljung-Box test is a statistical method used to evaluate the presence of autocorrelation in the residuals of a time series model. It assesses whether any correlation exists between different lags of the residuals. By examining a sequence of autocorrelations in the residuals up to a specific lag, the test checks if these correlations are statistically significant. If significant autocorrelations are detected, it suggests that the model might not fully account for all the underlying information in the data, indicating potential shortcomings that need to be addressed, such as refining the model or considering additional features in the analysis. The p-value < 0.05 indicates the lack of independence in the residuals."
  })
  
    serie_analize = eventReactive(input$calculatebtn,{
      
      
      if(input$opt_inputs == "A serie from my computer"){
        data = read_excel(input$file1$datapath)
        desc_serie = names(data)[2]
        names(data) = paste0("c",seq(1:ncol(data)))
        data = data %>% arrange(c1)
        
        
        
        inicio = str_split(data$c1[1],"-",simplify = T)
        dte = data$c1[(nrow(data)-input$tsh+1)]
        inicioteste = str_split(dte,"-",simplify = T) 
        x_trein = ts(data$c2[1:(nrow(data)-input$tsh)],
                   frequency = input$tsfreq,
                   start = c(as.numeric(inicio[1,1]),as.numeric(inicio[1,2])))
        x_test = ts(data$c2[(nrow(data)-input$tsh+1):nrow(data)],
                  frequency = input$tsfreq,
                  start = c(as.numeric(inicioteste[1,1]),as.numeric(inicioteste[1,2])))
      }
      if(input$opt_inputs == "AirPassengers time serie"){
        desc_serie="AirPassengers"
        x_trein = ts(AirPassengers[1:(length(AirPassengers)-input$tsh)],
                     frequency = input$tsfreq,
                     start = c(1949,1))
        inicioteste = ym("1949-1") %m+% months(length(AirPassengers)-input$tsh)
        inicioteste = str_split(inicioteste,"-",simplify = T) 
        x_test = ts(AirPassengers[(length(AirPassengers)-input$tsh+1):length(AirPassengers)],
                    frequency = input$tsfreq,
                    start = c(as.numeric(inicioteste[1,1]),as.numeric(inicioteste[1,2])))
      }
      lista = list(x_trein=x_trein,x_test=x_test,desc_serie=desc_serie)
      lista
    })
  
    x_trein = eventReactive(input$calculatebtn,{
      serie_analize()$x_trein
      })
    
    x_test = eventReactive(input$calculatebtn,{
      serie_analize()$x_test
      })
    
    description = eventReactive(input$calculatebtn,{
      serie_analize()$x_desc
      })

    output$tsPlot <- renderPlot({
      plot.ts(data.frame(cbind(x_trein(),x_test())), plot.type='s',col=1:2,ylab=description(),main="Time Serie")
      })
    
    output$MSTLPlot <- renderPlot({
      MSTL <- mstl(x_trein(), lambda = 'auto')
      autoplot(MSTL) +
        labs(x = "Ano") +
        theme_bw()
    })
    
    ##### AUTO.ARIMA #####
    arima_params = eventReactive(input$calculatebtn,{
      
      fit = auto.arima(x_trein())
      
      
      list(p=fit$arma[1],d=fit$arma[6],q=fit$arma[2],P=fit$arma[3],D=fit$arma[7],Q=fit$arma[4],f=fit$arma[5],AICc = fit$aicc,fit=fit)
      
    })
    
    output$arima_text1 = renderText({
      paste0("Using the auto.arima() function from the forecast package, the ARIMA class model found automatically has the following characteristics:")
    })
    
    
    output$arima_fit_parameter = renderTable({
      
      params = paste0("SARIMA(",arima_params()$p,",",arima_params()$d,",",arima_params()$q,")(",
                      arima_params()$P,",",arima_params()$D,",",arima_params()$Q,")[", arima_params()$f ,
                      "]")
      info = c("Order"=params,"AICc"=round(arima_params()$AICc,3),round(arima_params()$fit$coef,3))
      a = data.frame(t(matrix(info)))
      names(a)=rownames(data.frame(info))
      a
    })
    
    output$arima_residuals = renderPlot({
      fit= arima_params()$fit
      initial_res = 1 + arima_params()$d + input$tsfreq * arima_params()$D
      res = fit$residuals[initial_res:length(fit$residuals)]
      checkresiduals(res)
    })
    
    output$arima_text2= renderText({"P-Values for tests:"})
    
    output$arima_residualstests = renderTable({
      fit= arima_params()$fit
      initial_res = 1 + arima_params()$d + input$tsfreq * arima_params()$D 
      res = fit$residuals[initial_res:length(fit$residuals)]
      
      
      shapiro <- shapiro.test(res)
      kpss <- kpss.test(res)
      box <- Box.test(res, lag=15, type = "Ljung-Box")
      
      df_residuos <- data.frame(SW=shapiro$p.value,KPSS=kpss$p.value,LB=box$p.value)
                                
      names(df_residuos)<-c("Shapiro-Wilk","KPSS","Ljung-Box")
      
      df_residuos
    })
    
    output$arima_forecast = renderPlot({
      
      fit= arima_params()$fit
      res = fit$residuals
      initial_res = 1 + arima_params()$d + input$tsfreq * arima_params()$D 
      res = fit$residuals[initial_res:length(fit$residuals)]
      shapiro <- shapiro.test(res)$p.value
      btp = ifelse(shapiro>=0.05,FALSE,TRUE)
      fit_forec=forecast(fit, bootstrap=btp, h = length(x_test()),level = 95)
      plot(fit_forec)
    })
    
    output$arima_forecast_test = renderPlot({
      fit= arima_params()$fit
      res = fit$residuals
      initial_res = 1 + arima_params()$d + input$tsfreq * arima_params()$D 
      res = fit$residuals[initial_res:length(fit$residuals)]
      shapiro <- shapiro.test(res)$p.value
      btp = ifelse(shapiro>=0.05,FALSE,TRUE)
      fit_forec=forecast(fit, bootstrap=btp, h = length(x_test()),level = 95)
      prev_pont = fit_forec$mean
      lo95= fit_forec$lower
      hi95=fit_forec$upper
      tab=data.frame(cbind(prev_pont,lo95,hi95,x_test()))
      names(tab)=c("point forecast","lower 95%","higher 95%","real data")
      plot.ts(tab, plot.type='s',col=c("blue","grey","grey","red"),
              lwd=c(2,2,2,2),xlab="data",ylab="",main="With Zoom and Real Data")
      legend('topright', names(tab), col=c("blue","grey","grey","red"), lty=1, cex=.8)
    })
    
    
    
    ##### SES #####
    ses_params = eventReactive(input$calculatebtn,{
      
      fit = ses(x_trein(),level = 95,h= length(x_test()))
      
      
      list(alpha=fit$model$fit$par[1],l=fit$model$fit$par[2],AICc = fit$model$aicc,fit=fit)
      
    })
    
    output$ses_text1 = renderText({
      paste0("Using the ses() function from the forecast package, the SES model found automatically has the following characteristics:")
    })
    
    
    output$ses_fit_parameter = renderTable({
      info = c("AICc"=round(ses_params()$AICc,3),"alpha"=round(ses_params()$alpha,3),"l"=round(ses_params()$l,3))
      a = data.frame(t(matrix(info)))
      names(a)=rownames(data.frame(info))
      a
    })
    
    output$ses_residuals = renderPlot({
      fit= ses_params()$fit
      checkresiduals(fit)
    })
    
    output$ses_text2= renderText({"P-Values for tests:"})
    
    output$ses_residualstests = renderTable({
      fit= ses_params()$fit
      res = fit$residuals
      
      
      shapiro <- shapiro.test(res)
      kpss <- kpss.test(res)
      box <- Box.test(res, lag=15, type = "Ljung-Box")
      
      df_residuos <- data.frame(SW=shapiro$p.value,KPSS=kpss$p.value,LB=box$p.value)
      
      names(df_residuos)<-c("Shapiro-Wilk","KPSS","Ljung-Box")
      
      df_residuos
    })
    
    output$ses_forecast = renderPlot({
      plot(ses_params()$fit)
    })
    
    output$ses_forecast_test = renderPlot({
      fit_forec= ses_params()$fit
      prev_pont = fit_forec$mean
      lo95= fit_forec$lower
      hi95=fit_forec$upper
      tab=data.frame(cbind(prev_pont,lo95,hi95,x_test()))
      names(tab)=c("point forecast","lower 95%","higher 95%","real data")
      plot.ts(tab, plot.type='s',col=c("blue","grey","grey","red"),
              lwd=c(2,2,2,2),xlab="data",ylab="",main="With Zoom and Real Data")
      legend('topright', names(tab), col=c("blue","grey","grey","red"), lty=1, cex=.8)
    })
    
    ##### HOLT #####
    holt_params = eventReactive(input$calculatebtn,{
      
      fit = holt(x_trein(),level = 95,h= length(x_test()))
      
      
      list(alpha=fit$model$fit$par[1],beta=fit$model$fit$par[2],l=fit$model$fit$par[3],b=fit$model$fit$par[4],AICc = fit$model$aicc,fit=fit)
      
    })
    
    output$holt_text1 = renderText({
      paste0("Using the holt() function from the forecast package, the holt model found automatically has the following characteristics:")
    })
    
    
    output$holt_fit_parameter = renderTable({
      info = c("AICc"=round(holt_params()$AICc,3),"alpha"=round(holt_params()$alpha,3),"beta"=round(holt_params()$beta,3),"l"=round(holt_params()$l,3),"b"=round(holt_params()$b,3))
      a = data.frame(t(matrix(info)))
      names(a)=rownames(data.frame(info))
      a
    })
    
    output$holt_residuals = renderPlot({
      fit= holt_params()$fit
      checkresiduals(fit)
    })
    
    output$holt_text2= renderText({"P-Values for tests:"})
    
    output$holt_residualstests = renderTable({
      fit= holt_params()$fit
      res = fit$residuals
      
      shapiro <- shapiro.test(res)
      kpss <- kpss.test(res)
      box <- Box.test(res, lag=15, type = "Ljung-Box")
      
      df_residuos <- data.frame(SW=shapiro$p.value,KPSS=kpss$p.value,LB=box$p.value)
      
      names(df_residuos)<-c("Shapiro-Wilk","KPSS","Ljung-Box")
      
      df_residuos
    })
    
    output$holt_forecast = renderPlot({
      plot(holt_params()$fit)
    })
    
    output$holt_forecast_test = renderPlot({
      fit_forec= holt_params()$fit
      prev_pont = fit_forec$mean
      lo95= fit_forec$lower
      hi95=fit_forec$upper
      tab=data.frame(cbind(prev_pont,lo95,hi95,x_test()))
      names(tab)=c("point forecast","lower 95%","higher 95%","real data")
      plot.ts(tab, plot.type='s',col=c("blue","grey","grey","red"),
              lwd=c(2,2,2,2),xlab="data",ylab="",main="With Zoom and Real Data")
      legend('topright', names(tab), col=c("blue","grey","grey","red"), lty=1, cex=.8)
    })
    
    
    ##### holtd #####
    holtd_params = eventReactive(input$calculatebtn,{
      
      fit = holt(x_trein(),damped = TRUE,level = 95,h= length(x_test()))
      
      
      list(alpha=fit$model$fit$par[1],beta=fit$model$fit$par[2],phi=fit$model$fit$par[3],
           l=fit$model$fit$par[4],b=fit$model$fit$par[5],AICc = fit$model$aicc,fit=fit)
      
    })
    
    output$holtd_text1 = renderText({
      paste0("Using the holt(damped=TRUE) function from the forecast package, the holt with damped model found automatically has the following characteristics:")
    })
    
    output$holtd_fit_parameter = renderTable({
      info = c("AICc"=round(holtd_params()$AICc,3),"alpha"=round(holtd_params()$alpha,3),"beta"=round(holtd_params()$beta,3),"phi"=round(holtd_params()$phi,3),"l"=round(holtd_params()$l,3),"b"=round(holtd_params()$b,3))
      a = data.frame(t(matrix(info)))
      names(a)=rownames(data.frame(info))
      a
    })
    
    output$holtd_residuals = renderPlot({
      fit= holtd_params()$fit
      checkresiduals(fit)
    })
    
    output$holtd_text2= renderText({"P-Values for tests:"})
    
    output$holtd_residualstests = renderTable({
      fit= holtd_params()$fit
      res = fit$residuals
      
      
      shapiro <- shapiro.test(res)
      kpss <- kpss.test(res)
      box <- Box.test(res, lag=15, type = "Ljung-Box")
      
      df_residuos <- data.frame(SW=shapiro$p.value,KPSS=kpss$p.value,LB=box$p.value)
      
      names(df_residuos)<-c("Shapiro-Wilk","KPSS","Ljung-Box")
      
      df_residuos
    })
    
    output$holtd_forecast = renderPlot({
      plot(holtd_params()$fit)
    })
    
    output$holtd_forecast_test = renderPlot({
      fit_forec= holtd_params()$fit
      prev_pont = fit_forec$mean
      lo95= fit_forec$lower
      hi95=fit_forec$upper
      tab=data.frame(cbind(prev_pont,lo95,hi95,x_test()))
      names(tab)=c("point forecast","lower 95%","higher 95%","real data")
      plot.ts(tab, plot.type='s',col=c("blue","grey","grey","red"),
              lwd=c(2,2,2,2),xlab="data",ylab="",main="With Zoom and Real Data")
      legend('topright', names(tab), col=c("blue","grey","grey","red"), lty=1, cex=.8)
    })
    
    
    ##### holtw #####
    holtw_params = eventReactive(input$calculatebtn,{
      
      fit = hw(x_trein(),level = 95,h= length(x_test()))
      
      
      list(alpha=fit$model$fit$par[1],beta=fit$model$fit$par[2],gamma=fit$model$fit$par[3],
           l=fit$model$fit$par[4],b=fit$model$fit$par[5],s=fit$model$fit$par[6:length(fit$model$fit$par)],AICc = fit$model$aicc,fit=fit)
      
    })
    
    output$holtw_text1 = renderText({
      paste0("Using the hw() function from the forecast package, the holt-winters model found automatically has the following characteristics:")
    })
    
    output$holtw_fit_parameter = renderTable({
      info = c("AICc"=round(holtw_params()$AICc,3),"alpha"=round(holtw_params()$alpha,3),"beta"=round(holtw_params()$beta,3),"gamma"=round(holtw_params()$gamma,3),"l"=round(holtw_params()$l,3),"b"=round(holtw_params()$b,3))
      a = data.frame(t(matrix(info)))
      names(a)=rownames(data.frame(info))
      a
    })
    
    output$holtw_fit_parameter2 = renderTable({
      s= data.frame(t(as.matrix(round(holtw_params()$s,3))))
      names(s)=paste0("s",1:length(holtw_params()$s))
      s
    })
    
    output$holtw_residuals = renderPlot({
      fit= holtw_params()$fit
      checkresiduals(fit)
    })
    
    output$holtw_text2= renderText({"P-Values for tests:"})
    
    output$holtw_residualstests = renderTable({
      fit= holtw_params()$fit
      res = fit$residuals
      
      
      shapiro <- shapiro.test(res)
      kpss <- kpss.test(res)
      box <- Box.test(res, lag=15, type = "Ljung-Box")
      
      df_residuos <- data.frame(SW=shapiro$p.value,KPSS=kpss$p.value,LB=box$p.value)
      
      names(df_residuos)<-c("Shapiro-Wilk","KPSS","Ljung-Box")
      
      df_residuos
    })
    
    output$holtw_forecast = renderPlot({
      plot(holtw_params()$fit)
    })
    
    output$holtw_forecast_test = renderPlot({
      fit_forec= holtw_params()$fit
      prev_pont = fit_forec$mean
      lo95= fit_forec$lower
      hi95=fit_forec$upper
      tab=data.frame(cbind(prev_pont,lo95,hi95,x_test()))
      names(tab)=c("point forecast","lower 95%","higher 95%","real data")
      plot.ts(tab, plot.type='s',col=c("blue","grey","grey","red"),
              lwd=c(2,2,2,2),xlab="data",ylab="",main="With Zoom and Real Data")
      legend('topright', names(tab), col=c("blue","grey","grey","red"), lty=1, cex=.8)
    })
    
    ##### ets #####
    ets_params = eventReactive(input$calculatebtn,{
      
      fit = ets(x_trein())
      
      damped = ifelse(str_detect(fit$method,"d"),TRUE,FALSE)
      model = str_replace_all(fit$method,"(E|T|S|\\(|\\)|\\,|d)","")
      
      list(method=model,damped=damped,AICc = fit$aicc,fit=fit)
      
    })
    
    output$ets_text1 = renderText({
      paste0("Using the ets() function from the forecast package, the ETS model found automatically has the following characteristics:")
    })
    
    output$ets_fit_parameter = renderTable({
      info = c("Method"=ets_params()$fit$method,"AICc"=round(ets_params()$AICc,3))
      a = data.frame(t(matrix(info)))
      names(a)=rownames(data.frame(info))
      a
    })
    
    output$ets_fit_parameter2 = renderTable({
      s = data.frame(t(matrix(ets_params()$fit$par)))
      names(s)=rownames(data.frame(ets_params()$fit$par))
      s
    })
    
    output$ets_residuals = renderPlot({
      fit= ets_params()$fit
      checkresiduals(fit)
    })
    
    output$ets_text2= renderText({"P-Values for tests:"})
    
    output$ets_residualstests = renderTable({
      fit= ets_params()$fit
      res = fit$residuals
      
      
      shapiro <- shapiro.test(res)
      kpss <- kpss.test(res)
      box <- Box.test(res, lag=15, type = "Ljung-Box")
      
      df_residuos <- data.frame(SW=shapiro$p.value,KPSS=kpss$p.value,LB=box$p.value)
      
      names(df_residuos)<-c("Shapiro-Wilk","KPSS","Ljung-Box")
      
      df_residuos
    })
    
    output$ets_forecast = renderPlot({
      plot(forecast(ets_params()$fit,level = 95,h= length(x_test())))
    })
    
    output$ets_forecast_test = renderPlot({
      fit_forec= forecast(ets_params()$fit,level = 95,h= length(x_test()))
      prev_pont = fit_forec$mean
      lo95= fit_forec$lower
      hi95=fit_forec$upper
      tab=data.frame(cbind(prev_pont,lo95,hi95,x_test()))
      names(tab)=c("point forecast","lower 95%","higher 95%","real data")
      plot.ts(tab, plot.type='s',col=c("blue","grey","grey","red"),
              lwd=c(2,2,2,2),xlab="data",ylab="",main="With Zoom and Real Data")
      legend('topright', names(tab), col=c("blue","grey","grey","red"), lty=1, cex=.8)
    })
    
    ## Comparing
    
    output$accuracies = renderTable({
      # MAE e acuracia dos modelos
      y=x_test()
      # auto.arima
      acuracia1 <- forecast(arima_params()$fit, h = length(y), level = 95)
      acc1 <- mean(abs(y - acuracia1$mean))
      
      #ses
      acc2 <- mean(abs(y - ses_params()$fit$mean))
      
      #holt
      acc3 <- mean(abs(y - holt_params()$fit$mean))
      
      #holt+damped
      acc4 <- mean(abs(y - holtd_params()$fit$mean))
      
      #hw
      acc5 <- mean(abs(y - holtw_params()$fit$mean))
      
      #ets
      acuracia2 <- forecast(ets_params()$fit, h = length(y), level = 95)
      acc6 <- mean(abs(y - acuracia2$mean))
      
      df_acc = data.frame(
        "Function"= c("auto.arima()",
                    "ses()", "holt()", 
                    "holt(damped=TRUE)", 
                    "hw()", "ets()"),
        "MAE" = c(acc1,acc2,acc3,acc4,acc5,acc6)
      )
      
      df_acc <- df_acc %>% arrange(MAE)
      
      df_acc
    })
    
    output$forecasts_togheter = renderPlot({
      aafrc = forecast(arima_params()$fit, h = length(x_test()), level = 95)
      etsfrc = forecast(ets_params()$fit, h = length(x_test()), level = 95)
      
      tab=cbind("serie"=x_trein(),
                "real data" = x_test(),
                "auto.arima()" = aafrc$mean, 
                "ses()" = ses_params()$fit$mean,
                "holt()" = holt_params()$fit$mean,
                "holt(damped=T)"= holtd_params()$fit$mean,
                "hw()" = holtw_params()$fit$mean,
                "ets()" = etsfrc$mean)
      
      plot.ts(tab, plot.type='s',col=1:8,lwd=2,xlab="Date",ylab="",main=NULL)
      legend('topleft', legend=colnames(tab)[2:8], col=2:8, lwd=2)
      
    })
    
    sw = eventReactive(input$calculatebtn,{
      
      
      f_arima <- function(y, h){
        drft = "drift" %in% names(arima_params()$fit$coef)
        fit = Arima(y,
                    order = c(arima_params()$p,arima_params()$d,arima_params()$q),
                    seasonal = c(arima_params()$P,arima_params()$D,arima_params()$Q),
                    include.drift = drft)
        forecast(fit, h=h)
      }
      
      f_ses <- function(y, h){
        ses(y,h=h)
      }
      
      f_holt <- function(y, h){
        holt(y, h=h)
      }
      
      f_holt_d <- function(y, h){
        holt(y, h=h, damped = TRUE)
      }
      
      f_holt_w <- function(y, h){
        hw(y,h=h)
      }
      
      f_ets <- function(y, h){
        fit = ets(y,model = ets_params()$method,damped = ets_params()$damped )
        forecast(fit, h=h)
      }
      
      
      ini = length(x_trein()) - round(length(x_trein())*(input$tshswinitial/100),0)
      
      CV_arima = tsCV(y=x_trein(), forecastfunction = f_arima, h = input$tshsw, initial = ini)
      
      CV_ses = tsCV(y=x_trein(), forecastfunction = f_ses, h = input$tshsw, initial = ini)
      
      CV_h = tsCV(y=x_trein() , forecastfunction = f_holt, h = input$tshsw, initial = ini)
      
      CV_hd = tsCV(y=x_trein() , forecastfunction = f_holt_d, h = input$tshsw, initial = ini)
      
      CV_hw = tsCV(y=x_trein() , forecastfunction = f_holt_w, h = input$tshsw, initial = ini)
      
      CV_ets = tsCV(y=x_trein() , forecastfunction = f_ets, h = input$tshsw, initial = ini)
      
      MAE_arima = CV_arima %>% abs() %>% colMeans(na.rm=T)
      MAE_ses = CV_ses %>% abs() %>% colMeans(na.rm=T)
      MAE_h = CV_h %>% abs() %>% colMeans(na.rm=T)
      MAE_hd = CV_hd %>% abs() %>% colMeans(na.rm=T)
      MAE_hw = CV_hw %>% abs() %>% colMeans(na.rm=T)
      MAE_ets = CV_ets %>% abs() %>% colMeans(na.rm=T)
      tab = data.frame(cbind(round(MAE_arima,2), round(MAE_ses,2), round(MAE_h,2), round(MAE_hd,2), round(MAE_hw,2),round(MAE_ets,2)))
      names(tab) = c("auto.arima()", "ses()", "holt()", "holt(damped=T)", "hw()","ets()")
      
      list(tab=tab,ini=ini)
    })
    
    
    
    output$swplot = renderPlot({
      title= paste0("Study for ", input$tshsw ," forecast horizons started at n = ",sw()$ini)
      plot.ts(sw()$tab, plot.type='s',col=3:8,lwd=2,xlab="h",ylab="MAE", main = title)
      legend("topleft", legend=colnames(sw()$tab), col=3:8, lwd=2)
    })
    
    output$swinitialtext = renderText({
      paste0("You chose to use ", input$tshswinitial ,
             "% of the series for this study. The series size is n = ",length(x_trein()) ,
             ", so the study began at n = ",
             length(x_trein()) - round(length(x_trein())*(input$tshswinitial/100),0),".")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
