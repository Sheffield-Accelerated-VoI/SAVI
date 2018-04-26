# Copyright (c) 2014, 2015, 2018 the SAVI authors (see AUTHORS.txt).
# Licensed under the BSD 3-clause license (see LICENSE.txt)



##################
# BEGIN FUNCTION #
##################

fluidPage(
  
  ###########
  # STYLING #
  ###########
  
  tags$head(
    tags$style("body {background-color: #FFFFFF; }")),
  tags$style(type='text/css', '#textCheckTab {color: red;}'),
  tags$style(type='text/css', '#textCheckTabParams {color: red;}'),
  tags$style(type='text/css', '#textCheckTabCosts {color: red;}'),
  tags$style(type='text/css', '#textCheckTabEffects {color: red;}'),
  
  
  
  
  ################
  # HEADER PANEL #
  ################
  
  
  # uni is #F1F2F2
  headerPanel(HTML("<a href='http://www.sheffield.ac.uk/' 
                    target='_blank'><img src = 'uni2.gif' width = '200' 
                   alt='University of Sheffield logo' /></a> 
                    SAVI - Sheffield Accelerated Value of Information")),
  
  mainPanel(
    h4("Release version 2.1.0 (2018-04-26)"),    
    h6(HTML("Copyright &copy; 2018 University of Sheffield")),  
    
    
    
    
    
    ############
    # HOME TAB #
    ############
    
    tabsetPanel(       
      tabPanel("Home",
        sidebarLayout(position = "right",
          sidebarPanel(
            #h3("Returning Users"),
            #h3('Load previously saved session'),
            #p("The \"Load previously saved session\" facility is temporarily out 
            #  of action due to problems of backward compatibility 
            #  with SAVI version 1."),
            #fileInput('loadSession', label = h4('Load previously saved session')),
            #br(),br(),
            h3("Sign up for SAVI news and updates"),
            p(HTML("Send a blank email to"), 
             a("savi@sheffield.ac.uk", href='mailto:savi@sheffield.ac.uk?Subject=Register')),               
            p("We won't share your email address with anyone."),
            p("Also, you can now follow SAVI on Twitter. The SAVI team tweet regular updates and new features."),
            p(HTML("<a href='https://twitter.com/SheffieldSAVI' class='twitter-follow-button' data-show-count
              ='false' data-size='large'>Follow @SheffieldSAVI</a><script>!function(d,s,id)
              {var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
              if(!d.getElementById(id)){js=d.createElement(s);
              s.id=id;js.src=p+'://platform.twitter.com/widgets.js';
              fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>")),
            br(),
            
            h3("News"),  
            p(HTML("<strong>SAVI is now available as an R package</strong>, allowing you to run SAVI directly on your own machine. 
              You can download instructions "), 
              a("here.", target = '_blank',
                href="https://www.shef.ac.uk/polopoly_fs/1.511325!/file/Instructions_for_SAVI-package.txt")),
            br(),
            
            h3("New features and bug fixes"),
            strong("New in version 2.1.0"),
            p(HTML("We have added a new 'Risk Analysis' tab. SAVI now generates Risk Analysis charts as described in "), a("this
              paper by Grimm et al.", target = '_blank', href = "https://link.springer.com/article/10.1007%2Fs40273-017-0562-9")),
            strong("Fix for version 2.0.9"),
            p("We have added a note on the EVPPI Groups tab to say that the GP method for 
              calculating partial EVPI for groups of five or more parameters uses only the first 
              7,500 rows of the PSA."), 
            strong("Fix for version 2.0.5"),
            p("We have found that, for individual level simulation models, the regression method works best
              when a small number of individuals (rather than a single individual) are run per PSA sample. 
              Instructions have been updated."), 
            strong("New in version 2.0.0"),
            p("SAVI now calculates value of information for individual level simulation 
              models where only a single individual is simulated per PSA sample. See 
              the \"About your model\" tab."), 
            p("All tables can now be downloaded as csv files."), 
            strong("New in version 1.013"),
            p("You can now download some test files to try out on SAVI. See the Import Files tab."),           
            strong("New in version 1.009"),
            p("SAVI now accepts text files with the tab or the semicolon separator, 
              and with the comma as the decimal mark."),              
            strong("New in version 1.005"),
            p("You can now download a report containing all the results from the 
              SAVI analysis. Click on the Downloads tab."), 
            p("Different interventions and comparators can now be specified for the CE plane."), 
            br(),
            
            h3("Known issues"),  
            p("We've removed the individual-level patient simulation model tick-box option 
                while we upgrade the code. The new implimentation will improve speed and accuracy. 
              SAVI still works for your patient-level simulation model, 
                but we assume that the model has been run with a sufficient number
                of patients per PSA sample such that the mean costs and effects are stable."),
            p("Sometimes SAVI will either not load, or will hang for a while. 
              This is because SAVI can only deal with one set of computations at a time, 
              even though SAVI allows multiple concurrent users. 
              Be assurred that SAVI keeps concurrent users' data and results separate."),
            p("The \"Save session\" and \"Load previously saved session\" 
              facilities are temporarily out of action due to problems of backward compatibility 
              with SAVI version 1."),
            p("The report that SAVI generates is not quite as polished as we would like. 
              We are working on this."),
            br()

          ),
          

          mainPanel(  
            h3("What SAVI does"),
            p(HTML("Using"), strong("only"), HTML("PSA results from your model")),
            p(HTML("In a matter of seconds from the SAVI online application you can generate:")),
            tags$ol(
             tags$li("Standardised assessment of uncertainty (C-E planes and CEACs)"), 
             tags$li("Overall EVPI per patient, per jurisdiction per year and over 
                     your decision relevance horizon"), 
             tags$li("Expected Value of Perfect Parameter Information (EVPPI) 
                     for single and groups of parameters")
            ),
            #p("For individual-level simulation models you only need to simulate 
            #       a small number of individuals per PSA sample. 
            #  See the \"About your model\" tab."),
            
            br(),
            p(HTML("Disclaimer: This application is based on peer-reviewed 
                statistical approximation methods.  
              It comes with no warranty and should be utilised at the user\'s own risk 
                (see <a href = 'https://raw.githubusercontent.com/Sheffield-Accelerated-VoI/SAVI/master/DISCLAIMER.txt' 
              target = '_blank'>here</a>).
              The <a href = 'https://github.com/Sheffield-Accelerated-VoI/SAVI' 
                target='_blank'>underlying code</a> is made available under the 
              <a href = 'https://raw.githubusercontent.com/Sheffield-Accelerated-VoI/SAVI/master/LICENSE.txt' 
                   target = '_blank'>BSD 3-clause license</a>.")),                      
           # p(HTML("For more information on the method see   
           #   <a href='http://www.sheffield.ac.uk/scharr/sections/ph/staff/profiles/mark' 
           #   target='_blank'>Mark Strong's website</a> or 
           #   <a href = 'http://mdm.sagepub.com/content/34/3/311' target='_blank'>this paper</a>.")), 
            p(HTML("<strong>If you use SAVI in your work please cite our paper</strong>")),   
            p(HTML("<div style='border:1px solid
              black;width:67%;padding-left: 1em'>Strong M, Oakley JE, Brennan A. 
              Estimating multi-parameter partial Expected Value of 
              Perfect Information from a probabilistic sensitivity analysis sample: 
              a non-parametric regression approach. 
              <em>Medical Decision Making.</em> 2014;<b>34(3)</b>:311-26. Available open access 
              <a href='http://mdm.sagepub.com/content/34/3/311' target='_blank'>here.</a></div>")),


            br(),
            p(HTML("The SAVI process has 4 steps (using the TABS from left to right)")),
            p(HTML("Step 1: Save PSA input parametes, costs and effects as separate .csv files")),
              img(src = "step1_excel.png", style = "width:50%"),
            br(),
            br(),
            p(HTML("Step 2: Input details about your model, then upload and check PSA samples")),
              img(src = "step2_modelsetup.png", style = "width:100%"),
            br(),
            br(),
            p(HTML("Step 3: View your VoI analysis")),
              img(src = "step5_viewVOI.png", style = "width:50%"),
            br(),
            br(),
            p(HTML("Step 4: Download your results as .csv files. 
                   Download a report as a PDF, HTML or word document")),
              img(src = "step6_download.png", style = "width:50%"),
            br(),br(),
            
            h3("If you get stuck, or if something doesn't work, please let us know"),
            p(HTML("Our email address is"), a("savi@sheffield.ac.uk", href='mailto:savi@sheffield.ac.uk')),               
            br(),br(),br()
          )                    
        )
      ),
      
      
      
      
      
      
      
      
      
      
      
      
      ########################
      # ABOUT YOUR MODEL TAB #
      ########################
      
      tabPanel("About your model", 
#        sidebarLayout(position = "right",
#          sidebarPanel(
#            h3("Individual level simulation models"),
#            h4("This box is relevant if your model is an individual level simulation model."),
#            br(),
#            p(HTML("If individuals within a simulation model are independent
#             (conditional on the PSA parameters) then the optimum number of
#             individuals to simulate per sample of the PSA parameters is <strong>1</strong>. 
#             This will lead to the most efficient (lowest variance) estimators for the
#             mean costs, mean effects and mean net benefits (O\'Hagan et al., 2007)." )),
#            p("However, by simulating only a single patient per PSA sample we cannot 
#              calculate the overall EVPI, or construct the Cost-Effectiveness Plane
#              or CEAC using standard methods."),
#            p("We first need to partition the variation in the costs and effects
#              into variation caused by individual level variation, and variation 
#              caused by PSA level variation."), 
#            p("We can do this using non-parametric regression. The regression works best 
#              when a small number of patients are run per PSA sample, 
#               rather than just a single patient. We recommend at least 30."),
#            p("In SAVI, if \"Yes\" is selected below, an additive GAM 
#               model is used to separate the sources of variation. The 
#               GAM model fitted values are approximately equal to the values that would 
#               have been obtained had a large number of individuals been sampled per PSA run."),
#            p("The overall EVPI, the CE Plane, and the CEAC are then generated using
#              these GAM fitted values."),
#            p(),
#            radioButtons("indSim", label = h4("Is the model an individual level 
#             simulation with a small number of patients per PSA run?"), 
#                         choices = c("No", "Yes"), inline=TRUE),                             
#            br(),
#            p(HTML("<small>O\'Hagan et al. (2007) Health Economics. 16: 1009-23</small>"))
#            , width = 5),
#          
#          
#          mainPanel(                
           h3("Specify details about your model here"),
           h5("These can changed at any time - results will automatically update"),
           h5("Enter numeric values without a thousand separator comma, i.e. '15100', rather than '15,100'"),
           textInput("modelName", label = h5(strong("Name of your model")), value ="Model name goes here"),
           #textInput("current", label = h5("Name of strategy considered to be current/standard care"), 
           #value ="Current Care"),
           #textInput("t3", label = h5("Names of other strategies"), value ="Intervention 1"),
           #Need some way of adding more than one name to box
           numericInput("lambdaOverall", 
                     label = h5(strong("Threshold value of one unit of health effect (lambda)")), 
                     value = 20000, min = 0, step = 1000),
           textInput("effectDef", label = h5(strong("Definition of effectiveness measure")), 
                     value ="Discounted Lifetime QALYs"),
           textInput("costDef", label = h5(strong("Definition of cost measure")),
                     value ="Discounted Lifetime Costs (£)"),
           numericInput("annualPrev", label = h5(strong("Annual prevalence within jurisdiction 
                     (number of patients affected by the decision each year)")), 
                     value = 1000, min = 0, step = 10),
           numericInput("horizon", label = h5(strong("Decision relevance horizon 
                     (number of years that decision between these strategies 
                     is likely to be relevant)")), value = 10, min = 1),
           #numericInput("n1",label = h5("Number of strategies compared in the model 
           #(including current/standard care)"), value = 2, min = 2),
           #numericInput("nPeople",label = h5("If yes, how many individuals were run per PSA sample?"),
           #value = 0, min = 0, step = 100),
           textInput("currency", label = h5(strong("Units used for costs")), value ="£"),
           textInput("unitBens", label = h5(strong("Units used for benefits")), value ="QALY"),
           textInput("jurisdiction", label = h5(strong("Name of jurisdiction (e.g. country, region, city)")), 
                      value = "England")
           #, width = 7)
       #)     
      ),
      
      
      
      
      
      
      
      
      
      
      ####################
      # IMPORT FILES TAB #
      ####################
      
      tabPanel("Import files",     # Button to import parameter data         
       sidebarLayout(position = "right",
         sidebarPanel(
           h4("Dowload test files"),
           p("Try out SAVI using these test files that we have generated from a hypothetical model. 
             The model has 19 uncertain parameters and two decision options."),
           br(),
           downloadButton('testParams', 'Download parameters.csv'),
           br(),
           br(),
           downloadButton('testCosts', 'Download costs.csv'),
           br(),
           br(),
           downloadButton('testEffects', 'Download effects.csv')
         ),
         
         
         
         mainPanel(
           p(HTML("<div style='border:1px solid
           black;width:7;padding-left: 1em'><h4>Setting up your files for import</h4>
           Import your PSA samples of parameters, costs and effects using the 
           import buttons below.
           <br>Please supply the PSA samples in the form of three csv files.
           <br><br>
           SAVI assumes that the first row of the parameter file contains the parameter names.<br>
           SAVI assumes that the first row of the costs file holds the decision option names. <br>
           Avoid using any special symbols (e.g. currency symbols) in the names.<br>
           The first row of the effects file should also hold names, but these names are not used by SAVI.
           <br><br>
           The csv files must each have the same number of rows, and the rows must correspond, i.e. 
           the parameters in row 1 must be those that correspond to the costs and effects in row 1, and so on.
           <br><br>
           Costs and effects are assumed to be per-person, and to be absolute rather 
           than incremental (i.e. there must be 
           the same number of columns as decision options, including the baseline decision).<br>
           <br>
           </div>")),
           
           h3("Parameter importation"),
           # Various checkboxes and input fields to specify the data file format
           #checkboxInput('header1', 'Is there a header row?', TRUE),
           #checkboxInput('rownames1', 'Does the first column contain row names?', FALSE),
           radioButtons('sep', h5('Separator:'),
                        c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ',', inline=TRUE),
           #radioButtons('quote', 'Quote:',
           #           c('None'='none', 'Double Quote'="\"'", 'Single Quote'="\''"),
           #           selected="\"'", inline=TRUE),
           radioButtons('dec', h5('Decimal mark'), c(Dot='.', Comma=','), '.', inline=TRUE),
           fileInput('parameterFile', 'Choose CSV File',
                     accept=c('text/csv')),
           #br(),
           h4(textOutput("textCheckTabParams")),
           
           # Button to import costs  data    
           h3("Costs importation"),      
           # Various checkboxes and input fields to specify the data file format
           #checkboxInput('header2', 'Is there a header row?', TRUE),
           #checkboxInput('rownames2', 'Does the first column contain row names?', FALSE),
           radioButtons('sep2', h5('Separator:'),
                        c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ',', inline=TRUE),
           #radioButtons('quote2', 'Quote:',
           #        c(None='','Double Quote'='"','Single Quote'="'"),
           #           '"', inline=TRUE),
           radioButtons('dec2', h5('Decimal mark'), c(Dot='.', Comma=','), '.', inline=TRUE),
           fileInput('costsFile', 'Choose CSV File',
                     accept=c('text/csv')),
           #br(),
           h4(textOutput("textCheckTabCosts")),
           
           # Button to import effects data
           h3("Effects importation"),
           # Various checkboxes and input fields to specify the data file format
           #checkboxInput('header3', 'Is there a header row?', TRUE),
           #checkboxInput('rownames3', 'Does the first column contain row names?', FALSE),
           radioButtons('sep3', h5('Separator:'),
                        c(Comma=',',Semicolon=';',Tab='\t', Space=' '), ',', inline=TRUE),
           #radioButtons('quote3', 'Quote:',
           #          c(None='','Double Quote'='"','Single Quote'="'"),
           #          '"', inline=TRUE),
           radioButtons('dec3', h5('Decimal mark'), c(Dot='.', Comma=','), '.', inline=TRUE),
           fileInput('effectsFile', 'Choose CSV File',
                     accept=c('text/csv')),
           #h4("Are uploaded costs and effects incremental or absolute?"),
           #radioButtons('incremental', label="", c("Incremental" = "TRUE", "Absolute" = "FALSE"), "FALSE")
           
           h4(textOutput("textCheckTabEffects")),
           h4(textOutput("textCheckTab"))            
         )
       )
      ),
      
      
      
      
      
      
      
      ####################
      # CHECK UPLOAD TAB #
      ####################
      
      tabPanel("Check upload",
               
               h3("Check data import"),
               h4("The parameters, costs and effects you have uploaded are displayed below:"),
               p("(only the first 5 first rows of each dataset are shown)"),br(),
               
               h4("Parameters"),
               htmlOutput("textParamsTable"),
               tableOutput("checktable1"),
               h4("Costs"),
               htmlOutput("textCostsTable"),
               tableOutput("checktable2"),
               h4("Effects"),
               htmlOutput("textEffectsTable"),
               tableOutput("checktable3")
      ),
      
      
      
      
      
      
      
      
      
      
      
      ###################
      # PSA RESULTS TAB #
      ###################     
      
      tabPanel("PSA Results",
               h3("Cost-Effectiveness Plane"),
               
               sidebarLayout(
                 sidebarPanel(
                   h5("Choose intervention"),
                   radioButtons("decisionOptionCE1", NULL, 
                                c(""), 
                                selected = NULL),
                   br(),
                   
                   h5("Choose comparator"),
                   radioButtons("decisionOptionCE0", NULL, 
                                c(""), 
                                selected = NULL),
                   br()
                   , width=5),
                 
                 mainPanel(
                   plotOutput("plots1", width="500px", height="500px"),
                   br(),
                   p(strong("Strategies Compared"), textOutput("textCEplane4"))
                   , width = 7)
               ),
               
               p(HTML("<div id='textCEplane1' class='shiny-text-output'></div>")),
               br(),
               
               textOutput("textCEplane2"),
               br(),
               
               textOutput("textCEplane3"),
               h6(strong("Reference")),
               p("Section 5.1 in Briggs, Claxton & Sculpher. 
                Decision Modelling for Health Economic Evaluation 
                (Handbooks for Health Economic Evaluation). OUP Oxford; 
                1 edition (2006).  ISBN-13: 978-0198526629"),
               br(),
               
               h3("Table of Key Cost-Effectiveness Statistics"),
               tableOutput("tableCEplane"),

               h4("Download table as a csv file"),
               downloadButton('downloadTableCEplane', 'Download table'),
               br(),
               br(),
               
               h3("Cost-Effectiveness Acceptability Curve (CEAC)"),
               textOutput("textCEAC1"),
               h6(strong("Reference")),
               p("A guide to cost-effectiveness acceptability curves. 
                Fenwick & Byford. The British Journal of 
                Psychiatry (2005) 187: 106-108 doi: 10.1192/bjp.187.2.106"),
               br(),
               
               plotOutput("plots2", width="500px", height="500px"),    
               br(),
               
               h3("Net Benefit"),
               textOutput("textNB1"),
               br(),
               
               textOutput("textNB2"),
               br(),
               
               p("This is particularly useful when comparing several strategies 
                because the analyst and decision maker can 
                see in one single measure the expected net value of each strategy, 
                rather than looking at many comparisons of 
                incremental cost-effectiveness ratios between different options.  
                Under the rules of decision theory, the strategy 
                with the greatest expected net benefit is optimal."),
               br(),
               
               h3("Summary of Absolute Net Benefit Statistics"),
               tableOutput("tableNetBenefit"),
               h4("Download table as a csv file"),
               downloadButton('downloadTableNetBenefit', 'Download table'),               
               br(),
               br(),
               
               textOutput("textNB3"),
               br(),
               
               plotOutput("plots5a", width="500px", height="500px"),
               br(),
               
               h1("Net Benefit Densities"),
               plotOutput("plots5", width="700px", height="400px"),
               br(),
               
               p("Analysis of the expected incremental net benefit helps to visualise 
                whether particular strategies are better than others 
                and how certain a decision maker can be about the differences."),
               
               p("If there are strategies with  
                credible intervals for incremental net benefit that include zero, 
                then there is decision uncertainty. 
                Whether it is valuable to consider further research to reduce 
                uncertainty is the motivation for the value 
                of information calculations. These calculations can consider decision 
                uncertainty arising from all uncertain parameters together 
                (the overall expected value of perfect 
                information – overall EVPI) or for particular sets of uncertain 
                parameters (the expected value of perfect parameter 
                information – EVPPI)."),
               br()#,
               
               #h3("Summary of Incremental Net Benefit Statistics")
               #tableOutput("tableNetBenefitInc") Not yet made
               
      ),
      
      
      
      
      
      
      
      
      
      
      
      
      
      ############
      # EVPI TAB #
      ############
      
      tabPanel("EVPI",
               h3("Overall Expected Value of Perfect Information"),
               
               textOutput("textEVPI1"),
               br(),
               
               textOutput("textEVPI2"),
               br(),
               
               textOutput("textEVPI3"),
               br(),
               
               textOutput("textEVPI4"),
               br(),
               
               textOutput("textEVPI5"),
               br(),
               
               
               h4("Overall EVPI"),
               h6("The Expected Value of Removing all Current Decision Uncertainty"),
               tableOutput("tableEVPI"), 
               h4("Download table as a csv file"),
               downloadButton('downloadTableEVPI', 'Download table'),
               br(),
               br(),
               
               h4("Overall EVPI (on costs scale) versus lambda"),               
               plotOutput("plots3", width="500px", height="500px"),
               
               h4("Overall EVPI (on effects scale) versus lambda"),
               plotOutput("plots4", width="500px", height="500px"),
               br(),
               
               h4("Overall EVPI per population and time horizon"),
               plotOutput("plots6", width="700px", height="600px"),
               
               h4("Understanding the EVPI: a non-technical explanation"),
               br(),
               
               p("The calculation begins with the existing confidence intervals 
                (or credible intervals) for the model parameters as used 
                 in the probabilistic sensitivity analysis.  We then imagine a 
                world in which we become absolutely (perfectly) certain 
                about all of the model parameters i.e. the confidence interval 
                for every single parameter is shrunk right down to zero.  
                 The decision maker would then be absolutely certain which strategy 
                to select and would choose the one with highest net 
                 benefit.  One can visualise this idea by imagining that instead 
                of seeing the cloud of dots on the cost-effectiveness plane 
                 (representing current uncertainty in costs and benefits) and 
                having to choose, the decision maker now knows exactly which 
                 \'dot\' is the true value (because all of the uncertainty is removed) 
                and so can be certain to choose the strategy which 
                 gives the greatest net benefit. In a two strategy comparison of new 
                versus current care, if the \'true dot\' turns out to be 
                 below and to the right of the threshold lambda line, then the 
                decision maker would select the new strategy.  If the \'true dot\' 
                 is above and to the left, then current care would be selected.  
                Under the current uncertainty, the decision maker will 
                 choose the strategy based on the expected costs and benefits 
                (essentially on whether the \'centre of gravity\' of the cloud 
                 is above or below the threshold line)."),
               br()
               
      ),
      
      
      
      
      
      
      
      
      
      
      
      
      ###############################
      # EVPPI SINGLE PARAMETERS TAB #
      ###############################
      
      tabPanel("EVPPI single parameters",
         #h3("Specify lambda"),
         #sliderInput('lambdaSingleEvppiTab', label="", 0, 60000, 20000, 1000),
         h3("Partial EVPI for each parameter separately"),
         p("Partial EVPI enables identification of those parameters that contribute particularly
           highly to decision uncertainty. For each parameter, the expected value of removing
           current uncertainty is displayed in the table below. The barplot shows parameters in 
           descending order of importance."),
         br(),
         
         h4("EVPPI for individual parameters"),
         tableOutput("tableEVPPI"),
         
         h4("Download table as a csv file"),
         downloadButton('downloadSingleEVPPI', 'Download table'),
         
         #h3("Partial EVPI for single parameters"),
         #tableOutput("summary"),
         
         h3("Partial EVPI barplot for single parameters"),
         plotOutput("plot7", width="500px", height="500px")
               
      ),
      
      
      
      
      
      
      
      
      
      
      
      
      
      ####################
      # EVPPI GROUPS TAB #
      ####################
      
      
      tabPanel("EVPPI groups",
         h3("Partial EVPI for groups of parameters"),
         p(HTML("Although EVPPI information about individual parameters is 
          useful, often it is more informative if EVPPI can
           be computed for groups of associated parameters e.g. all 
          parameters associated with efficacy data. This is the
           maximum expected value of further research that will jointly 
          inform this set of parameters.")),
         p(HTML("First, define groups of parameters for which to calculate 
          EVPPI. Choose a subset of parameters
          using the tick boxes and press the Calculate EVPPI button.")),
         p(HTML("When calculation of the first parameter group is complete, 
          select a new subset (remember to untick
            your original choices) and press the Calculate EVPPI button 
          again. This can be repeated for any number of different
          groups, with all results appearing below on an expanding results table.")), 
         p(HTML("For subsets with up to five parameters, the GAM regression method is used. 
          For subsets with five or more parameters the GP regression method is used. 
          See <a href='http://mdm.sagepub.com/content/34/3/311' 
                target='_blank'>this paper</a> for details.")),
         br(),
         sidebarLayout(
           sidebarPanel(
             h3("Select Parameters for EVPPI"),
             checkboxGroupInput("pevpiParameters", NULL, 
                                c(""), 
                                selected = NULL),
             br(),
             #                    actionButton("addSelection", "Add selection"),
             #                    br(),
             #                    br(),
             actionButton("calculateSubsetsEvpi", "Calculate EVPPI values"), width=5),
           
           mainPanel(
             br(),
             h3("Results"),
             br(),
             h4("EVPPI for parameter groups"),
             tableOutput("selectedTable"),
             h4("Download table as a csv file"),
             downloadButton('downloadGroupEVPPI', 'Download table'),
             # br(),
             #h4("Partial EVPI values"),
             #tableOutput("selectedEvpiTable"),
             #   br(),
             #   actionButton("clearSubsetsEvpi", "Clear Selections"),
             width=7)),
         p("NOTES"),
         p(tags$ul(
           tags$li("Currently this table does not automatically 
          update previously calculated EVPPI values
          when model settings (e.g. lambda) are changed."), 
           tags$li("The GP method must invert an n x n matrix where n is the number of 
           rows in the PSA. This is very slow for large matrices, so only the first 7,500
           rows of the PSA are used at present")
         ))
      ),
      
      # Numerical summary of the dataset,
      # coming from the function output$summary in server.R
      
      
      


      ############
      # PSUB TAB #
      ############
      
      tabPanel("Risk Analysis",
               h3("The Payer Strategy-Specific and Uncertainty Burdens"),
               p("The 'Payer Strategy-Specific Burden' (PSB) and 'Payer Uncertainty Burden' (PUB)
                             reflect the payer's financial risks."),
               p(HTML("The PSB for decision option <em>d</em> is the difference
                      between the expected net benefit of the most
                      cost-effective option, and the expected net benefit of decision option <em>d</em>.
                      The PSB indicates to the Payer the risk of choosing an option
                            that is not the most cost-effective option.")),
               p("The PUB is equal to the overall Expected Value of Perfect Information.
                            It indicates to the Payer the financial
                             risk of making the decision with current evidence,
                       relative to making the decision with perfect evidence."),
               p(HTML("The concepts are explained in detail in this
                            <a href = 'http://www.nicedsu.org.uk/Managed-Entry-Agreements-MEA(3026860).htm'
                           target='_blank'>NICE Decision Support Unit Report</a>, and in 
                            <a href = 'https://link.springer.com/article/10.1007%2Fs40273-017-0562-9' 
                           target='_blank'>this paper by Grimm et al</a>.")),
               br(),
               h4("Table showing Payer Strategy-Specific and Uncertainty Burdens, relative to the most cost-effective option"),
               tableOutput("tablePSUB"),
               h4("Download table as a csv file"),
               downloadButton('downloadTablePSUB', 'Download table'),
               
               br(),
               br(),
               h4("Stacked barchart showing Payer Strategy-Specific and Uncertainty Burdens,
                                relative to the most cost-effective option"),
               plotOutput("plotsPSUBstacked", width="700px", height="600px"),
               br(),
               br(),
               h4("Side-by-side barchart showing Payer Strategy-Specific and Uncertainty Burdens,
                                relative to the most cost-effective option"),
               plotOutput("plotsPSUBsideBySide", width="700px", height="600px"),
               br()
               
      ),


      
      
      
      
      
      #################
      # REPORT TAB #
      #################
      
      tabPanel("Report", 
         h3("Download summary report"),
         p("This document contains all the tables and figures generated from the 
           SAVI analysis of your PSA."),
         radioButtons('format', 'Please select the document format you require', 
                      c('PDF', 'HTML', 'Word'),
                      inline = TRUE),
         downloadButton('downloadReport', 'Download summary report'),
         br(), br(), 
         p("NB generating the document can take some time.")
      ),
      
      
      
      
      
      
      
      
      
      ####################
      # SAVE SESSION TAB # 
      ####################
      
#      CURRENTLY OUT OF ACTION
      
#       tabPanel("Save session", 
#          textInput("RdataFileName", strong("Filename"), value="SAVISession.Rdata"),
#          br(), br(),
#          downloadButton('saveSession', 'Save SAVI session')
#       ),
      
      
      
      
      
      
      
      
      
      
      ################
      # ABOUT US TAB #
      ################
      
      tabPanel("About us", 
       h3("About us"),
       p(HTML("This web tool is an 
              <a href = 'http://shiny.rstudio.com/' target='_blank'>R Shiny Server application</a>.")),
       p(HTML("It was written at the University of Sheffield's 
              <a href = 'http://www.sheffield.ac.uk/scharr' 
                target='_blank'>School of Health and Related Research</a> by 
              <a href='https://www.shef.ac.uk/scharr/sections/ph/staff/profiles/mark' 
                target='_blank'>Mark Strong</a>, 
              <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/breeze_p' 
                target='_blank'>Penny Breeze</a>, 
              <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/thomas_c' 
                target='_blank'>Chloe Thomas</a>, 
              <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/brennan_a' 
                target='_blank'>Alan Brennan</a> and 
              <a href='http://www.imperial.ac.uk/people/christophe.stevens' 
                target='_blank'>Christophe Stevens</a>.")), 
       p(HTML("The regression-based method for approximating partial EVPI was developed by 
         <a href='https://www.shef.ac.uk/scharr/sections/ph/staff/profiles/mark' 
          target='_blank'>Mark Strong</a>
              in collaboration with 
              <a href='http://www.jeremy-oakley.staff.shef.ac.uk/' target='_blank'>Jeremy Oakley</a>
              and <a href='https://www.sheffield.ac.uk/scharr/sections/heds/staff/brennan_a' 
              target='_blank'>Alan Brennan</a>.")),
       p(HTML("The source code is available on GitHub at 
        <a href = 'https://github.com/Sheffield-Accelerated-VoI/SAVI' 
              target='_blank'>https://github.com/Sheffield-Accelerated-VoI/SAVI.</a>")),
       p(HTML("Please cite the method as")),
       p(HTML("<div style='border:1px solid
              black;width:67%;padding-left: 1em'>Strong M, Oakley JE, Brennan A. 
          Estimating multi-parameter partial Expected Value of 
          Perfect Information from a probabilistic sensitivity analysis sample: 
          a non-parametric regression approach. 
          <em>Medical Decision Making.</em> 2014;<b>34(3)</b>:311-26. Available open access 
              <a href='http://mdm.sagepub.com/content/34/3/311' target='_blank'>here.</a></div>")),
       br(),h3("Contact us"), 
       p(HTML("Please email us at <a href='mailto:savi@sheffield.ac.uk?Subject=SAVI%20query' 
          target='_top'> savi@sheffield.ac.uk</a>")),
       p("Please tell us about any bugs!"),
       br(),
#        p(HTML("<a href='http://www.nihr.ac.uk/' 
#                     target='_blank'><img src = 'NIHR-stamp-fund.png' width = '100' 
#                    alt='Funded by National Institute for Health Research' /></a>"), align = "right"),
#        
       h3(HTML("Funding acknowledgement")), 
       p(HTML("The method for partial EVPI computation that is implemented in 
              this web application arose from independent research 
              supported by the National Institute for Health Research (Mark Strong, 
              postdoctoral fellowship PDF-2012-05-258). 
              The views expressed in this publication are those 
              of the authors and not necessarily those of the National Health Service, 
              the National Institute for Health Research, or the Department of Health. 
              ")),
br(),  
p(HTML("This website complies with The University of Sheffield's 
          <a href='http://www.sheffield.ac.uk/privacy' target='_blank'>Privacy
         Policy</a>")),       

       p(HTML("<a href='http://www.nihr.ac.uk/' 
                    target='_blank'><img src = 'NIHR-Logo.png' width = '200' 
                   alt='Funded by National Institute for Health Research' /></a>"), align = "right")
    


      )
      
      
      
      
      
      
      ########################
      # CONTROL LOOK OF TABS #
      ########################
      ,  type = "pills" # this controls the look of the tabs
    ), 
    
    
    
    ##########
    # FOOTER #
    ##########
    
    tags$style(type="text/css", ".tab-content { overflow: visible; }", 
            "footer {background-color: #F8F8F8;
            width: 100%;
            bottom: 0;
            position: relative; }")
    , width = 12 # 12 is the max width of the mainPanel page
  )
  #,theme="bootstrap.css" # can style with bootstrap
  , title="SAVI - Sheffield Accelerated Value of Information") # BROWSER WINDOW TITLE

#can add id="tabId", before type = "pills"
