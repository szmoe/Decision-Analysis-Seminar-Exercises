#Seminar 6: Model programming
install.packages("decisionSupport")
library(decisionSupport)
install.packages("DiagrammeR")
library(DiagrammeR)

# Plot the impact pathway ####

mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px")

#add management cost
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        MC(Management cost)-->F; linkStyle 4 stroke: blue, stroke-width:1.5px")

# LR argument in the mermaid function specifies that 
# the plot will go from left to right

# Change to TD
mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        MC(Management cost)-->F; linkStyle 4 stroke: blue, stroke-width:1.5px")

# Use the style options in mermaid to change the arrow widths to 2px and 
# the node colors to red for costs and green for benefits. 
# You will need to break the line and put the linkStyle on a new line
# to add the color to the node.

#change arrow width and node color
mermaid("graph LR
        Y(Yield)-->I(Income); style I fill: green
        linkStyle 0 stroke:green, stroke-width:2px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:2px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:2px
        CL(Labor cost)-->F; style CL fill: red
        linkStyle 3 stroke: red, stroke-width:2px
        MC(Management cost)-->F; style MC fill: red
        linkStyle 4 stroke: blue, stroke-width:2px")


# Building the model ####

input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost"),
                              lower = c(6000, 3, 500),
                              median = NA,
                              upper = c(14000, 8, 1000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", 
                                        "Labor cost (USD/ha)"),
                              Description = c(
                             "Yield in a sweet cherry farm under normal conditions",
                             "Price of sweet cherry in a normal season",
                             "Labor costs in a normal season")) 


input_estimates

# Add new variable Management_cost; lower bound 100; upper bound 2000;
# distribution "posnorm"; label "Management cost (USD/ha)"; 
# description "Management costs in a normal season".
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost",
                                           "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", 
                                               "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)",
                                        "Labor cost (USD/ha)", 
                                        "Management cost (USD/ha)"),
                              Description = c(
                         "Yield in a sweet cherry farm under normal conditions",
                        "Price of sweet cherry in a normal season",
                         "Labor costs in a normal season", 
                         "Management costs in a normal season"))

input_estimates

# Here we use the mcSimulation function from the decisionSupport package to
# implement a model (Luedeling et al. 2022). The model function that describes
# the graphical impact pathway. 
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Estimate the final results from the model
  final_result <- income - Labor_cost
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")

example_mc_simulation

# Add a new line of code that summarizes the Labor_cost and Management_cost 
# into overall_costs, then subtract these from the income
# to calculate final_result
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  # Estimate the overall cost
  overall_costs <- Labor_cost + Management_cost

  # Estimate the final results from the model
  final_result <- income - overall_costs
  
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                      model_function = model_function,
                                      numberOfModelRuns = 800,
                                      functionSyntax = "plainNames")

example_mc_simulation

# Here we show the results of a Monte Carlo simulation (800 model runs)
# for estimating the profits in sweet cherry orchards.
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

# Change the plot to a histogram by using the method argument
# in the plot_distributions function.
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

# Testing with make_variables ####

# You could simply start further developing the decision model now, 
# but since the model function will be designed to make use of variables 
# provided to it externally (random numbers drawn according to 
# the information in the data table), you will need to define 
# sample values for all variables, if you want to test pieces of 
# the function code during the development process. 
# This can be done manually, but it’s more easily accomplished with 
# the following helper function make_variables:

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

# Applying make_variables and as.estimate to the data table 
# (with default setting n=1) generates one random number for each variable,
# which then allows you to easily test the code you are developing.
# Try running this function on your code as you build the decision function.
# This allows for testing the values within a model rather 
# than running the full model.

# Run the make_variables and as.estimate on the input_estimates input table
# that we created and then calculate the result of Labor_cost + Management_cost
# given a single random value for these variables. 
# Note that each time you run this code it generates a new random draw
# and produces a different number from within the range for the variables 
# in the input table.

make_variables(as.estimate(input_estimates))

Market_price

make_variables(as.estimate(input_estimates))

Labor_cost + Management_cost

# Applying the mcSimulation function in decision support ####
# https://cran.r-project.org/web/packages/decisionSupport/vignettes/example_decision_function.html

#Rmarkdown: https://raw.githubusercontent.com/eikeluedeling/decisionSupport/master/vignettes/example_decision_function.Rmd

#Example Burkina Reservoir Sedimentation

# We use the decisionSupport functions vv() to produce time series 
# with variation from a pre-defined mean and coefficient of variation,
# chance_event() to simulate whether events occur and discount() to
# discount values along a time series.

example_decision_function <- function(x, varnames){
  
  # calculate ex-ante risks: impact the implementation of interventions ####
  intervention_NonPopInvolvEvent <-
    chance_event(intervention_NonPopInvolv, 1, 0, n = 1)
  
  # pre-calculation of common random draws for all intervention model runs ####
  
  # profits from Tropical Livestock Units (TLU)
  TLU <- vv(TLU_no_intervention, var_CV, n_years)
  TLU_profit <- vv(profit_per_TLU, var_CV, n_years)
  
  # benefits of fruit
  precalc_intervention_fruit_benefits <-
    vv(intervention_fruit_area_ha, var_CV, n_years) *
    vv(intervention_fruit_yield_t_ha, var_CV, n_years) *
    vv(intervention_fruit_profit_USD_t, var_CV, n_years)
  
  # benefits of vegetables
  precalc_intervention_vegetable_benefits <-
    vv(intervention_vegetable_area_ha, var_CV, n_years) *
    vv(intervention_vegetable_yield_t_ha, var_CV, n_years) *
    vv(intervention_vegetable_profit_USD_t, var_CV, n_years)
  
  # benefits of rainfed crops
  precalc_intervention_rainfed_crop_benefits <-
    vv(intervention_rainfed_crop_area_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_yield_t_ha, var_CV, n_years) *
    vv(intervention_rainfed_crop_profit_USD_t, var_CV, n_years)
  
  #  Intervention ####
  
  for (decision_intervention_strips in c(FALSE,TRUE))
  {
    
    if (decision_intervention_strips)
    {
      intervention_strips <- TRUE
      intervention_strips_PlanningCost <- TRUE
      intervention_strips_cost <- TRUE
    } else
    {
      intervention_strips <- FALSE
      intervention_strips_PlanningCost <- FALSE
      intervention_strips_cost <- FALSE
    }
    
    if (intervention_NonPopInvolvEvent) {
      intervention_strips <- FALSE
      intervention_strips_cost <- FALSE
    }
    
    # Costs ####
    if (intervention_strips_cost) {
      cost_intervention_strips <-
        intervention_adaptation_cost + intervention_tech_devices_cost + intervention_nursery_cost +
        intervention_wells_cost +
        intervention_training_cost + intervention_mngmt_oprt_cost + intervention_mngmt_follow_cost +
        intervention_mngmt_audit_cost
    } else
      cost_intervention_strips <- 0
    
    if (intervention_strips_PlanningCost) {
      plan_cost_intervention_strips <-
        intervention_communication_cost + intervention_zoning_cost
    } else
      plan_cost_intervention_strips <- 0
    
    maintenance_cost <- rep(0, n_years)
    
    if (intervention_strips)
      maintenance_cost <-
      maintenance_cost + vv(maintenance_intervention_strips, var_CV, n_years)
    
    intervention_cost <- maintenance_cost
    intervention_cost[1] <-
      intervention_cost[1] + cost_intervention_strips + plan_cost_intervention_strips
    
    
    # Benefits from  cultivation in the intervention strips ####
    
    intervention_fruit_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_fruit_benefits
    intervention_vegetable_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_vegetable_benefits
    intervention_rainfed_crop_benefits <-
      as.numeric(intervention_strips) * precalc_intervention_rainfed_crop_benefits
    
    # Total benefits from crop production (agricultural development and riparian zone) ####
    crop_production <-
      intervention_fruit_benefits +
      intervention_vegetable_benefits +
      intervention_rainfed_crop_benefits
    
    # Benefits from livestock ####
    # The following allows considering that intervention strips may
    # restrict access to the reservoir for livestock.
    
    if (intervention_strips)
      TLU_intervention <-
      TLU * (1 + change_TLU_intervention_perc / 100)
    else
      TLU_intervention <- TLU
    
    if (decision_intervention_strips){
      livestock_benefits <- TLU_intervention * TLU_profit
      total_benefits <- crop_production + livestock_benefits
      net_benefits <- total_benefits - intervention_cost
      result_interv <- net_benefits}
    
    
    if (!decision_intervention_strips){
      livestock_benefits <- TLU_no_intervention * TLU_profit
      total_benefits <- livestock_benefits
      net_benefits <- total_benefits - intervention_cost
      result_n_interv <- net_benefits}
    
  } #close intervention loop bracket
  
  NPV_interv <-
    discount(result_interv, discount_rate, calculate_NPV = TRUE)
  
  NPV_n_interv <-
    discount(result_n_interv, discount_rate, calculate_NPV = TRUE)
  
  # Beware, if you do not name your outputs (left-hand side of the equal sign) in the return section, 
  # the variables will be called output_1, _2, etc.
  
  return(list(Interv_NPV = NPV_interv,
              NO_Interv_NPV = NPV_n_interv,
              NPV_decision_do = NPV_interv - NPV_n_interv,
              Cashflow_decision_do = result_interv - result_n_interv))
}

# Perform a Monte Carlo simulation ####

# The numberOfModelRuns argument is an integer indicating the number of
# model runs for the Monte Carlo simulation. Unless the model function
# is very complex, 10,000 runs is a reasonable choice 
# (for complex models, 10,000 model runs can take a while, 
# so especially when the model is still under development, 
# it often makes sense to use a lower number).
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("https://raw.githubusercontent.com/eikeluedeling/decisionSupport/master/vignettes/example_input_table.csv"),
  model_function = example_decision_function,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)

mcSimulation_results

# Plot Net Present Value (NPV) distributions ####

# We can use the plot_distributions() function to produce one of 
# the several plotting options for distribution outputs. 
# This shows us an overlay of the full results of 
# the Monte Carlo model of the decision options, i.e. 
# the expected NPV if we choose to do the intervention Interv_NPV or 
# not do the intervention NO_Interv_NPV.
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV", "NO_Interv_NPV"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

# We use the same function to show the distributions of the ‘do’ and
# ‘do not do’ decision scenarios as boxplots. 
# This can be useful when comparing multiple outputs by 
# illustrating the spread of the data resulting from the decision model.
# Boxplots show the median (central line), the 25th and 75th percentiles
# (sides of boxes) and any outliers (light circles outside of boxes).
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Interv_NPV",
                                             "NO_Interv_NPV"),
                                    method = 'boxplot')

# We can use the same function for the value of the decision 
# (difference in NPV between do and do not do). This is more helpful for us
# since it shows us the outcome distribution of the decision itself.
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_do",
                                    method = 'boxplot_density')
# Cashflow Analysis ####

# Here we plot the distribution of annual cashflow over the entire 
# simulated period for the intervention. For this we use the plot_cashflow() 
# function which uses the specified cashflow outputs from 
# the mcSimulation() function (in our case Cashflow_decision_do)
# to show cashflow over time.
plot_cashflow(mcSimulation_object = mcSimulation_results, 
              cashflow_var_name = "Cashflow_decision_do")

# Projection to Latent Structures (PLS) analysis ####

# We apply a post-hoc analysis to the mcSimulation() outputs with 
# plsr.mcSimulation() to determine the Variable Importance in 
# the Projection (VIP) score and coefficients of a Projection to
# Latent Structures (PLS) regression model. This function uses the outputs of
# the mcSimulation() selecting all the input variables from 
# the decision analysis function in the parameter object and 
# then runs a PLS regression with an outcome variable defined 
# in the parameter resultName. We use the code names(mcSimulation_results$y)[3]
# to select the outcome variable NPV_decision_do, which is the third element of
# the list y in our mcSimulation_results outputs 
# (this must be a character element).
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3],
                                ncomp = 1)

pls_result

# We run the plot_pls() on the results from plsr.mcSimulation() with a number 
# of standard settings. The length of the bars is equal to VIP with 
# a vertical line at ‘1’ on the x-axis indicating a standard cut-off for 
# VIP used for variable selection (Whitney et al. 2017). 
# The overall plot only shows those variables with a VIP > 0.8, 
# which is the common threshold for variable selection E. Luedeling 
# and Shepherd (2016). The colors of the bars represent the positive 
# or negative coefficient of the given input variable with the output variable.

# Here we import the input table again to replace the labels for the variables 
# on the y-axis. The input table can include a ‘label’ and ‘variable’ column. 
# The standard labels (from the ‘variable’ column) are usually 
# computer readable and not very nice for a plot. The plot_pls() function uses 
# the text in the ‘label’ column as replacement for the default text 
# in the ‘variable’ column.
input_table <- read.csv("https://raw.githubusercontent.com/eikeluedeling/decisionSupport/master/vignettes/example_input_table.csv")

plot_pls(pls_result, input_table = input_table, threshold = 0)

# Value of Information (Vol) analysis ####

# We calculate Value of Information (VoI) analysis with the Expected Value of
# Perfect Information (EVPI). EVPI measures the expected opportunity loss that 
# is incurred when the decision-maker does not have perfect information about 
# a particular variable. EVPI is determined by examining the influence of 
# that variable on the output value of a decision model.

# We use the function data.frame() to transform the x and y outputs of
# the mcSimulation() function for EVPI calculation. We use the multi_EVPI() 
# function to calculate the EVPI for multiple independent variables.
# For the first_out_var argument we choose ‘intervention_mngmt_audit_cost’ 
# from the input table since this is the first variable after the NPV and 
# cashflow model outputs, which we would like to exclude from the EVPI analysis.

#here we subset the outputs from the mcSimulation function (y) by 
# selecting the correct variables
# this should be done by the user (be sure to run the multi_EVPI only 
# on the variables that the user wants)
mcSimulation_table <- data.frame(mcSimulation_results$x, 
                                 mcSimulation_results$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Interv_NPV")
#> [1] "Processing 3 output variables. This can take some time."
#> [1] "Output variable 1 (Interv_NPV) completed."
#> [1] "Output variable 2 (NO_Interv_NPV) completed."
#> [1] "Output variable 3 (NPV_decision_do) completed."

# We use the function plot_evpi() on the results from multi_EVPI() 
# to plot the Expected Value of Perfect Information (EVPI). Here we show 
# the results with the standard settings. The length of the bars 
# is equal to EVPI.
plot_evpi(evpi, decision_vars = "NPV_decision_do")
#> Warning: There are no variables with a positive EVPI. 
#> You probably do not need a plot for that.

# Finally, we provide a single function for a quick assessment. 
# The compound_figure() function can be used to run the full decision assessment
# for a simple binary decision (‘do’ or ‘do not do’).
compound_figure(mcSimulation_object = mcSimulation_results, 
                input_table = input_table, 
                plsrResults = pls_result, 
                EVPIresults = evpi, 
                decision_var_name = "NPV_decision_do", 
                cashflow_var_name = "Cashflow_decision_do", 
                base_size = 7)
#> Warning: There are no variables with a positive EVPI. 
#> You probably do not need a plot for that
