#Seminar 6
install.packages("decisionSupport")
library(decisionSupport)
install.packages("DiagrammeR")
library(DiagrammeR)
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
#change to TD
mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        MC(Management cost)-->F; linkStyle 4 stroke: blue, stroke-width:1.5px")
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
#building the model
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost"),
                              lower = c(6000, 3, 500),
                              median = NA,
                              upper = c(14000, 8, 1000),
                              distribution = c("posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season"))

input_estimates
#add new variable Management_cost; lower bound 100; upper bound 2000; distribution "posnorm"; label "Management cost (USD/ha)"; 
#description "Management costs in a normal season".
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost", "Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm", "posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season", "Management costs in a normal season"))

input_estimates
#graphical impact pathway
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
#summarizes the Labor_cost and Management_cost into overall_costs, then subtract these from the income to calculate final_result
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
#histogram
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
#make_variables
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}
#apply make_variables as as.estimate
make_variables(as.estimate(input_estimates))

Market_price
make_variables(as.estimate(input_estimates))
Management_cost
Market_price
Labor_cost + Management_cost

