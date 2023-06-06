install.packages("decisionSupport")
library(decisionSupport)
?random.estimate1d
example_decision_inputs <- read.csv("data/example_decision_inputs.csv")
example_decision_inputs.url <- "https://raw.githubusercontent.com/CWWhitney/Decision_Analysis_Course/main/data/example_decision_inputs.csv"
example_decision_inputs <- read.csv(url(example_decision_inputs.url))
example_decision_model <- function(x, varnames) {
  
  profit <- benefits-costs
  
  final_profits <- profit + 500
  
}
mcSimulation(estimate = as.estimate(example_decision_inputs), 
             
             model_function = example_decision_model, 
             
             numberOfModelRuns = 100,
             
             functionSyntax = "plainNames")
mcSimulation(estimate = as.estimate(example_decision_inputs), 
             
             model_function = example_decision_model, 
             
             numberOfModelRuns = 700,
             
             functionSyntax = "plainNames")
install.packages("Diagrammer")
install.packages("DiagrammeR")
library(DiagrammeR)
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke:green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke:green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke:red, stroke-width:1.5px")
mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke:green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke:green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke:red, stroke-width:1.5px")
