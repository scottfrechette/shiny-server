library(shiny)

roll_attack <- function(horde_size, attack_bonus, roll_type) {
  
  attack_rolls_a <- sample(1:20, horde_size, replace = T) + attack_bonus
  attack_rolls_b <- sample(1:20, horde_size, replace = T) + attack_bonus
  ifelse(roll_type == "Normal", 
         attack_rolls <- attack_rolls_a,
         ifelse(roll_type == "Disadvantage", 
                attack_rolls <- pmin(attack_rolls_a, attack_rolls_b),
                attack_rolls <- pmax(attack_rolls_a, attack_rolls_b)))
  
  return(attack_rolls)
  
}
roll_damage <- function(horde_size, damage_die, damage_bonus) {
  
  sample(1:size_damage_die, horde_size, replace = T) + damage_bonus
  
}
calc_total_ac <- function(ac, buffs, shield) {ac + buffs + (shield * 5)}
calc_num_hits <- function(attack_rolls, total_ac) {sum(attack_rolls > total_ac)}
calc_damage <- function(attack_rolls, total_ac, damage_rolls) {
  
  sum((attack_rolls > total_ac) * damage_rolls)
  
}
sim_attack <- function(horde_size, roll_type, attack_bonus,
                       damage_die, damage_bonus,
                       ac, buffs, shield,
                       verbose = F) {
  
  attack_rolls <- roll_attack(horde_size, attack_bonus, roll_type)
  damage_rolls <- roll_damage(horde_size, damage_die, damage_bonus)
  total_ac <- calc_total_ac(ac, buffs, shield)
  hits <- calc_num_hits(attack_rolls, total_ac)
  damage <- calc_damage(attack_rolls, total_ac, damage_rolls)
  
  out <- c(damage,
           paste(hits, "enemies landed a blow for total damage of", damage))
  
  if(!verbose) out <- as.integer(out[1])
  
  return(out)
  
}
sim_many_attacks <- function(sims = 100,
                             horde_size, roll_type, attack_bonus,
                             damage_die, damage_bonus, 
                             ac, buffs, shield) {
  
  replicate(sims, 
            sim_attack(horde_size, roll_type, attack_bonus,
                       damage_die, damage_bonus, 
                       ac, buffs, shield))
  
}

ui <- fluidPage(

    titlePanel("Horde Simulation"),

    sidebarLayout(
        sidebarPanel(
            ## PC
            numericInput("ac", "AC", 15),
            numericInput("buffs", "Buffs/Debuffs", 0),
            checkboxInput("shield", "Shield"),

            ## Horde
            numericInput("horde_size", "Horde Size", 10),
            selectInput("roll_type", "Roll Type", c("Advantage", "Normal", "Disadvantage"), selected = "Normal"),
            numericInput("attack_bonus", "Attack Bonus", 4),
            numericInput("num_damage_die", "# Damage Die", 1),
            numericInput("size_damage_die", "Damage Die", 6),
            numericInput("damage_bonus", "Damage Bonus", 0)#,
            
            ## Submit
            # submitButton("Roll Die")

        ),

        mainPanel(textOutput("attack"),
                  hr(),
                  plotOutput("sim_plot"))
    )
)

server <- function(input, output) {

    sim <- reactive(sim_attack(input$horde_size, input$roll_type, input$attack_bonus,
                                  input$size_damage_die, input$damage_bonus,
                                  input$ac, input$buffs, input$shield,
                                  verbose = T))
    
    sims <- reactive(sim_many_attacks(sims = 1e4,
                                      input$horde_size, input$roll_type, input$attack_bonus,
                                      input$size_damage_die, input$damage_bonus,
                                      input$ac, input$buffs, input$shield))
    
    output$attack <- renderText(sim()[2])
    
    output$sim_plot <- renderPlot({
      
      hist(sims(),
           breaks = max(sims()) - min(sims()),
           border = 'white',
           xlab = "Damage",
           ylab = NULL,
           yaxt = "n",
           main = "Where does this roll fall among 10,000 simulations?")
      abline(v = as.integer(sim()[1]), col="red", lwd=3)
      
    })
  
}

shinyApp(ui = ui, server = server)
