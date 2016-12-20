# Solving facility location problem 

# https://en.wikipedia.org/wiki/Facility_location_problem

#''' We assume customers are located in a grid with euclidian distance'''

set.seed(1234)
grid_size <- 1000
n <- 100

customer_locations <- data.frame(
  id = 1:n,
  x = round(runif(n)*grid_size),
  y = round(runif(n)*grid_size)
)

#'''Warehouse are placed randomly on the grid'''

m <- 20
warehouse_locations <- data.frame(
  id = 1:m,
  x = round(runif(m)*grid_size),
  y = round(runif(m)*grid_size)
)


#'''Fixed cost for warehousing is generated randomly with mean fixed cost as 50000'''

fixedcost <- round(rnorm(m,mean = grid_size*10, sd= grid_size*5))

fixedcost

transport_cost <- function(i,j){
  customer <- customer_locations[i,]
  warehouse <- warehouse_locations[j,]
  round(sqrt((customer$x- warehouse$x)^2 + (customer$y - warehouse$y)^2))
}

#'''Plotting customer and warehouse location on a plot'''
library(ggplot2)

p <- ggplot(customer_locations, aes(x,y))+
  geom_point() +
  geom_point(data = warehouse_locations,color = "red",alpha = 0.5, shape = 17) +
  scale_x_continuous(limits = c(0,grid_size))+
  scale_y_continuous(limits = c(0,grid_size))+
  theme(axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), panel.grid = element_blank())

p + ggtitle("Warehouse Location Problem")

devtools::install_github("dirkschumacher/ompr")
devtools::install_github("dirkschumacher/ompr.roi")


library(ompr)
library(magrittr)

model <- MIPModel() %>%
  # 1 iff i gets assigned to warehouse j
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # 1 iff warehouse j is built
  add_variable(y[j], j = 1:m, type = "binary") %>%
  
  # maximize the preferences
  set_objective(sum_expr(transportcost(i, j) * x[i, j], i = 1:n, j = 1:m) + 
                  sum_expr(fixedcost[j] * y[j], j = 1:m), "min") %>%
  
  # every customer needs to be assigned to a warehouse
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>% 
  
  # if a customer is assigned to a warehouse, then this warehouse must be built
  add_constraint(x[i,j] <= y[j], i = 1:n, j = 1:m)

# Solution to model 

library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

suppressPackageStartupMessages(library(dplyr))
matching <- result %>% 
  get_solution(x[i,j]) %>%
  filter(value > .9) %>%  
  select(i, j)

# Plotting best possible locations 

plot_assignment <- matching %>% 
  inner_join(customer_locations, by = c("i" = "id")) %>% 
  inner_join(warehouse_locations, by = c("j" = "id"))


customer_count <- matching %>% group_by(j) %>% summarise(n = n()) %>% rename(id = j)

plot_warehouses <- warehouse_locations %>% 
  mutate(costs = fixedcost) %>% 
  inner_join(customer_count, by = "id") %>% 
  filter(id %in% unique(matching$j))

p + 
  geom_segment(data = plot_assignment, aes(x = x.y, y = y.y, xend = x.x, yend = y.x)) + 
  geom_point(data  = plot_warehouses, color = "red", size = 3, shape = 17) +
  ggrepel::geom_label_repel(data  = plot_warehouses, 
                            aes(label = paste0("fixed costs:", costs, "; customers: ", n)), 
                            size = 2, nudge_y = 20) + 
  gggtitle(paste0("Cost optimal warehouse locations and customer assignment"),
           "Big red triangles show warehouses that will be built, light red are unused warehouse locations. 
           Dots represent customers served by the respective warehouses.")