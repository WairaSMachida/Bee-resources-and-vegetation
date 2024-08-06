## =============================================================================
## =============================================================================
##           Linking vegetation structure, plant traits and bee traits      ----
## =============================================================================
## =============================================================================

## ---------------------------------
# Produced by Waira Saravia Machida
# 2024/07/22, modified at 2024/08/06
## ---------------------------------

# This script shows the information described in the section "Box 1. Diversity 
# of bee nesting strategies: Brazil as a case study" and "Concluding remarks" 
# from the paper of Machida et al. in prep.

## --------------
# Packeges ----
library(dplyr) 
library(here)
## --------------

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# Import and prepare BBTD
# I downloaded the .csv file from https://github.com/lgcarvalheiro/BBTD
bbtd <- read.csv(here("data","BBTD_01082024.csv"), 
                 h = T, sep = ",")

head(bbtd)
names(bbtd)

# Create a col with species names
bbtd$specie <- paste(bbtd$Genus, bbtd$Specific.Epithet)
head(bbtd)

# There are information of 2060 bees
nrow(bbtd)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----



# 1. How many bees nest above- or below-ground? ----
bee_nest <- bbtd %>%
  dplyr::select(Genus,Specific.Epithet,Nesting.Position,Nesting.Remarks,
                Nesting.Method,Nesting.Substrate,Nesting.Data.Accuracy)


nest_position <- bee_nest %>% 
  count(Nesting.Position)

# Calculate the percentage
nest_position$percentage <- (nest_position$n*100)/nrow(bee_nest)
nest_position



# 2. Types of nests substrate ----
# Separate the nesting substrate into five classes 
# Data used to construct the top-left bar of Figure I.

nest_modified <- bee_nest %>%
  mutate(Nesting.Substrate = case_when(
    Nesting.Substrate %in% c("insect nest; human-made structures",
                            "insect nest") ~ "Another animal nest",
    Nesting.Substrate %in% c("soil","soil; human-made structures") ~ "Soil",
    Nesting.Substrate %in% c("wood","wood; human-made structures",
                             "cavity independent; human-made structures",
                             "cavity independent") ~ "Woody plant",
    Nesting.Substrate %in% c("no information") ~ "No information",
    
    Nesting.Substrate %in% c("soil; wood", "soil; wood; human-made structures") ~ "Soil or/and wood",
    
    Nesting.Substrate %in% c("soil; insect nest","wood, bird nest",
                             "wood; insect nest; human-made structure",
                             "wood; human-made structures; insect nest",
                             "wood; insect nest",
                             "wood; insect nest; soil; human-made structures",
                             "wood; insect nest; soil") ~ "Soil or/and wood or/and another animal nest",
    TRUE ~ as.character(Nesting.Substrate) # if none of the above conditions 
    # are met, keep the original value
  ))

# Confirm the classes
nest_substrate <- nest_modified %>% 
  count(Nesting.Substrate)

# Calculate the percentage
nest_substrate$percentage <- (nest_substrate$n*100)/nrow(nest_modified)
nest_substrate

# Visualize the data
barplot(nest_substrate$percentage, 
        col=viridis::viridis(6), 
        border="white", 
        font.axis=1, 
        beside=T,
        names.arg = nest_substrate$Nesting.Substrate,
        las = 2, cex.names = 0.7)



# 3. Types of wood nesting ----

# Change the classes of nesting substrate to filter those that use wood
# Here we merge all the species that use wood
nest_modified2 <- bee_nest %>%
  mutate(Nesting.Substrate = case_when(
    Nesting.Substrate %in% c("wood; human-made structures",
                          "soil; wood","wood; insect nest",
                          "wood; insect nest; soil",
                          "wood; insect nest; human-made structure",
                          "wood; insect nest; soil; human-made structures",
                          "soil; wood; human-made structures",
                          "wood, bird nest",
                          "wood; human-made structures; insect nest",
                          "cavity independent",
                          "cavity independent; human-made structures") ~ "wood",
    Nesting.Substrate %in% c("insect nest",
                             "insect nest; human-made structures") ~ "insect nest",
    Nesting.Substrate %in% c("soil",
                             "soil; human-made structures") ~ "soil",
    Nesting.Substrate %in% c("soil; insect nest") ~ "soil and/or insect nest",
        TRUE ~ as.character(Nesting.Substrate) # if none of the above conditions 
    # are met, keep the original value
  ))



# 3.1 How many bees nest in the wood?
nest_subst_count <- nest_modified2 %>% 
  count(Nesting.Substrate) # 769 species that nest in wood

# Calculate the percentage
nest_subst_count$perc <- (nest_subst_count$n*100)/sum(nest_subst_count$n)
nest_subst_count



# 3.2 How about the dead wood?

nest_modified3 <- nest_modified2 %>%
  mutate(Nesting.Substrate = case_when(
    stringr::str_detect(Nesting.Remarks, "dead") ~ "dead wood",
    stringr::str_detect(Nesting.Remarks, "rott") ~ "dead wood",
    stringr::str_detect(Nesting.Remarks, "decay") ~ "dead wood",
    stringr::str_detect(Nesting.Remarks, "fence") ~ "dead wood",
    TRUE ~ as.character(Nesting.Substrate)))

# How many species nest in dead and alive wood?
nest_subst_count_wood <- nest_modified3 %>%
  count(Nesting.Substrate)

# Calculate the percentage
nest_subst_count_wood$percentage <- (nest_subst_count_wood$n*100)/nrow(nest_modified3)
nest_subst_count_wood



# 3.2.1 Within wood nesting, how many nest in dead and alive wood?

dead_alive_wood <- nest_modified3 %>% 
  filter(Nesting.Substrate == "wood" | Nesting.Substrate == "dead wood")

# How many species nest in dead and alive wood?
dead_alive_wood_count <- dead_alive_wood %>%
  count(Nesting.Substrate)

# Calculate the percentage
dead_alive_wood_count$percentage <- (dead_alive_wood_count$n*100)/nrow(dead_alive_wood)
dead_alive_wood_count



# 3.3 Within each group, how the nesting method varies?
# Alive wood
# Middle-bar of the Figure I

nest_alive_wood <- nest_modified3 %>% 
  filter(Nesting.Substrate == "wood")

# How many bees per nesting method?
nest_alive_wood_count <- nest_alive_wood %>% 
  count(Nesting.Method)

# Calculate the percentage
nest_alive_wood_count$percentage <- (nest_alive_wood_count$n*100)/
                                      nrow(nest_alive_wood)
nest_alive_wood_count

# Visualize the data
barplot(nest_alive_wood_count$percentage, 
        col=viridis::viridis(5), 
        border="white", 
        font.axis=1, 
        beside=T, 
        names.arg = nest_alive_wood_count$Nesting.Method,
        las = 1, cex.names = 0.7)



# Dead wood

nest_dead_wood <- nest_modified3 %>% 
  filter(Nesting.Substrate == "dead wood")

# How many bees per nesting method?
nest_dead_wood_count <- nest_dead_wood %>% 
  count(Nesting.Method)

# Calculate the percentage
nest_dead_wood_count$percentage <- (nest_dead_wood_count$n*100)/
  nrow(nest_dead_wood)
nest_dead_wood_count


# 4. Eltonian shortfalls ----

# Separate the species that the nesting information accuracy is not species
bee_nest_accur <- bee_nest %>%
  filter(Nesting.Data.Accuracy != "species") %>% 
  count(Nesting.Position)

# Calculate the percentage
bee_nest_accur$percentage <- (bee_nest_accur$n*100)/nrow(bee_nest)
bee_nest_accur

# calculate the percentage of accuracy at species level
100-sum(bee_nest_accur$percentage)

## =============================================================================
####                                  END                                   ####
## =============================================================================
