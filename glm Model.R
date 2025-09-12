
#Variables
# y=gambiae_count
# x=n_nets_per_hh, people_sleeping, has_net, sprayed,



#Build a glm for the counts
model_gambiae <- glm(gambiae_count ~ has_net + sprayed + 
                     people_sleeping + nets_owned,
                    data = sim_data,
                    family = "poisson")

model_funestus <- glm(funestus_count ~ has_net + sprayed + 
                       people_sleeping + nets_owned,
                     data = sim_data,
                     family = poisson(link = "log"))


summary(model_gambiae)
