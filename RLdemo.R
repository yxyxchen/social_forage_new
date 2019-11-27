# in this demo, I test different RL increment rules in the prey selection task 
# it seems that all increment ruls are influenced by short term effects 
# they are also influenced by the orfer effects. 


# paras 
initial = 0.8
alpha = 0.2

# I consider two senarios:
# get 3 after 4 secs and 4 again after 5 secs 
# get 8 after 8 secs 


# learn at each time step, Neil, 2019
junk = initial * (1-beta)^3 + beta * 5
post1 = junk  * (1-beta)^5 + beta * 3
post2 = initial * (1 - beta) ^ 8 + beta * 8
c(post1, post2)


# constantino, 2015
junk = initial * (1 - beta)^3 + (1 - (1 - beta)^3) * (5/3)
post1 = junk * (1 - beta)^5 + (1 - (1 - beta) ^ 5) * (3/5)
post2 = initial * (1 - beta)^8 + (1 - (1 - beta)^8) * (8/8)
c(post1, post2)

# R-learning 
junk = initial  + beta * (4 - initial * 4)
post1 = junk  + beta * (4 - junk * 4)
post2 =  initial  + beta * (8 - initial * 8) 
c(post1, post2)
