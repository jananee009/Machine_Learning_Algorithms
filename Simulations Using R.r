# problem: One hundred people line up to board an airplane. Each has a boarding pass with
# assigned seat. However, the first person to board has lost his boarding pass and takes a random
# seat. After that, each person takes the assigned seat if it is unoccupied, and one of unoccupied
# seats at random otherwise. What is the probability that the last person to board gets to sit in
# his assigned seat?


number_of_simulations = 10000
seat_available_for_last_passenger = c() # list of seats available for last passenger.

# We assume that, as per boarding card, passenger 1 is assigned seat 1, passenger 2 is assigned seat 2 and so on.
for (s in 1:number_of_simulations){
	seats = c(1:100)
	
	seat_taken_by_passenger_1 = sample(seats,1,replace=FALSE) # first passenger selects a seat randomly and occupies it.
	seats = seats[seats != seat_taken_by_passenger_1] #  seats remaining after first passenger has taken a seat.
	
	for (passenger in 2:99){	
		if (is.element(passenger,seats) ){ # passeneger n checks if seat n is vacant. 
			seats = seats[seats != passenger]	# passenger n takes seat n. 
		}
		else { # seat n is already occupied. 
		seat_assigned = sample(seats,1,replace=FALSE) Hence, passenger n randomly picks a seat and occupies it.
		seats = seats[seats != seat_assigned]
	     }
	}		
	seat_available_for_last_passenger = c(seat_available_for_last_passenger,seats) 
}

number_of_times_last_passenger_gets_his_own_seat = sum(seat_available_for_last_passenger == 100)

required_probability = number_of_times_last_passenger_gets_his_own_seat / number_of_simulations


#  Problem: Mr. Smith works on the 13th floor of a 15 floor building. The only elevator moves continuously through floors 
# 1, 2, . . . , 15, 14, . . . , 2, 1, 2, . . . , except that it stops on a floor on which the button has been pressed. 
# Assume that time spent loading and unloading passengers is very small compared to the travelling time.
# Mr. Smith complains that at 5pm, when he wants to go home, the elevator almost always goes up when it stops on his floor. 
# What is the explanation?
# Now assume that the building has n elevators, which move independently. 
# Compute the proportion of time the first elevator on Mr. Smith’s floor moves up.

# simulation for first sub-question:
floors <- c(1:15)
favs <- c()
number_of_starting_pts <- 1000

for(j in 1:number_of_starting_pts)
{
  m <- 1000
  favorables = 0
  current_floor = sample(floors, 1)
  direction = sample(c("up", "down"), 1)
  
  for(i in 1:m)
  {
    if(direction == "up")
    {
      if(current_floor < 15)
      {
        current_floor = current_floor + 1    
      }
      else
      {
        current_floor = 14
        direction = "down"
      }
    }
    else 
    {
      if(current_floor > 1)
        current_floor = current_floor - 1    
      else
      {
        current_floor = 2
        direction = "up"
      }
    }
    if(current_floor > 13 
        | (current_floor == 13 & direction == "down")
       )
    {
      favorables = favorables + 1
    }
  }
  favs <- c(favs, favorables/m)
}
print(mean(favs))


#Newton–Pepys problem

#Which of the following three propositions has the greatest chance of success?
#A. Six fair dice are tossed independently and at least one “6” appears.
#B. Twelve fair dice are tossed independently and at least two “6”s appear.
#C. Eighteen fair dice are tossed independently and at least three “6”s appear

library(stringr)
throw_dice <- function(number_of_dice) {
 ret <- vector()
 for(i in 1:number_of_dice)
 {
   ret <- c(ret,  sample(1:6, size = 1, replace = TRUE) )
 }
 return(ret)
}

probability_of_atleast_ntimes_X <-function(number_of_dice, number_of_times_of_occurrence, x)
{
 replications = 10^5
 numberOf_atleast <-0
 
 for(i in 1:replications)
 {
   dices <- throw_dice(number_of_dice)
   str <- paste(dices, collapse='')
   strCount <- str_count(str, x)
   if(strCount >= number_of_times_of_occurrence)
   {
     numberOf_atleast = numberOf_atleast + 1
   }
 }
 prob_of_atleast= numberOf_atleast/replications
 return(prob_of_atleast)
}
     
print(probability_of_atleast_ntimes_X(6, 1, '6'))
print(probability_of_atleast_ntimes_X(12, 2, '6'))
print(probability_of_atleast_ntimes_X(18, 3, '6'))


# Three Prisoners problem
#Three prisoners, A, B and C, are in separate cells and sentenced to death. The governor has selected one of them at random to be pardoned. The warden knows which one is pardoned, but is not allowed to tell. Prisoner A begs the warden to let him know the identity of one of the others who is going to be executed. "If B is to be pardoned, give me C's name. If C is to be pardoned, give me B's name. And if I'm to be pardoned, flip a coin to decide whether to name B or C."
#The warden tells A that B is to be executed. Prisoner A is pleased because he believes that his probability of surviving has gone up from 1/3 to 1/2, as it is now between him and C. Prisoner A secretly tells C the news, who is also pleased, because he reasons that A still has a chance of 1/3 to be the pardoned one, but his chance has gone up to 2/3. What is the correct answer?

library(stringr)

prisoners <- c('A', 'B', 'C')
total_replications <- 0
selected_replications <- 0
pardoned_array <- vector()
replications = 10^5

for(i in 1:replications)
{
 pardoned <- sample(prisoners, 1)  
 if(pardoned == 'A')
 {
   pris = c(prisoners)
   executed <- sample(pris[pris != pardoned], 1)  
   if(executed == 'B')
   {
     selected_replications = selected_replications + 1
     pardoned_array <- c(pardoned_array, 'A')       
   }
 }
 else if(pardoned == 'B')
 {
   executed = 'C'
 }
 else if(pardoned == 'C')
 {
   executed ='B'
   selected_replications = selected_replications + 1
   pardoned_array <- c(pardoned_array, 'C')
 }
}

print(pardoned_array)
print(selected_replications)
str <- paste(pardoned_array, collapse='')
strCount <- str_count(str, 'A')
print(strCount/selected_replications)
strCount <- str_count(str, 'B')
print(strCount/selected_replications)
strCount <- str_count(str, 'C')
print(strCount/selected_replications)


# Problem: Alice attends a small college in which each class meets only once a week. She is deciding between 30 non overlapping classes. 
# There are 6 classes to choose from for each day of the week from Monday through Friday. Trusting in the benevolence of randomness, 
# Alice decides to register for 7 randomly selected classes out of 30 with all choices equally likely. 
# What is the probability that she will have a class everyday Monday through Friday? 

x = c("1A", "1B", "1C", "1D", "1E", "1F", "2A", "2B", "2C", "2D", "2E", "2F", "3A", "3B", "3C", "3D", "3E", "3F", "4A", "4B", "4C", "4D", "4E", "4F", "5A", "5B", "5C", "5D", "5E", "5F")
 
 mean(replicate(10^5, str_detect(paste(str_extract_all(sample(x, 7),"\\(?[0-9,.]+\\)?"), collapse=''), '^(?=.*1)(?=.*2)(?=.*3)(?=.*4)(?=.*5).*$')))


 # Problem: You have one fair coin, and one biased coin which lands heads with probability 3/4. You pick one of the coins at random and flip it 3 times. 
 # It lands Heads all 3 times. Given this information, what is the probability that the coin you picked is fair. 
m <- 10^5
count = 0
sel_coins <- vector()
for(i in 1:m)
{
  c <- c("f", "b")
  coin = sample(c) 
  if(coin[1] == "f")
  {
     tosses = sample(c("H", "T"), 3, replace=TRUE)
  }
  else
  {
    tosses = sample(c("H", "T"), 3, replace=TRUE, prob=c("0.75", "0.25"))
  }
  str = paste(tosses, collapse='')
  strCount <- str_count(str, "H") 
  if(strCount == 3)
  {
    sel_coins <- c(sel_coins, coin[1])
  }
}  
print(str_count(paste(sel_coins, collapse=''), "f")/length(sel_coins))

# Problem: Continuing with the above problem, if we toss the coin a 4th time, what is the probability that it will land Heads once more?
m <- 10^4
count = 0
sel_coins <- vector()
for(i in 1:m)
{
  c <- c("f", "b")
  coin = sample(c) 
  if(coin[1] == "f")
  {
     tosses = sample(c("H", "T"), 3, replace=TRUE)
  }
  else
  {
    tosses = sample(c("H", "T"), 3, replace=TRUE, prob=c("0.75", "0.25"))
  }
  str = paste(tosses, collapse='')
  strCount <- str_count(str, "H") 
  if(strCount == 3 && coin[1] == "f")
  {
    tosses = sample(c("H", "T"), 1, replace=TRUE)
    sel_coins <- c(sel_coins, tosses[1])
  }
  else if(strCount == 3 && coin[1] =='b')
  {
    tosses = sample(c("H", "T"), 1, replace=TRUE, prob=c("0.75", "0.25"))
    sel_coins <- c(sel_coins, tosses[1])
  }
}  
print(str_count(paste(sel_coins, collapse=''), "H")/length(sel_coins))

# Problem: A bag contains one marble which is either green or blue, with equal probabilities. 
# A green marble is put in the bag (so there are 2 marbles in the bag now), and then a random marble is taken out. The marble taken out is green.
# What is the probability that the remaining marble is also green ?
m <- 10^4
maincnt = 0
cnt = 0
for(i in 1:m)
{
  c <- c("g", "b")
  bag = sample(c, 1)
  bag <- c(bag, "g")
  
  sam = sample(1:length(bag), 1)

  if(bag[sam] == "g")
  {
    bag <- bag[-sam]
    maincnt = maincnt + 1
    if(bag[1] == "g")
    {
      cnt = cnt + 1
    }
  }


}
print(cnt/maincnt)



