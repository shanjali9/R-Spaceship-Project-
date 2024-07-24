---
  editor_options: 
  markdown: 
  wrap: sentence
---
  
```{r}
spaceship <- read_csv("files/spaceship-titanic/train.csv")

view(spaceship)
```

What we know:
  
1.  almost half of these passengers were transported to another dimension during an anomaly
2.  these records are from before the anomaly, and were recovered from the damaged computer system
3.  it is possible to predict who was transported based off the information given

Interesting columns:
  
1.  Transported - boolean - TRUE if transported, FALSE if not
2.  HomePlanet - discrete
3.  CryoSleep - boolean - TRUE if they were in cryo sleep in their cabin for the entire voyage, including when the anomaly occurred
4.  Cabin - deck / num / side (port/starboard) - location on ship may have mattered if in cryo
5.  Age - integer - discrete

Let's start by graphing some of these variables to get a sense of the data.

```{r}
# how many people were transported?
ggplot(data = spaceship)+
  geom_bar(mapping = aes(x = Transported))

# passengers were very evenly split; about half were transported
```
```{r}
# HomePlanet (discrete) vs. Age (continuous) (vs. Transported bool)
ggplot(data = spaceship) +
  geom_boxplot(mapping = aes(x = HomePlanet, y = Age, color = Transported))

# no trends jump out here
```

```{r}
# another way of visualizing age; histogram with transported line layered over?
ggplot(data = spaceship)+
  geom_histogram(mapping = aes(x = Age, fill = Transported))

# very young passengers seem to be more likely to be transported (<12)
```
```{r}
# how about just age vs. home planet
ggplot(data = spaceship) +
  geom_boxplot(mapping = aes(x = Age, y = HomePlanet))

# oldest group was from Europa, youngest was Earth but there were many old outliers
# no strong trends in transported, slight trend towards younger crew, but not from any particular planet
```

```{r}
# lets see what fraction of cryo and non cryo got transported
ggplot(data = spaceship) +
  geom_bar(mapping = aes(x = CryoSleep, fill = Transported))
# looks like those in cryo were significantly more likely to be transported!

# cryo sleepers made up a significant fraction of the transported passengers (~50-60%) even though they made up only about 1/3 of total passengers
ggplot(data = spaceship)+
  geom_bar(mapping = aes(x = Transported, fill = CryoSleep))
```
```{r}
# how can we map cabin location on the ship to whether people were transported?
# mutate cabin position to several columns?
# would be interesting to see if there are any location trends in the cryo transports since they were the only ones we know were in their cabins at the time of the anomaly
spaceship <- spaceship %>% separate(Cabin, c("Deck", "Cabin Number", "Side", sep = "/", remove=FALSE))

ggplot(data = spaceship)+
  geom_bar(mapping = aes(x = Side, fill = Transported, na.rm = TRUE))

ggplot(data = spaceship)+
  geom_bar(mapping = aes(x = Deck, fill = Transported, na.rm = TRUE))+
  facet_wrap(~Side, nrow = 2)

# slightly more people who had cabins on the Starboard side of the ship were transported, seems to be true over most decks of the ship
# what about if we filtered for those on cryo?
spaceship_cryo <- spaceship %>%
filter(CryoSleep == TRUE)

ggplot(data = spaceship_cryo)+
  geom_bar(mapping = aes(x = Deck, fill = Transported, na.rm = TRUE))+
  facet_wrap(~Side, nrow = 2)

# a very high percentage of the cryo passengers were transported, EXCEPT for those on the E and G decks, with G being the largest deck and weighting our previous looks at how many passengers had transported from cryo
```
Let's explore the less obvious variables:
  
-   destination

-   VIP Status

-   amount spent on luxury amenities

```{r}
# destination vs. transported
ggplot(data = spaceship)+
  geom_bar(mapping = aes(x = Destination, fill = Transported, na.rm = TRUE))

# transported passengers more or less evenly split between destinations
```

```{r}
# VIP
ggplot(data = spaceship)+
  geom_bar(mapping = aes(x = VIP, fill = Transported, na.rm = TRUE))

# no trend; evenly split 
```
```{r}
# Room Services
# people in cryo didn't use room services
spaceship_awake <- spaceship %>%
  filter(CryoSleep == FALSE)

spaceship_awake_ameneties <- spaceship_awake %>%
  mutate(spaceship_awake,
         ameneties_total = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck
  )

ggplot(data = spaceship_awake_ameneties)+
  geom_boxplot(mapping = aes(x = ameneties_total, y = Transported))

ggplot(data = spaceship_awake_ameneties)+
  geom_col(mapping = aes(x = ameneties_total, y = Transported))

# this could be misleading if a few people spent a lot on ameneties and didn't get transported
# might be better to look at if a person spent money on ameneties at all, and which ameneties they frequented
```

```{r}
# lets make a boolean if a person spent money at any amenity, again using awake people since all cryo passengers spent 0
spaceship_awake_amenity_bool <- spaceship_awake_ameneties %>%
  mutate(amenity_bool = case_when(ameneties_total > 0 ~ TRUE, 
                                  ameneties_total == 0 ~ FALSE))
ggplot(data = spaceship_awake_amenity_bool)+
  geom_bar(mapping = aes(x = amenity_bool, fill = Transported, na.rm = TRUE))

spaceship_awake_no_ameneties <- spaceship_awake_amenity_bool %>%
  + filter(amenity_bool == FALSE)

# we have more damaged records than we have records of awake people who didn't spend money at an amenity, raises questions of the validity of this graph, but we can assume that damaged records would have equally affected transported and non-transported passengers records
# A higher percentage of people who chose not to spend money at the ameneties were transported than those who did spend money at the ameneties
# if we assume that if people were not at the ameneties, they spent more time in their rooms, then we could filter the dataset to just the awake passengers who did not spend money at the ameneties and do a similar analysis to our cabin mapping of cryo sleepers, to see if location on the ship was a primary factor in transportation
```

```{r}
spaceship_transported_1 <- spaceship %>%
  mutate(spaceship, ameneties_total = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck)

spaceship_transported_2 <- spaceship_transported_1 %>%
  mutate(spaceship, ameneties_bool = case_when(ameneties_total > 0 ~ TRUE, ameneties_total == 0 ~ FALSE))

ggplot(data = spaceship_transported_2)+
  geom_bar(mapping = aes(x = ameneties_bool, fill = CryoSleep, na.rm = TRUE))+
  facet_wrap(~Transported, nrow = 2)

ggplot(data = spaceship_awake_no_ameneties)+
  geom_bar(mapping = aes(x = Deck, fill = Transported, na.rm = TRUE))+
  facet_wrap(~Side, nrow = 2)

ggplot(data = spaceship)+
  geom_bar(mapping = aes(x = Deck, fill = Transported, na.rm = TRUE))+
  facet_wrap(~Side, nrow = 2)

# amongst awake people not spending money on ameneties, we see a similar trend to the cryo passengers; the G and E of the ship seemed to have far more of the transported passengers
# here we also see that awake passengers on port side of the B deck of the ship were also more affected
```

Final Conclusions:
  
1.  Home planet, destination, and VIP status seemed to have an insignificant relation to whether or not a passenger was transported
2.  Teenage passengers seem to be more likely to be transported
3.  Passengers in cryo-sleep in their cabins on decks G and E made up about 60% of the total transported passengers
4.  Awake passengers who spent on amenities were another significant block of transported passengers
