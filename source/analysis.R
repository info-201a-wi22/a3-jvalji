
og_incarceration_file <- read.csv("../source/incarceration_trends.csv", header = T)


#Consolidate original messy data (this step mostly makes this easier to view, but is technically unnecessary)
my_incarceration_file <- og_incarceration_file %>%
  drop_na() %>%
  arrange(year) %>%
  select(year,
         state,
         county_name,
         total_jail_pop,
         latinx_jail_pop,
         white_jail_pop)


#Extract specific data needed and create new table
CA_incarceration_file <- my_incarceration_file %>% group_by(year) %>% filter(state == "CA") %>% 
  select(year, latinx_jail_pop, white_jail_pop) %>%
  gather(key = race, value = population, -year, na.rm = TRUE) %>%
  group_by(year, race) %>%
  summarise(population = sum(population))


CA_incarceration_file$race <- CA_incarceration_file$race %>% 
  factor(levels = c("latinx_jail_pop", "white_jail_pop"))

#Charts
#Chart 1: Comparing latinx vs white jail incarcerations in CA overtime.
chart1 <- ggplot(CA_incarceration_file)+
geom_line(mapping = aes(x = year, y = population, color = race))+
geom_line(mapping = aes(x = year, y = population, color = race))+
scale_x_continuous(breaks = c(2000 : 2013))+
  labs(title = "Total CA Latinx vs. White Incarcerations from 2000-2013", 
     x = "year",
     y = "total incarcerations", fill = "incarcerations")
chart1

#Chart 2: Showing the relationship between latinx jail populations to white jail populations from the total jail incarcerations in CA in 2013.

        

CA_total_incars <- my_incarceration_file %>%
  filter(year == "2013") %>%
  filter(state == "CA") %>% 
  summarise(latinx_jail_pop = sum(latinx_jail_pop), total_jail_pop = sum(total_jail_pop))


#Pie Chart code
pie_chart2<- ggplot(CA_total_incars, aes(x = "", y = "latinx_jail_pop", fill = "latinx incarceration"))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = "percentage of latinx incarceration"))+
  coord_polar("y")+
  ggtitle("Latinx to Total Jail Populations, CA 2013")
pie_chart2

#Map: Comparing latinx incarceration rates to white incarceration rates nationwide  

#Create 2 tables for the map with a column showing the ratio of latinx jail populations to total jail populations for each state of the country.

## Year 2006 map table:
map_table_2006 <- my_incarceration_file %>% group_by(state) %>% filter(year == "2006") %>% 
  select(year, latinx_jail_pop, total_jail_pop) %>%
  mutate(ratio_latinx_incar = (latinx_jail_pop/total_jail_pop))

##Year 2013 map table:
map_table_2013 <- my_incarceration_file %>% group_by(state) %>% filter(year == "2013") %>% 
  select(year, latinx_jail_pop, total_jail_pop) %>%
  mutate(ratio_latinx_incar = (latinx_jail_pop/total_jail_pop))


#Create a map from each table (showing the ratio of national latinx jail populations to total jail populations in that year).
#Blank Theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )
##Year 2006 map:
map_1 <- plot_usmap(data = map_table_2006, values = "ratio_latinx_incar", colour = "white")+
  scale_fill_continuous(name = "latinx jail population", label = scales::comma)+
  theme(legend.position = "right")+
theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
      axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ 
  ggtitle('U.S. Map of Latinx Jail Population, 2006')
map_1
##Year 2013 map:
map_2 <- plot_usmap(data = map_table_2013, values = "ratio_latinx_incar", colour = "white")+
  scale_fill_continuous(name = "latinx jail population", label = scales::comma)+
  theme(legend.position = "right")+
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ 
  ggtitle('U.S. Map of Latinx Jail Population, 2013')
map_2
