# The function performs a univariate EDA. It shows us the main aspects of the data we have:
Uni_EDA <- function(dat){

  p2<- dat %>% ggplot(aes(x = Item_Weight)) + geom_histogram(bins = 50, fill = "darkgreen") + 
    theme_economist_white() + xlab ("Weight") + ylab("Count")
  
  p3 <- dat %>% ggplot(aes(x = Item_Visibility)) + geom_histogram(bins = 100, fill = "darkgreen") + 
    theme_economist_white() + xlab ("Item Visibility") + ylab("Count")
  
  p4 <- dat %>% ggplot(aes(x = Item_MRP)) + geom_histogram(bins = 200, fill = "darkgreen") + 
    theme_economist_white() + xlab ("Item MRP") + ylab("Count")
  
  p5 <- dat %>% group_by(Item_Type) %>% summarise(Count = n()) %>%
    ggplot() + 
    geom_bar(aes(factor(Item_Type), Count), stat = "identity", fill = "coral1") +
    geom_label(aes(factor(Item_Type), Count, label = Count), vjust = 1) +
    theme_economist_white() + ggtitle ("Item Type") + xlab("") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0.01))
  
  
  p6 <- dat %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n()) %>% 
    ggplot() + 
    geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
    geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 1) +
    xlab("Outlet_Establishment_Year") +
    theme_economist_white()
  
  p7 <- dat %>% group_by(Outlet_Type) %>% summarise(Count = n()) %>%
    ggplot() + 
    geom_bar(aes(factor(Outlet_Type), Count), stat = "identity", fill = "coral1") +
    geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 1) +
    theme_economist_white() + xlab ("Outlet Type")
  
  p8 <- dat %>% group_by(Outlet_Size) %>% summarise(Count = n()) %>%
    ggplot() + 
    geom_bar(aes(factor(Outlet_Size), Count), stat = "identity", fill = "coral1") +
    geom_label(aes(factor(Outlet_Size), Count, label = Count), vjust = 1) +
    theme_economist_white() + xlab ("Outlet Size")
  
  p9 <- dat %>% group_by(Outlet_Location_Type) %>% summarise(Count = n()) %>%
    ggplot() + 
    geom_bar(aes(factor(Outlet_Location_Type), Count), stat = "identity", fill = "coral1") +
    geom_label(aes(factor(Outlet_Location_Type), Count, label = Count), vjust = 1) +
    theme_economist_white() + xlab ("Outlet Location")
  
  
  p10 <- dat %>% group_by(Item_Fat_Content) %>% summarise(Count = n()) %>%
    ggplot() + 
    geom_bar(aes(factor(Item_Fat_Content), Count), stat = "identity", fill = "coral1") +
    geom_label(aes(factor(Item_Fat_Content), Count, label = Count), vjust = 1) +
    theme_economist_white() + xlab ("Item Fat Content")
  
  first_row <- plot_grid(p2,p3,p4, nrow = 1)
  second_row <- plot_grid(p8,p9,p10, nrow = 1)
  grid <- plot_grid(p6,p7,first_row,second_row, ncol = 1, 
            scale = 1)
  Uni_EDA <- list(grid, p5)
}

fix_data <- function(dat){
  dat$Item_Fat_Content[dat$Item_Fat_Content == "LF"] = "Low Fat"
  dat$Item_Fat_Content[dat$Item_Fat_Content == "low fat"] = "Low Fat"
  dat$Item_Fat_Content[dat$Item_Fat_Content == "reg"] = "Regular"
  
  # Simply put the missing data of outlet size into category Small
  dat$Outlet_Size[dat$Outlet_Size == ""] = "Small"
  # In item visibility we notice many values at 0. 
  # We substitute them with the mean
  #Find the mean value of the ItemVisibility without the values at 0.
  mean_vis <- dat %>% filter(Item_Visibility>0) %>%
    summarize(mean(Item_Visibility))
  dat$Item_Visibility[dat$Item_Visibility == 0] = as.numeric(mean_vis)
  fix_data <- dat
  
}


