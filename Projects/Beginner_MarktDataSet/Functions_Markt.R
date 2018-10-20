# The function performs a univariate EDA. It shows us the main aspects of the data we have:
Uni_EDA <- function(dat){

  p2<- dat %>% ggplot(aes(x = Item_Weight)) + geom_histogram(bins = 100, fill = "darkgreen") + 
    theme_economist_white() + xlab ("Weight") + ylab("Count")
  
  p3 <- dat %>% ggplot(aes(x = Item_Visibility)) + geom_histogram(bins = 100, fill = "darkgreen") + 
    theme_economist_white() + xlab ("Item Visibility") + ylab("Count")
  
  p4 <- dat %>% ggplot(aes(x = Item_MRP)) + geom_histogram(bins = 100, fill = "darkgreen") + 
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
  zero_index = which(dat$Item_Visibility == 0)
  for(i in zero_index){
    item = dat$Item_Identifier[i]
    dat$Item_Visibility[i] = mean(dat$Item_Visibility[dat$Item_Identifier == item], na.rm = TRUE)
  }
  
  # Missing values in Item_Weight:
  #Replace with the mean value 
  index_na_weight <- which(is.na(dat$Item_Weight))
  for (i in index_na_weight){
    item <- dat$Item_Identifier[i]
    avg_weight <- mean(dat$Item_Weight[dat$Item_Identifier == item], na.rm = TRUE)
    dat$Item_Weight[i] <- avg_weight
  }

  fix_data <- dat
  
}


Bivariate_EDA <- function(dt){
  #Categorical Variables:
 p1<- dt %>% group_by(Item_Fat_Content) %>% 
    ggplot(aes(factor(Item_Fat_Content),Item_Outlet_Sales)) +
    geom_violin(fill = "coral1") + theme_economist_white() + 
    xlab("Item Fat Content") + ylab("Sales") + 
    ggtitle("Item Fat Content vs. Sales")
  
  
 p2<- dt %>% group_by(Outlet_Establishment_Year) %>% 
    ggplot(aes(factor(Outlet_Establishment_Year),Item_Outlet_Sales)) +
    geom_violin(fill = "coral1") + theme_economist_white() + 
    xlab("Establishment Year") + ylab("Sales") + 
    ggtitle("Establishment Year vs. Sales")
  
 p3<- dt %>% group_by(Outlet_Size) %>% 
    ggplot(aes(factor(Outlet_Size),Item_Outlet_Sales)) +
    geom_violin(fill = "coral1") + theme_economist_white() + 
    xlab("Outlet Size") + ylab("Sales") + 
    ggtitle("Outlet Size vs. Sales")
  
 p4<- dt %>% group_by(Outlet_Type) %>% 
    ggplot(aes(factor(Outlet_Type),Item_Outlet_Sales)) +
    geom_violin(fill = "coral1") + theme_economist_white() + 
    xlab("Outlet Type") + ylab("Sales") + 
    ggtitle("Outlet Type vs. Sales") +
   theme(axis.text.x = element_text(angle = 90, hjust = 0.01))
  
 p5<- dt %>% group_by(Outlet_Location_Type) %>% 
    ggplot(aes(factor(Outlet_Location_Type),Item_Outlet_Sales)) +
    geom_violin(fill = "coral1") + theme_economist_white() + 
    xlab("Outlet Location Type") + ylab("Sales") + 
    ggtitle("Outlet Location Type vs. Sales")
  
 p6<- dt %>% group_by(Item_Type) %>% 
    ggplot(aes(factor(Item_Type),Item_Outlet_Sales)) +
    geom_violin(fill = "coral1") + theme_economist_white() + 
    xlab("Item Type") + ylab("Sales") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 0.01)) +
    ggtitle("Item Type vs. Sales")
  
  # Continuous variables:
  
   p7 <- dt %>% 
   ggplot(aes(Item_MRP, Item_Outlet_Sales)) +
   geom_point(col = "coral1") + theme_economist_white() + 
   xlab("Item MRP") + ylab("Sales") + 
   ggtitle("Item MRP vs. Sales")
 
   p8 <- dt %>% 
     ggplot(aes(Item_Visibility, Item_Outlet_Sales)) +
     geom_point(col = "coral1") + theme_economist_white() + 
     xlab("Item Visibility") + ylab("Sales") + 
     ggtitle("Item Visibility vs. Sales")
   
   p9 <- dt %>% 
     ggplot(aes(Item_Weight, Item_Outlet_Sales)) +
     geom_point(col = "coral1") + theme_economist_white() + 
     xlab("Item Weight") + ylab("Sales") + 
     ggtitle("Item Weight vs. Sales")
   
   p10 <- dt %>% 
     ggplot(aes(Item_MRP/Item_Weight, Item_Outlet_Sales)) +
     geom_point(col = "coral1") + theme_economist_white() + 
     xlab("Item MRP/Weight Ratio") + ylab("Sales") + 
     ggtitle("Item MRP/Weight Ratio vs. Sales")
  
  grid1 <- plot_grid(p1,p2,p3,p4,p5,p6, ncol = 2)
  grid2 <- plot_grid(p7,p8,p9,p10, ncol = 2)
  Bivariate_EDA <- list(grid1,grid2)
  
}

Eng_dat <- function(dat){
  #The function first encodes the categorical variables. 
  #Then it creates new variables.
  #Finally It returns the new dataset. 
  dat$Outlet_Location_Type <- as.numeric(dat$Outlet_Location_Type)
  
  dat$Outlet_Size_Num[dat$Outlet_Size=="Small"] <- 0
  dat$Outlet_Size_Num[dat$Outlet_Size=="Medium"] <- 1
  dat$Outlet_Size_Num[dat$Outlet_Size=="High"] <- 2
  
  dat$Item_Fat_Content_Num[dat$Item_Fat_Content == "Low Fat"] <- 0
  dat$Item_Fat_Content_Num[dat$Item_Fat_Content =="Regular"] <- 1
  
  dat$Outlet_Type_Num[dat$Outlet_Type=="Grocery Store"] <- 0
  dat$Outlet_Type_Num[dat$Outlet_Type=="Supermarket Type1"] <- 1
  dat$Outlet_Type_Num[dat$Outlet_Type=="Supermarket Type2"] <- 2
  dat$Outlet_Type_Num[dat$Outlet_Type=="Supermarket Type3"] <- 3
  
  perishable = c("Breads", "Breakfast", "Dairy", 
                 "Fruits and Vegetables", "Meat", "Seafood")
  
  non_perishable = c("Baking Goods", "Canned", 
                     "Frozen Foods", "Hard Drinks", 
                     "Health and Hygiene", "Household", "Soft Drinks")
  
  dat$Item_Type_new <- ifelse(dat$Item_Type %in% perishable, 0, 
                               ifelse(dat$Item_Type %in% non_perishable, 1, 2))
  
  dat$Item_category <- substr(dat$Item_Identifier, 1, 2)
  dat$Item_category_Num[dat$Item_category == "NC"] <- 0
  dat$Item_category_Num[dat$Item_category == "DR"] <- 1
  dat$Item_category_Num[dat$Item_category == "FD"] <- 2
  
  #dat$Item_Fat_Content[dat$Item_category_Num == 0] <- as.factor("NonEdible")
  dat$Item_Fat_Content_Num[dat$Item_category_Num == 0] <- 2
  
  dat$Operating_Years <- 2013 - dat$Outlet_Establishment_Year
  
  Eng_dat <- dat
}
