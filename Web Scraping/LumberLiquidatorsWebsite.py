#Objective is to scrape the comments of all the products from Lumber Liquidator's website. 
#This was used to derive sentiments from the comments and match them against the corresponsing sales data shared by LL.

import requests
from bs4 import BeautifulSoup
import time
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import re
import pandas as pd
from selenium.webdriver.common.action_chains import ActionChains

#Step 1:
#GEt URLs for all product Categories
#MAnually pulled URLs for relevant product categories since they were only a few and would have been time consuming to automatically fetch them.


urls = ["https://www.lumberliquidators.com/ll/s/vinyl-plank"
,"https://www.lumberliquidators.com/ll/s/vinyl-plank?pg=2"
,"https://www.lumberliquidators.com/ll/s/laminate-flooring"
,"https://www.lumberliquidators.com/ll/s/waterproof-flooring"
,"https://www.lumberliquidators.com/ll/s/waterproof-flooring?pg=2"
,"https://www.lumberliquidators.com/ll/s/hardwood-flooring"
,"https://www.lumberliquidators.com/ll/s/hardwood-flooring?pg=2"
,"https://www.lumberliquidators.com/ll/s/hardwood-flooring?pg=3"
,"https://www.lumberliquidators.com/ll/s/hardwood-flooring?pg=4"
,"https://www.lumberliquidators.com/ll/s/bamboo-flooring"
,"https://www.lumberliquidators.com/ll/s/tile-flooring"
,"https://www.lumberliquidators.com/ll/s/distressed?keyword=distressed"
,"https://www.lumberliquidators.com/ll/s/distressed?pg=2"]


# Step2: For all the categories mentioned above, GEt URL for all products within them.

#Downloaded the chrome driver and added a pointer to it
driver = webdriver.Chrome('/Users/virajraje/Downloads/chromedriver')  # Optional argument, if not specified will search path.
cols = ['ItemID', 'URL']
itemList = []


for url in urls:
    print(url)
    options = webdriver.ChromeOptions()
    options.add_argument("--incognito")
    driver = webdriver.Chrome('/Users/virajraje/Downloads/chromedriver', options=options) 
    driver.implicitly_wait(60)
    driver.get(url)
    driver.implicitly_wait(60)
    driver.switch_to_default_content()
    actions = ActionChains(driver)
    actions.send_keys(Keys.ESCAPE)
    actions.perform()
    driver.implicitly_wait(60)
    driver.switch_to_default_content()
    driver.page_source
    soupLevel1 = BeautifulSoup(driver.page_source, 'html.parser')
    soupLevel2 = soupLevel1.find('div', {'class': 'container-fluid prodListing'})
    
    #Loop through the anchor tag and populate the URL for all products into itemList
    for product_div in soupLevel2.find_all('a', {'class': 'clickable-product-area'}):
        print(product_div.get('data-id'))
        print("https://www.lumberliquidators.com" + product_div.get('data-link'))
        itemList.append([product_div.get('data-id'), "https://www.lumberliquidators.com" + product_div.get('data-link')])
    driver.quit()
 
 #Transferred the data for all product URL from itemList variable to item_df       
item_df = pd.DataFrame(itemList, columns=cols)   
#item_df is stored in excel


#Step3: Loop thorugh all the products URL, open them in chrome browser and scrape the comments
#Read excel containing all product URLs
import pandas as pd
item_df = pd.read_excel("/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/Sem3/Text Mining/Project/Article_URL.xlsx")


#Created a method for scraping comments 
def get_Comments_Function(pgSource, itemId, itemURL ):
    soupCommentLevel1 = BeautifulSoup(pgSource, 'html.parser')
    print(itemId)
    print(itemURL)
    for article in soupCommentLevel1.find_all('article'):
        description = article.find('p', {'class': 'pr-rd-description-text'}).getText()
        heading = article.find('h2', {'class': 'pr-rd-review-headline'}).getText()
        stars =  article.find('div', {'class': 'pr-snippet-rating-decimal'}).getText()
        print(description)
        print(heading)
        print(stars)
        commentList.append([itemId,itemURL,stars,heading,description])
        description = ''
        heading = ''
        stars = ''


#Below code will loop through all the products in the excel, open them in chrome browser one at a time and call the above method to scrape comments
# The site shows only 10 comments at a time. So below code also clicks on "Next" button to loop through comments on next page.
x = 0
src = []
Finalcols = ['ItemID', 'URL', 'stars', 'heading', 'description']
commentList = []
for item in item_df.iterrows():
    print('*********************************')
    print(x)
    print('*********************************')
    options = webdriver.ChromeOptions()
    options.add_argument("--incognito")
    driver = webdriver.Chrome('/Users/virajraje/Downloads/chromedriver', options=options) 
    driver.implicitly_wait(20)
    driver.get(item[1][2])
    time.sleep(2)
    actions = ActionChains(driver)
    actions.send_keys(Keys.ESCAPE)
    actions.perform()
    time.sleep(2)
    driver.switch_to_default_content()
    if len(driver.find_elements_by_xpath('//*[@id="fold5"]/div/div[1]/a')) > 0 :
        button = driver.find_element_by_xpath('//*[@id="fold5"]/div/div[1]/a')
        time.sleep(2)
        button.click()    
        time.sleep(2)
        driver.switch_to_default_content()
        src.append(driver.page_source)
        get_Comments_Function(driver.page_source, item[1][1], item[1][2])
        button = None
        
        if len(driver.find_elements_by_xpath('//*[@id="pr-review-display"]/footer/div/aside/button')) > 0 :
            button = driver.find_element_by_xpath('//*[@id="pr-review-display"]/footer/div/aside/button')
            button.click()  
            driver.switch_to_default_content()
            time.sleep(2)
            get_Comments_Function(driver.page_source, item[1][1], item[1][2])
        else:
            continue
        
        button = None
        multipleNExt = 0
        
        
        if len(driver.find_elements_by_xpath('//*[@id="pr-review-display"]/footer/div/aside/button[2]')) > 0 :
            multipleNExt = 1  #HAndle products with comments on multiple pages
        else :
            multipleNExt = 0
            
        while multipleNExt == 1:
            button = driver.find_element_by_xpath('//*[@id="pr-review-display"]/footer/div/aside/button[2]')
            button.click()  
            time.sleep(2)
            driver.switch_to_default_content()
            get_Comments_Function(driver.page_source, item[1][1], item[1][2])
            button = None
            if len(driver.find_elements_by_xpath('//*[@id="pr-review-display"]/footer/div/aside/button[2]')) > 0 :
                multipleNExt = 1
            else :
                multipleNExt = 0
    
    driver.quit()


#commentList ariable has all the comments for all products. Transferred them to excel for further analysis.