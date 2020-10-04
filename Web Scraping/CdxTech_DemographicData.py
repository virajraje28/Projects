#Objective of this is to pull demographic data of the trade area of the Lumber Liquidators store. 
#The List of stores along with the Zip code was provied to us by LL



import requests
import bs4 as bs
import time
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import re
import pandas as pd
from selenium.webdriver.common.action_chains import ActionChains
import random


#Added all the zip codes of LL's US stores.
zipCodes = [
#This contained the list of Zip codes        
]


driver = webdriver.Chrome('/Users/virajraje/Downloads/chromedriver')  # Optional argument, if not specified will search path.
url = 'https://www.cdxtech.com/tools/bulk/demographics/radius/'
miles = 20
x = 0

#Below code will open chrome in incognito mode. Go the the CDX website. Type in the zip code and miles and click on "Report" button.
#On the next page, the code will scrape the data from the table and store it in the Data frame. 
#It will then close the chrome browser and continue these steps for all the zip codes.
 
for zipCode in zipCodes:
    print(x)
    print(url)
    options = webdriver.ChromeOptions()
    options.add_argument("--incognito")
    driver = webdriver.Chrome('/Users/virajraje/Downloads/chromedriver', options=options) 
    driver.implicitly_wait(30)
    time.sleep(random.randint(2,10))
    driver.get(url)
    time.sleep(random.randint(5,15))
    text_ZipCode = driver.find_element_by_id('Zipcode')
    text_ZipCode.send_keys(zipCode)
    text_Miles = driver.find_element_by_id('Mileage')
    text_Miles.send_keys(miles)
    btnGenPreview = driver.find_element_by_id('EntryReport')
    btnGenPreview.click()
    time.sleep(random.randint(10,20))
    #print(driver.page_source)
    soup = bs.BeautifulSoup(driver.page_source,'lxml')
    table = soup.find(id  = 'PreviewTable')
    table_body = table.find('tbody')
    table_rows = table.find_all('tr')
    for tr in table_rows:
        td = tr.find_all('td')
        row = [i.text for i in td]
        if len(row) == 68 :
            series = pd.Series(row, index = df.columns)
            df = df.append(series, ignore_index=True)
    
    driver.quit()
    x = x + 1

    
df1 = df

df.to_csv(r'/Users/virajraje/Documents/Viraj/Masters/VCU/StudyMaterial/GitRepo/LumberVCU/Data/DerivedFiles/DemographicInfo.csv', index = False)


    
    


