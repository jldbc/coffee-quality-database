from selenium import webdriver
from selenium.webdriver.common.by import By
import pandas as pd
import time

login_email = 'your_email'
login_password = 'your_password'

# open chromedriver
driver = webdriver.Chrome('/Users/jledoux/Downloads/chromedriver')
time.sleep(2)

# navigate to login page
driver.get('https://database.coffeeinstitute.org/login')
time.sleep(2)

# submit login credentials 
form = driver.find_element_by_xpath('//html/body/content[@class="scrollable"]/div[@class="container page"]/div[@class="form short"]/div[@class="login panel"]/form')
username = driver.find_element_by_name("username")
password = driver.find_element_by_name("password")
time.sleep(2)

username.send_keys(login_email)
password.send_keys(login_password)
driver.find_element_by_class_name("submit").click()
time.sleep(2)


# navigate to coffees page, then to arabicas page containing links to all quality reports 
coffees = driver.find_element_by_xpath('//html/body/header/nav[@id="main"]/div[@class="container"]/div[@class="in"]/a[@href="/coffees"]').click()
time.sleep(2)
driver.find_element_by_link_text('Arabica Coffees').click()

# process a coffee report - pull all data and export to csv 

#table = driver.find_element_by_xpath('//table')
for i in driver.find_elements_by_xpath("//tr"):
	print(i.get_attribute('row'))

test_page = driver.find_elements_by_xpath("//tr")[4].click() # click on 4th coffee as an example 

tables = driver.find_elements(By.TAG_NAME, "table")

# loop over all coffee reports on the page, processing each one and writing to csv

i = 1 # make this increment once im iterating through all coffees
j = 0
for i in tables:
	#t = BeautifulSoup(tables[i].get_attribute('innerHTML'), "html.parser")
	t = BeautifulSoup(i.get_attribute('outerHTML'), "html.parser")
	print(t)
	df = pd.read_html(str(t))
	name = 'coffee_{}_table_{}.csv'.format(i,j)
	df[0].to_csv(name)
	j += 1


# skip to next page when done 




# close the driver
driver.close()












