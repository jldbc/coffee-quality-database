from selenium import webdriver
import time

login_email = 'yours_here'
login_password = 'yours_here'

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












