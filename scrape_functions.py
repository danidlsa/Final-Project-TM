from selenium.common.exceptions import NoSuchElementException, ElementClickInterceptedException, StaleElementReferenceException
import re
from selenium.webdriver.support.ui import Select
import random
import time
from selenium import webdriver
from selenium.webdriver.firefox.service import Service
import os
import numpy as np
import countryinfo
from countryinfo import CountryInfo
import csv
import itertools
import os

   
"""
Code with scrape function.

The function start_up is the only one we will use to download from the UN. It initiates the browser using the stipulated functions and options.

The download option allows students to download immediately on opening the target page. This is very useful for the UN application we will discuss.

"""

#geko_path = '/Users/luisignaciomenendezgarcia/MEGA/Portatil_mega/conflictforecast/regional_trials/geckodriver'
#download_path = '/Users/luisignaciomenendezgarcia/Dropbox/CLASSES/class_scraping_bse/resolutions'
#geko_path = "/Users/joulevoelz/Documents/BSE/DSM/text-mining/web-scraping-module/web-scraping/geckodriver.exe"
#download_path = "/Users/joulevoelz/Documents/BSE/DSM/text-mining/web-scraping-module/web-scraping/booking_downloads"
geko_path = "geckodriver.exe"
download_path = "Users/danid/Dropbox/BSE - DSDM/Intro text mining/problem set 3/booking_downloads"


def start_up(dfolder, link, download):
    # check whether I moved to next step; otherwise repeat it:
    # Set aMozilla firefox to automatically download
    profile = webdriver.FirefoxProfile()
    os.makedirs(download_path, exist_ok=True)
    # open browser and set up its preferences:
    option = webdriver.FirefoxOptions()
    #option.binary_location = "Users/joulevoelz/Applications/Firefox.app/Contents/MacOS/firefox.exe"
    driverService = Service("/Users/joulevoelz/Documents/BSE/DSM/text-mining/web-scraping-module/web-scraping/geckodriver.exe")


    if download==True:
        option.download=True
        profile.set_preference("browser.download.dir", dfolder)
        profile.set_preference("browser.download.folderList", 2)
        profile.set_preference("browser.download.manager.showWhenStarting", False)
        profile.set_preference("browser.helperApps.neverAsk.saveToDisk",
                               "application/msword,application/rtf, application/csv,text/csv,image/png ,image/jpeg, application/pdf, text/html,text/plain,application/octet-stream")
        mime_types = "application/pdf,application/vnd.adobe.xfdf,application/vnd.fdf,application/vnd.adobe.xdp+xml"
        profile.set_preference("browser.helperApps.neverAsk.saveToDisk", "application/pdf,application/x-pdf")
        profile.set_preference("plugin.disable_full_page_plugin_for_types", mime_types)
        profile.set_preference("pdfjs.disabled", True)
    browser = webdriver.Firefox(service=driverService, firefox_profile=profile, options=option)

    ########### --------------------
    # 2.- Login

    # Enter the website address here
    browser.get(link)
    time.sleep(seconds())
    return profile, browser, download_path


def check_exists(browser, xpath, type):
    '''
    Function that checks whether the object exists so to proceed to subsequent
    steps.
    '''
    try:
        if type == "xpath":
            browser.find_element('xpath', xpath)
        elif type == "id":
            browser.find_element('id',xpath)
        elif type == "css":
            browser.find_element('css selector',xpath)
        elif type == "class":
            browser.find_element('class name',xpath)
    # except NoSuchElementException:
    except (ElementClickInterceptedException, NoSuchElementException, StaleElementReferenceException):
        return False
    return True


def check_obscures(browser, xpath, type):
    '''
    Function that checks whether the object is being "obscured" by any element so
    that it is not clickable. Important: if True, the object is going to be clicked!
    '''
    try:
        if type == "xpath":
            browser.find_element('xpath',xpath).click()
        elif type == "id":
            browser.find_element('id',xpath).click()
        elif type == "css":
            browser.find_element('css selector',xpath).click()
        elif type == "class":
            browser.find_element('class name',xpath).click()
        elif type == "link":
            browser.find_element('link text',xpath).click()
    except (ElementClickInterceptedException, NoSuchElementException, StaleElementReferenceException):
        return False
    return True


def check_and_click(browser, xpath, type):
    '''
    Function that checks whether the object is clickable and, if so, clicks on
    it. If not, waits one second and tries again.
    '''
    ck = False
    ss = 0
    while ck == False:
        ck = check_obscures(browser, xpath, type)
        time.sleep(1)
        ss += 1
        if ss == 15:
            # warn_sound()
            # return NoSuchElementException
            ck = True
            # browser.quit()


def seconds():
    '''
    Generates random waiting time
    '''
    s = 7 + (random.random() * 2)
    return s


def input_Dates(browser, dates):
    '''
    inputs key arguments
    '''

    # Choosing 'custom dates' on the drop down menu
    # setting start date:
    search1 = browser.find_element_by_id("dateFrom")
    search1.send_keys(str(dates[0]))
    time.sleep(1)
    # setting end date:
    search2 = browser.find_element_by_id("dateTo")
    search2.send_keys(str(dates[1]))
    time.sleep(1)
