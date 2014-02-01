from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import *
from itertools import count
from time import sleep
import re

DEBUG = False

def get_month_num(month_str):
    return str(["january"
                , "february"
                , "march"
                , "april"
                , "may"
                , "june"
                , "july"
                , "august"
                , "september"
                , "november"
                , "december"].index(month_str.lower()) + 1)

def get_text(elm):
    return elm.text

def wait_until_located(elm, timeout, by, value):
    wait = WebDriverWait(elm, timeout)
    return wait.until(EC.presence_of_element_located((by, value)))

def wait_until_all_located(elm, timeout, by, value):
    wait = WebDriverWait(elm, timeout)
    return wait.until(EC.presence_of_all_elements_located((by, value)))


def remove_ticket_links(links):
    tick_pat = re.compile("ticketmob")
    return filter(lambda l: not(tick_pat.search(l)), links)

def debug_print_all(xs):
    if DEBUG:
        for x in xs:
            print x
    else: 
        pass

def debug_print(x):
    if DEBUG:
        print x
    else:
        pass

def get_showtimes(cal):
    entries = []
    for day in cal:
        try:
            # Get WebElements
            show_elms = wait_until_all_located(day, 20, By.TAG_NAME, "a")
            time_elms = wait_until_all_located(day, 20, By.TAG_NAME, "small")
            # Extract data
            date = wait_until_located(day, 10, By.CLASS_NAME, "dayHead").text
            times = map(lambda s: s.text, time_elms)
            times = map(lambda t: "11:59PM" if t == "MIDNITE" else t, times)
            titles = map(lambda a: a.text, show_elms)
            titles = filter(lambda t: t != '', titles)
            # debug_print_all(titles)
            links = map(lambda a: a.get_attribute("href"), show_elms)
            links = remove_ticket_links(links)
            entry = (date, zip(titles, times, links))
            entries.append(entry)
        except NoSuchElementException:
            pass
        except TimeoutException:
            pass
    return entries
    
def get_calendar(driver):
    results = wait_until_all_located(driver, 30, By.TAG_NAME, "td")
    # results = driver.find_elements_by_tag_name("td")
    (month, year) = results[1].text.split(' ')
    days = get_showtimes(results[4:])
    return (month, year, days)

def format_date(month, day, year):
    return get_month_num(month) + "/" + day + "/" + year
            
def quote(s):
    return '\"' + s + '\"'

def cal_to_csv(cal):
    month = cal[0]
    year = cal[1]
    theatre = "Cinefamily"
    csvs = []
    for entry in cal[2]:
        day = entry[0]
        for showtime in entry[1]:
            subject = showtime[0]
            time = showtime[1]
            link = showtime[2]
            csv = quote(subject + " @ " + theatre)
            csv += ',' + format_date(month,day,year)
            csv += ',' + time 
            csv += ',' + quote(link)
            csv += ',' + theatre
            csvs.append(csv)
    return csvs

def open_page(driver):
    driver.get("http://www.cinefamily.org")

def click_next_month(driver):
    # debug_print("clicking")
    next_month_button = wait_until_located(driver, 10, By.ID, "EC_nextMonthLarge")
    next_month_button.click()
    # debug_print("done clicking")

def write_csvs_to_file(csvs):
    try:
        f = open("cf.csv", "w")
        f.write("Subject,Start Date,Start Time,Description,Location")
        for csv in csvs:
            f.write(csv + '\n')
    finally:
        f.close()

def main():
    try:
        driver = webdriver.Firefox(timeout = 60 * 5)
        driver.set_page_load_timeout(30)
        open_page(driver)
        click_next_month(driver)
        # debug_print("sleeping")
        # Sleep to prevent race condition when get_calender() is called before
        # DOM is updated
        sleep(10)
        # debug_print("done sleeping")
        cal = get_calendar(driver)
        csvs = cal_to_csv(cal)
        write_csvs_to_file(csvs)
    finally:
        driver.quit()

if __name__ == "__main__":
    main()
    
