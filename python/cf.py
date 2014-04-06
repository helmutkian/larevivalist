from selenium import webdriver
from bs4 import BeautifulSoup
from larevivalist import *

def format_time(time):
    if time == "MIDNITE":
        return "11:59 PM"
    else:
        return time

def open_page():
    driver = webdriver.Firefox()
    driver.implicitly_wait(10)
    driver.get("http://www.cinefamily.org")
    return BeautifulSoup(driver.page_source)

def get_calendar(soup):
    return soup.find(id="calendar-top")

def get_month_year(calendar):
    return calendar.find(id="EC_current-month").text.split(' ')

def get_calendar_entries(calendar):
    cells = calendar.find_all("td")
    return filter(lambda c: c.find(class_="dayHead"), cells)

def make_show(event, date):
    time = format_time(event.find("small").text)
    title_link = event.find(class_="EC-tt-title-link")
    title = title_link.text
    link = title_link.get("href")
    return Show(title, date, time, "Cinefamily", link)

def get_showtimes(calendar):
    month_str, year = get_month_year(calendar)
    month = get_month_num(month_str)
    entries = get_calendar_entries(calendar)
    shows = []
    for entry in entries:
        day = entry.find(class_="dayHead").text
        date = format_date(month, day, year)
        events = entry.find_all(class_="event-block")
        for event in events:
            if event.find("small"):
                shows.append(make_show(event, date))
    return shows
 
def main():
    page = open_page()
    calendar = get_calendar(page)
    shows = get_showtimes(calendar)
    write_to_file(shows, 'cf.csv')


if __name__ == '__main__':
    main()
