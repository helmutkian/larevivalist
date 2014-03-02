from bs4 import BeautifulSoup
from larevivalist import *
import urllib2

start_url = "http://www.lacma.org/programs/film/listings"
root_url = "http://www.lacma.org/"

def open_page(url):
    src = urllib2.urlopen(url)
    return BeautifulSoup(src)

def get_calendar_entries(soup):
    is_show = lambda x: x.find(class_="views-field-title")
    return filter(is_show, soup.find_all(class_="views-row"))

def get_title(entry):
    title = entry.find(class_="views-field-title").a.text
    print title
    return title

def get_link(entry):
    link = entry.find(class_="views-field-title").a.get("href")
    return root_url+link

def format_time(time_str):
    if len(time_str) < len("0:00 PM"):
        hour, am_pm = time_str.split(' ')
        return hour+':00 '+am_pm
    else:
        return time_str

def get_date_time(entry):
    date_time = entry.find(class_="views-field-field-event-display-date").text
    date_str, time_str = date_time.split(' | ')
    week_day, month_day, year = date_str.split(', ')
    month_str, day = month_day.split(' ')
    month = get_month_num(month_str)
    time = remove_newline(time_str.upper())
    return (format_date(month,day,year),format_time(time))

def get_shows(soup):
    shows = []
    for entry in get_calendar_entries(soup):
        title = get_title(entry)
        link = get_link(entry)
        date, time = get_date_time(entry)
        show = Show(title,date,time,"LACMA",link)
        shows.append(show)
    return shows

def get_next_page_url(soup):
    link = soup.find(class_="pager-next")
    if link:
        return root_url+link.a.get("href")
    else: 
        return None


def main():
    url = start_url
    shows = []
    while True:
        soup = open_page(url)
        shows += get_shows(soup)
        url = get_next_page_url(soup)
        if not(url):
            break
        print url
    write_to_file(shows, "lacma.csv")

if __name__ == '__main__':
    main()
