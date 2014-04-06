from bs4 import BeautifulSoup
import urllib2
from larevivalist import *

root_url = "http://americancinemathequecalendar.com"
start_url = "http://americancinemathequecalendar.com/calendar/"

def open_page(url):
    src = urllib2.urlopen(start_url)
    return BeautifulSoup(src)

def get_month_year(soup):
    month, year = soup.find(class_="date-heading").text.split(' ')
    return (remove_newline(month), remove_newline(year))
    
def get_calendar_entries(soup):
    return soup.find_all(class_="has-events")

def get_day(entry):
    return filter(lambda x: x!=' ', entry.find(class_="day").text)

def get_events(entry):
    return entry.find_all(class_="monthview")

def get_title(event):
    return remove_newline(event.find(class_="node-title").text)

def get_link(event):
    return root_url+event.find("a").get("href")

def get_theatre(event):
    key = event.find(class_="stripe").get("title")
    if "egyptian" in key.lower():
        return "Egyptian Theatre"
    else:
        return "Aero Theatre"

def get_time(event):
    return event.find(class_="date-display-single").text

def format_time(time_str):
    am_pm_index = time_str.lower().find('p')
    if -1 == am_pm_index:
        am_pm_index = time_str.lower().find('a')
    hr = time_str[0:am_pm_index]
    am_pm = time_str[am_pm_index:].upper()
    return hr+' '+am_pm
    

def get_shows(soup):
    month, year = get_month_year(soup)
    month = get_month_num(month)
    shows = []
    for entry in get_calendar_entries(soup):
        day = get_day(entry)
        for event in get_events(entry):
            title = get_title(event)
            link = get_link(event)
            time = format_time(get_time(event))
            theatre = get_theatre(event)
            date = format_date(month,day,year)
            show = Show(title,date,time,theatre,link)
            shows.append(show)
    return shows
            
def main():
    soup = open_page(start_url)
    shows = get_shows(soup)
    write_to_file(shows,"ac.csv")

if __name__=='__main__':
    main()
