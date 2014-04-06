from larevivalist import *
from bs4 import BeautifulSoup
import urllib2

root_url = "http://72268.formovietickets.com:2235/"

def open_page(url):
    src = urllib2.urlopen(url)
    return BeautifulSoup(src)

def format_lbat_date(date_str):
    month_day, year = date_str[4:].split(", ")
    month_str, day = month_day.split(' ')
    month = get_month_num(month_str)
    return format_date(month, day, year)

def get_calendar_entries(soup):
    entries = []
    for option in soup.find("select").find_all("option"):
        date = format_lbat_date(option.contents[0])
        entry_page = root_url + option.get("value")
        entries.append((date, entry_page))
    return entries

def get_showlistings(entry):
    results = filter(lambda s: s.find('b'), entry.find_all("br")[11:])
    listing_set = {}
    for result in results:
        title = result.find('b').text
        if not(title in listing_set):
            listing_set[title] = result
    return listing_set.values()

def get_showtime(showlisting):
    title = showlisting.contents[0].contents[0]
    time = showlisting.contents[1].contents[0] # fix text encoding
    return (title, time)
