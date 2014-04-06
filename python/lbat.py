from bs4 import BeautifulSoup
import urllib2

root_url = "http://72268.formovietickets.com:2235/"

def open_page(url):
    src = urllib2.urlopen(url)
    return BeautifulSoup(src)

def format_date(date_str):
    month_str, day, year = date_str.split(' ')[1:]
    month = get_month_num(month_str, map(lambda m: m[0:3], MONTHS))
    return str(month)+'/'+day+'/'+year

def get_calendar_entries(soup):
    entries = []
    for option in soup.find_all("option"):
        date = format_date(option.contents[0])
        page = root_url+'/'+option.get("value")
        entries.append((date, option))


