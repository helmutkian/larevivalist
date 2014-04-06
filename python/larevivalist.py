class Show:
    def __init__(self, title, date, time, theatre, link):
        self.title = title
        self.date = date
        self.time = time
        self.theatre = theatre
        self.link = link

    def to_csv(self):
        quote = lambda x: '\"'+x+'\"'
        csv = quote(self.title + ' @ ' + self.theatre)
        csv += ',' + self.date
        csv += ',' + self.time
        csv += ',' + quote(self.theatre)
        csv += ',' + quote(self.link)
        return csv.encode('utf-8')

def remove_newline(s):
    return filter(lambda x: x!='\n', s)

def get_month_num(month_str):
    months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]
    return months.index(month_str[0:3].lower())+1

def format_date(month,day,year):
    return str(month)+'/'+str(day)+'/'+str(year)

def write_to_file(shows, file_name):
    f = open(file_name, 'w')
    with f:
        f.write("Subject,Start Date,Start Time,Location,Description\n")
        for show in shows:
            f.write(show.to_csv()+'\n')


