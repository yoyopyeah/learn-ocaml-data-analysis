#!/usr/bin/env python2.7

import bs4
import os
import re
import shutil

FILE_RE = re.compile(r'(\d+)-\d+ - (([^ ]-|[^-])+) -.*')

def parse_file_name(n):
    m = FILE_RE.match(n)
    return m.group(1), m.group(2)

def load_file(path):
    with open(path) as fd:
        return bs4.BeautifulSoup(fd, "html.parser")

    
def select_rows(path):
    soup = load_file(path)
    return soup.select('tr[bgcolor="#AAAAAA"]+tr')


def handle_student(tr):
    fname = next(tr.strings)
    idn, name = parse_file_name(fname)
    fname = fname + u'.ml'
    dir_name = u'{}-{}'.format(idn, name)
    os.mkdir(dir_name)
    print u"moving from {} to {}".format(fname, os.path.join(dir_name, "code.ml"))
    shutil.copyfile(fname, os.path.join(dir_name, u"code.ml"))

def main():
    rows = select_rows('index.html')
    for tr in rows:
        handle_student(tr)

if __name__ == '__main__':
    main()
