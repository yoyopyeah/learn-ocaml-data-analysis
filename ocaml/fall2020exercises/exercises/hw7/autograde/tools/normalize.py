#!/usr/bin/env python3.8

import csv
import sys

class Student(object):
    def __init__(self, m, name, s, b):
        (_, name) = name.split('-', 1)
        self._name = name
        self._idn = m[name]
        (num, denom) = s.split('/')
        self._num = float(num)
        self._denom = int(denom)
        self._bad_style = bool(b)

    def overall_points(self):
        return max(0.0, self._num - (5 if self._bad_style == 'true' else 0))

    def normalize(self):
        return self.overall_points() / self._denom * 100

    def row(self):
        return [self._idn, self.normalize(), "#"]
    
def load_file(m, f):
    with open(f) as fd:
        rdr = csv.reader(fd)
        return [Student(m, *r) for r in rdr]
            
        
def write_file(sts, f):
    with open(f, 'w') as rd:
        wrt = csv.writer(rd)
        header = "OrgDefinedId,Coding Points Grade,End-of-Line Indicator"
        headers = header.split(',')
        wrt.writerow(headers)
        for s in sts:
            wrt.writerow(s.row())

def load_student_ids(f):
    with open(f) as fd:
        rdr = csv.reader(fd)
        next(rdr)
        m = {}
        for (idn, lname, fname, _) in rdr:
            name = fname + " " + lname
            if name in m:
                print("clashed name {}".format(name))
            else:
                m[name] = idn[1:]
        return m
            
def main():
    (idcsv, inp, outp) = sys.argv[1:]
    ids = load_student_ids(idcsv)
    sts = load_file(ids, inp)
    write_file(sts, outp)

if __name__ == '__main__':
    main()
