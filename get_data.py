#!/usr/bin/python

import re
import os

sources_directory = os.path.expanduser("~/Dropbox/programs/tex/arxiv_sources")

def get_subject_area(article_id):
    f = open(sources_directory + "/" + article_id + "/" + article_id)
    html_text = f.read()
    f.close()
    pattern = r"<span class=\"primary-subject\">[^\(]*\(([a-zA-Z\-]*)(?:\.([a-zA-Z\-]*))?\)</span>"
    match_obj = re.search(pattern, html_text)
    field = match_obj.group(1)
    subject = match_obj.group(2)
    return field, subject

if __name__ == '__main__':
    for i in range(1, 501):
        article_id = "1701.%05d" % i
        print article_id, get_subject_area(article_id)
