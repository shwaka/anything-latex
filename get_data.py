#!/usr/bin/python

import re
import os
from collections import Counter

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

def get_packages(article_id):
    article_dir = sources_directory + "/" + article_id
    files_of_article = os.listdir(article_dir)
    package_list = []
    for texfile_name in filter(lambda s: re.search(r"\.tex$", s), files_of_article):
        f = open(article_dir + "/" + texfile_name)
        tex_code = f.read()
        f.close()
        #pattern = r"\\usepackage\{([a-zA-Z0-9\-,]*)\}" # old version, without MULTILINE
        pattern = r"^[^%]*\\usepackage(?:\[[^\]]*\])?\{([a-zA-Z0-9\-,]*)\}"
        for match_obj in re.finditer(pattern, tex_code, re.MULTILINE):
            print match_obj.group()
            package_list = package_list + match_obj.group(1).split(",")
    return package_list

def count_packages(article_id_list, field_list=None):
    # field_list=None => all fields
    # field_list=["math", "cs"] => count only in math and cs
    package_list = []
    for article_id in article_id_list:
        if (field_list is None) or (get_subject_area(article_id)[0] in field_list):
            package_list = package_list + get_packages(article_id)
    counter = Counter(package_list)
    return counter.most_common()

def count_areas(article_id_list):
    area_list = []
    for article_id in article_id_list:
        area_list.append(get_subject_area(article_id)[0])
    counter = Counter(area_list)
    return counter.most_common()

if __name__ == '__main__':
    max_id = 10
    # for i in range(1, max_id+1):
    #     article_id = "1701.%05d" % i
    #     print article_id, get_subject_area(article_id), get_packages(article_id)
    # print "-------------"
    article_id_list = map(lambda i: "1701.%05d" % i, list(range(1,max_id+1)))
    #print count_packages(article_id_list, field_list=None)
    for package, count in count_packages(article_id_list, field_list=None):
        print package, count
    print "-------------"
    for area, count in count_areas(article_id_list):
        print area, count
