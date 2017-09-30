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

def get_data_from_texfile(article_id, pattern):
    article_dir = sources_directory + "/" + article_id
    files_of_article = os.listdir(article_dir)
    data_list = []
    for texfile_name in filter(lambda s: re.search(r"\.tex$", s), files_of_article):
        f = open(article_dir + "/" + texfile_name)
        tex_code = f.read()
        f.close()
        for match_obj in re.finditer(pattern, tex_code, re.MULTILINE):
            data_list = data_list + match_obj.group(1).split(",")
    return data_list


def count_data(article_id_list, pattern, field_list=None, exclude_pattern_list=None):
    data_list = []
    for article_id in article_id_list:
        if (field_list is None) or (get_subject_area(article_id)[0] in field_list):
            data_list_temp = get_data_from_texfile(article_id, pattern)
            if exclude_pattern_list is not None:
                for exclude_pattern in exclude_pattern_list:
                    data_to_exclude = get_data_from_texfile(article_id, exclude_pattern)
                    for exc in data_to_exclude:
                        data_list_temp = filter(lambda d: d!=exc, data_list_temp)
            # if "document" not in data_list_temp:
            #     print article_id
            data_list_temp = list(set(data_list_temp)) # remove duplicates
            data_list = data_list + data_list_temp
    counter = Counter(data_list)
    return counter.most_common()

def count_areas(article_id_list):
    area_list = []
    for article_id in article_id_list:
        area_list.append(get_subject_area(article_id)[0])
    counter = Counter(area_list)
    return counter.most_common()

def print_count(article_id_list, pattern, field_list=None, exclude_pattern_list=None, name="", suffix=""):
    total_count = 0
    print "\n-----%s-----" % name
    output = ""
    for data, count in count_data(article_id_list, pattern,
                                  field_list=field_list, exclude_pattern_list=exclude_pattern_list):
        # print data, count
        # output += "%s(%d) " % (data, count)
        output += "%s\n" % (data+suffix)
        total_count += count
    print "total_count: %d" % total_count
    print output

def main():
    package_regexp       = r"^[^%\n]*\\usepackage(?:\[[^\]]*\])?\{([a-zA-Z0-9\-,]*)\}"
    documentclass_regexp = r"^[^%\n]*\\documentclass(?:\[[^\]]*\])?\{([a-zA-Z0-9\-,]*)\}"
    environment_regexp   = r"\\end\{([a-zA-Z0-9\-,*]*)\}"
    newtheorem_regexp    = r"\\newtheorem\*? *\{([a-zA-Z0-9*]*)\} *(?:\[[a-zA-Z0-9*]*\])? *\{([^}\n]*)\}"
    newenvironment_regexp = r"\\newenvironment *\{([a-zA-Z0-9*]*)\}"

    max_id = 1000
    article_id_list = map(lambda i: "1701.%05d" % i, list(range(1,max_id+1)))
    field_list = None
    field_list = ["math"]

    print_count(article_id_list, documentclass_regexp, field_list, name="documentclass", suffix=".cls")
    print_count(article_id_list, package_regexp, field_list, name="package", suffix=".sty")
    print_count(article_id_list, environment_regexp, field_list,
                exclude_pattern_list=[newenvironment_regexp, newtheorem_regexp], name="environment")

    print "\n-----area-----"
    for area, count in count_areas(article_id_list):
        print area, count

if __name__ == '__main__':
    main()
