import subprocess
import json
import os
import logging
from urllib import request, parse
import ErlSmart.core.global_vars as gv


def add_index_job(file_path: str):
    if file_path.endswith(".erl"):
        pool = gv.pool()
        pool.submit(do_index, file_path)


def del_index_job(file_path: str, is_dir: bool):
    pool = gv.pool()
    pool.submit(do_del, file_path, is_dir)


def do_index(file_path: str):
    modified = int(os.path.getmtime(file_path))
    need_updated = gv.index_reader().is_file_need_update(file_path, modified)
    if need_updated:
        params = parse.urlencode({'path': file_path})
        f = request.urlopen(r"http://127.0.0.1:48437/?%s" % params)
        data = f.read().decode('utf-8')
        obj = json.loads(data)
        # if obj['code'] == "ok" and obj['data'] != "":
        # some file can't analyse, just ignore it
        if obj['code'] != "parse_file_error":
            gv.index_writer().add_req("index", (file_path, obj['data']))
        else:
            logging.warning("can not parser: %s", file_path)
    pass


def do_del(file_path: str, is_dir: bool):
    gv.index_writer().add_req("del", (file_path, is_dir))
