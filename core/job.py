import subprocess
import json
import os
import logging
import ErlSmart.core.global_vars as gv


def add_index_job(file_path: str):
    if file_path.endswith(".erl"):
        pool = gv.get('pool')
        pool.submit(do_index, file_path)


def del_index_job(file_path: str):
    pool = gv.get('pool')
    pool.submit(do_del, file_path)


def do_index(file_path: str):
    modified = int(os.path.getmtime(file_path))
    need_updated = gv.get('cache').is_file_need_update(file_path, modified)
    if need_updated:
        ret = subprocess.getoutput(r"escript parser/_build/default/bin/parser " + file_path)
        if ret != '':
            obj = json.loads(ret)
            gv.get('writer').add_req("index", (file_path, obj))
        else:
            logging.debug("can not parser: %s", file_path)


def do_del(file_path: str):
    gv.get('writer').add_req("del", file_path)
