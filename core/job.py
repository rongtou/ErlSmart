import logging
import subprocess
import json
import os
import ErlSmart.core.global_vars as gv
import threading


def run_job(file_path: str):
    modified = int(os.path.getmtime(file_path))
    need_updated = gv.get('cache').is_file_need_update(file_path, modified)
    logging.info("mod %s", need_updated)
    if need_updated:
        # logging.info("mod %s", obj['mod'])
        ret = subprocess.getoutput(r"escript parser/_build/default/bin/parser " + file_path)
        obj = json.loads(ret)
        gv.get('writer').add_req(file_path, modified, obj)
        # logging.debug("json %s", obj['export'])
