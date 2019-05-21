import logging
import subprocess
import json
import os
import ErlSmart.core.global_vars as gv


def run_job(file_path: str):
    modified = int(os.path.getmtime(file_path))
    need_updated = gv.get('cache').is_file_need_update(file_path, modified)
    if need_updated:
        ret = subprocess.getoutput(r"escript parser/_build/default/bin/parser " + file_path)
        obj = json.loads(ret)
        gv.get('writer').add_req(file_path, modified, obj)
