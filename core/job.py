import logging
import subprocess
import json


def run_job(filename: str):
    ret = subprocess.getoutput(r"escript parser/_build/default/bin/parser " + filename)
    # logging.warning("parse ret %s", ret)
    try:
        obj = json.loads(ret.strip('<>'))
        logging.warning("json %s", obj['export'])
    except Exception as err:
        print(err)
