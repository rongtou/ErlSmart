import logging
import subprocess
import os


def run_job(filename: str):
    # script = "parser/_build/default/bin/parser"
    # logging.info("scan file %s", "escript " + script + filename)
    # p = subprocess.Popen(["escript", script, filename], stdout=subprocess.PIPE, shell=True)
    # p = subprocess.Popen(r"escript e:\Work\code\ErlSmart\parser\_build\default\bin\parser", stdout=subprocess.PIPE, shell=True)
    ret = subprocess.getoutput(r"escript parser\_build\default\bin\parser " + filename)
    # p = subprocess.Popen(r"escript parser/parser", stdout=subprocess.PIPE, shell=True)
    logging.warning("parse ret %s", "succ")

