import os
from concurrent.futures import ThreadPoolExecutor
from .job import run_job


def scan(pool: ThreadPoolExecutor, path: str):
    for file_path, dirs, files in os.walk(path):
        for f in files:
            filename = os.path.join(file_path, f)
            if filename.endswith(".erl"):
                pool.submit(run_job, filename)
