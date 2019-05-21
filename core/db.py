import sqlite3
import ErlSmart.core.global_vars as gv
import queue
import hashlib
import logging
import os
import traceback
from threading import Thread

cache_file = "cache/cache.db"

CREATE_FOLDER_SQL = '''
create table if not exists file_path (
    id varchar(32) not null,
    path varchar(256) not null,
    updated_at int unsigned not null,
    primary key(id)
);
'''

CREATE_ERL_FILE_SQL = '''
create table if not exists erl_file (
    fid varchar(256) not null,
    mod varchar(128) not null,
    fun varchar(128) not null,
    arity tinyint not null,
    line int unsigned not null,
    args varchar(256) not null,
    exported bool not null,
    primary key(fid, mod, fun, arity, line)
);
'''


def init_db():
    cache = Cache()
    gv.put('cache', cache)
    cache.create_table()
    writer = CacheWriter()
    gv.put('writer', writer)


class Cache(object):

    def __init__(self):
        self.__pool_size = 10
        self.__pool = queue.Queue(self.__pool_size)
        self._create_con()

    def _create_con(self):
        for i in range(self.__pool_size):
            con = sqlite3.connect(cache_file, check_same_thread=False)
            con.execute('pragma journal_mode=wal')
            self.__pool.put(con)

    def get_con(self) -> sqlite3.Connection:
        return self.__pool.get()

    def release_con(self, con):
        self.__pool.put(con)

    def create_table(self):
        con = self.get_con()
        con.execute(CREATE_FOLDER_SQL)
        con.execute(CREATE_ERL_FILE_SQL)
        self.release_con(con)

    def is_file_need_update(self, path: str, modified: int) -> bool:
        ret = None
        con = self.get_con()
        cur = con.cursor()
        try:
            cur.execute("select updated_at from file_path where path=?", (path,))
            ret = cur.fetchone()
        except Exception as err:
            logging.error("err: %s", err)
            traceback.print_exc()
        finally:
            self.release_con(con)
        if ret is None:
            return True
        else:
            return ret[0] != modified

    def close(self):
        for i in range(self.__pool_size):
            con = self.__pool.get()
            con.close()


class CacheWriter(Thread):

    def __init__(self):
        super(CacheWriter, self).__init__()
        self.__con = sqlite3.connect(cache_file, check_same_thread=False)
        self.__con.execute('pragma journal_mode=wal')
        self.__cur = self.__con.cursor()
        self.__reqs = queue.Queue()
        self.start()

    def run(self):
        while True:
            path, modified, ret = self.__reqs.get()

            if not self.is_file_need_update(path):
                continue

            encrypt = hashlib.md5()
            encrypt.update(path.encode("utf-8"))
            fid = encrypt.hexdigest()
            try:
                self.__cur.execute("insert into file_path(id, path, updated_at) values(?,?,?)", (fid, path, modified))
                self.__cur.execute("delete from erl_file where fid=?", (fid,))
                mod = ret['mod']
                for funobj in ret['func']:
                    self.__cur.execute(
                        "insert into erl_file(fid, mod, fun, arity, line, args, exported) values(?,?,?,?,?,?,?)",
                        [fid, mod, funobj['name'], funobj['arity'], funobj['line'],
                         ", ".join(funobj['args']), funobj['exported']])
                self.__con.commit()
            except Exception as err:
                logging.error("err: %s", err)
                traceback.print_exc()

    def add_req(self, path, modified, ret):
        self.__reqs.put((path, modified, ret))

    def is_file_need_update(self, path: str) -> bool:
        modified = int(os.path.getmtime(path))
        ret = None
        try:
            self.__cur.execute("select updated_at from file_path where path=?", (path,))
            ret = self.__cur.fetchone()
        except Exception as err:
            logging.error("err: %s", err)
            traceback.print_exc()
        if ret is None:
            return True
        else:
            return ret[0] != modified
