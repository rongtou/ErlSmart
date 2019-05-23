import sqlite3
import ErlSmart.core.global_vars as gv
import queue
import hashlib
import logging
import os
import platform
import traceback
from threading import Thread

cache_file = "cache/cache.db"

CREATE_FOLDER_SQL = '''
create table if not exists file_path (
    fid varchar(32) not null,
    path varchar(256) not null,
    updated_at int unsigned not null,
    primary key(fid)
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
        except sqlite3.Error:
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
            op, param = self.__reqs.get()
            if op == "index":
                self.add_index(param)
            else:
                self.del_index(param)

    def add_index(self, param):
        path, parse_obj = param

        modified = int(os.path.getmtime(path))
        try:
            self.__cur.execute("select updated_at from file_path where path=?", (path,))
            ret = self.__cur.fetchone()
            if ret is None:
                need_update = True
            else:
                need_update = ret[0] != modified

            if not need_update:
                return

            encrypt = hashlib.md5()
            encrypt.update(path.encode("utf-8"))
            fid = encrypt.hexdigest()
            if ret is None:
                logging.debug("index insert %s", path)
                self.__cur.execute("insert into file_path(fid, path, updated_at) values(?,?,?)", (fid, path, modified))
            else:
                logging.debug("index update %s", path)
                self.__cur.execute("update file_path set updated_at = ? where fid = ?", (modified, fid))
            self.__cur.execute("delete from erl_file where fid=?", (fid,))
            mod = parse_obj['mod']
            for funobj in parse_obj['func']:
                logging.debug("index %s %s", mod, funobj['name'])
                self.__cur.execute(
                    "insert into erl_file(fid, mod, fun, arity, line, args, exported) values(?,?,?,?,?,?,?)",
                    [fid, mod, funobj['name'], funobj['arity'], funobj['line'],
                     ", ".join(funobj['args']), funobj['exported']])
            self.__con.commit()
        except sqlite3.Error:
            self.__con.rollback()
            traceback.print_exc()

    def del_index(self, path: str):
        logging.debug("del index ", path)
        root, ext = os.path.splitext(path)
        try:
            if ext == ".erl":
                encrypt = hashlib.md5()
                encrypt.update(path.encode("utf-8"))
                fid = encrypt.hexdigest()
                self.__cur.execute("delete from file_path where fid=?", (fid,))
                self.__cur.execute("delete from erl_file where fid=?", (fid,))
                self.__con.commit()
            elif ext == "":
                if platform.system() == "Windows":
                    path = path + "\\"
                else:
                    path = path + "/"
                self.__cur.execute("select fid from file_path where path like '" + path + "%'")
                rets = self.__cur.fetchall()
                if len(rets) > 0:
                    for ret in rets:
                        self.__cur.execute("delete from file_path where fid=?", (ret[0],))
                        self.__cur.execute("delete from erl_file where fid = ?", (ret[0],))
                        self.__con.commit()
        except sqlite3.Error:
            self.__con.rollback()
            traceback.print_exc()

    def add_req(self, op, param):
        self.__reqs.put((op, param))
