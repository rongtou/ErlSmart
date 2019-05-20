import sqlite3
import ErlSmart.core.global_vars as gv
import queue
import hashlib
import os
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
    primary key(fid, mod, fun, arity)
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
        self._pool_size = 10
        self._pool = queue.Queue(self._pool_size)
        self._create_con()

    def _create_con(self):
        for i in range(self._pool_size):
            con = sqlite3.connect(cache_file, check_same_thread=False)
            con.execute('pragma journal_mode=wal')
            self._pool.put(con)

    def get_con(self) -> sqlite3.Connection:
        return self._pool.get()

    def release_con(self, con):
        self._pool.put(con)

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
            print("error ", err)
        finally:
            self.release_con(con)
        if ret is None:
            return True
        else:
            return ret[0] != modified


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
            encrypt = hashlib.md5()
            encrypt.update(path.encode("utf-8"))
            fid = encrypt.hexdigest()
            self.__cur.execute("insert into file_path(id, path, updated_at) values(?,?,?)", (fid, path, modified))
            self.__cur.execute("delete from erl_file where fid=?", (fid,))
            mod = os.path.splitext(os.path.basename(path))[0]
            for funobj in ret['func']:
                self.__cur.execute("insert into erl_file(fid, mod, fun, arity, line, args) values(?,?,?,?,?,?)",
                                   [fid, mod, funobj['name'], funobj['arity'], funobj['line'],
                                    ", ".join(funobj['args'])])
            self.__con.commit()

    def add_req(self, path, modified, ret):
        self.__reqs.put((path, modified, ret))
