
_global_dict = {}


def put(name, value):
    global _global_dict
    _global_dict[name] = value


def get(name, default=None):
    try:
        return _global_dict[name]
    except KeyError:
        return default


def index_reader():
    return get('index_reader')


def set_index_reader(reader):
    put('index_reader', reader)


def index_writer():
    return get('index_writer')


def set_index_writer(writer):
    put('index_writer', writer)


def monitor():
    return get('monitor')


def set_monitor(monitor):
    put('monitor', monitor)


def pool():
    return get('pool')


def set_pool(pool):
    put('pool', pool)

