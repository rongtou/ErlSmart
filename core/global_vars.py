
_global_dict = {}


def put(name, value):
    global _global_dict
    _global_dict[name] = value


def get(name, default=None):
    try:
        return _global_dict[name]
    except KeyError:
        return default
