import sublime
import platform


def get_folders():
    all_folders = []
    for window in sublime.windows():
        all_folders = all_folders + window.folders()
    return list(map(lambda path: adjust_path(path), all_folders))


def adjust_path(path):
    if platform.system() == "Windows":
        return path.replace("/", "\\").capitalize()
    return path
