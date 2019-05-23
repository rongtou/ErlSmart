import logging
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from .job import add_index_job, del_index_job
from .utils import get_folders, adjust_path


class Monitor(object):
    def __init__(self):
        self.__observer = Observer()
        self.__wawtches = {}

    def start(self):
        for folder in get_folders():
            self.add_path(folder)
        if not self.__observer.isAlive():
            self.__observer.start()

    def add_path(self, path):
        event_handler = ErlFileEventHandler()
        watch = self.__observer.schedule(event_handler, path, recursive=True)
        self.__wawtches[path] = watch

    def remove_path(self, path):
        watch = self.__wawtches.pop(path)
        self.__observer.unschedule(watch)

    def shutdown(self):
        if self.__observer.isAlive():
            self.__observer.stop()
            self.__observer.join()


class ErlFileEventHandler(FileSystemEventHandler):

    def on_moved(self, event):
        # what = 'directory' if event.is_directory else 'file'
        # logging.debug("Moved %s: from %s to %s", what, event.src_path,
        #              event.dest_path)
        del_index_job(adjust_path(event.src_path), event.is_directory)

    def on_created(self, event):
        # what = 'directory' if event.is_directory else 'file'
        # logging.debug("Created %s: %s", what, event.src_path)
        if not event.is_directory:
            add_index_job(adjust_path(event.src_path))

    def on_deleted(self, event):
        # what = 'directory' if event.is_directory else 'file'
        # logging.debug("Deleted %s: %s", what, event.src_path)
        del_index_job(adjust_path(event.src_path), event.is_directory)

    def on_modified(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.debug("Modified %s: %s", what, event.src_path)
        if not event.is_directory:
            add_index_job(adjust_path(event.src_path))

