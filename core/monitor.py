import time, logging
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler


class Monitor():
    def __init__(self):
        self.__observer = Observer()

    def run(self):
        logging.basicConfig(level=logging.INFO,
                            format='%(asctime)s - %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S')
        path = "e:\\Work\\xw01\\server"
        event_handler = ErlFileEventHandler()
        self.__observer.schedule(event_handler, path, recursive=True)
        self.__observer.start()

    def shutdown(self):
        self.__observer.stop()

class ErlFileEventHandler(FileSystemEventHandler):
    """Logs all the events captured."""

    def on_moved(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.info("Moved %s: from %s to %s", what, event.src_path,
                     event.dest_path)

    def on_created(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.info("Created %s: %s", what, event.src_path)

    def on_deleted(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.info("Deleted %s: %s", what, event.src_path)

    def on_modified(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.info("Modified %s: %s", what, event.src_path)
