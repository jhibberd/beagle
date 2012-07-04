import tornado.ioloop
import tornado.web

# Experimental

import sys
from subprocess import PIPE, Popen
from threading  import Thread
from Queue import Queue, Empty

STOP=False

def proxy(queue):
    p = Popen(['../Player'], shell=False, stdin=PIPE, stdout=PIPE)
    while True:
        try:
            scenario, handler = queue.get(timeout=3)
        except Empty:
            if STOP: return
            else: continue
        p.stdin.write(scenario+'\n')
        x = p.stdout.readline()
        handler.write(x)
        handler.finish()

q = Queue()

class MainHandler(tornado.web.RequestHandler):

    def get(self):
        self.render('index.html')

class GameHandler(tornado.web.RequestHandler):

    @tornado.web.asynchronous
    def get(self):
        scenario = self.get_argument('q')
        q.put((scenario, self))

application = tornado.web.Application([
    (r"/", MainHandler),
    (r"/game", GameHandler),
], static_path="static", debug=True)

if __name__ == "__main__":
    application.listen(8888)
    Thread(target=proxy, args=(q, )).start()
    try:
        tornado.ioloop.IOLoop.instance().start()
    except KeyboardInterrupt:
        STOP=True
        raise

