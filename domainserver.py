"""Environment, independent of genetic algorithm, that evaluates the fitness of
a candiate solution, in terms of how closely its observable characteristics 
(phenotype) match the desirable characteristics defined for the problem domain.
"""

import BaseHTTPServer
import SocketServer

PORT = 1831

class Status(object):
    Executed = 0
    Error = 1

class Handler(BaseHTTPServer.BaseHTTPRequestHandler):
   
    @classmethod
    def load_domain(cls, domain):
        cls.domain = {}
        exec(domain, None, cls.domain)

    def do_GET(self):
        code = "x="+self.path[1:]
        domain = self.domain.copy()
        try:
            exec(code, None, domain)
            x = domain.get('x')
            status = Status.Executed
        except:
            x = 0
            status = Status.Error
        response = "%s%s" % (status, x)
    
        self.send_response(200)
        self.send_header("Content-type", "text/plain")
        self.send_header("Content-Length", str(len(response)))
        self.end_headers()
        print response
        self.wfile.write(response)

DOMAIN = """
def foo(x, y):
    return x+y
x = 5
y = 1
z = 10
"""
Handler.load_domain(DOMAIN)
httpd = SocketServer.TCPServer(("", PORT), Handler)
httpd.serve_forever()
