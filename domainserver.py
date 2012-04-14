import BaseHTTPServer
import SocketServer

PORT = 1831

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
        except:
            x = 0
        self.send_response(200)
        self.send_header("Content-type", "text/plain")
        self.send_header("Content-Length", str(len(str(x))))
        self.end_headers()
        self.wfile.write(x)

DOMAIN = """
def foo(x, y):
    return x+y
x = 5
y = 1
z = 10
"""
Handler.load_domain(DOMAIN)
httpd = SocketServer.TCPServer(("", PORT), Handler)

print "serving at port", PORT
httpd.serve_forever()
