#! /usr/bin/env python

# gtnetcat - a grammatech version of netcat functionality
#            for sending data to a <address, port> pair>
#
#            Unlike netcat, gtnetcat is double-threaded,
#            handling writes on one thread and reads on
#            the other. This ensures that the utility
#            doesn't fail to consume messages from the
#            destination address while busy pushing (or
#            waiting to push) data to that destination.
#
#            gtnetcat is intended for use when submitting
#            test results to the datamanager.
#
#            usage: python gtnetcat.py HOST PORT FILE
#
import sys, socket
BLOCK = 4096
SECS_TO_TIMEOUT = 60 * 60 # allow an hour for heavy loads and slow machines

try:
    import asyncore
except Exception:
    asyncore = None

class RetryException(Exception):
    pass

def usage():
    sys.stderr.write('usage: %s HOST PORT FILE\n' % sys.argv[0])
    exit(1)

def readloop(sock, in_generator, res):
    try:
        try:
            for val in in_generator():
                sock.sendall(val)
            res[0] = True
        finally:
            sock.shutdown(socket.SHUT_WR)
    except Exception, e:
        res[0] = e

def writeloop(sock, out, res, rtres):
    try:
        try:
            # Stop receiving immediately if the sender thinks it's
            # done.
            window = ""
            while True:
                val = sock.recv(BLOCK)
                if val is None or len(val) == 0:
                    raise RetryException('Lost connection before request was finished')
                out.write(val)
                out.flush()
                window += val
                if "Not satisfied" in window: 
                    raise Exception('Request not satisfied')
                if "Satisfied" in window: 
                    if rtres[0] is False:
                        raise Exception('Datamanager reported request satisfied before full request was sent')
                    else:
                        break
                window = window[-32:]
            res[0] = True
        finally:
            try:
                # This can fail because the other end may have closed the
                # socket for us, which is expected.
                sock.shutdown(socket.SHUT_RD)
            except socket.error:
                pass
    except Exception, e:
        res[0] = e

def submit_sync(host, port, in_generator, out):
    # We can't have a timeout here or else the send will timeout,
    # which would be bad.
    sto = socket.getdefaulttimeout()
    socket.setdefaulttimeout(SECS_TO_TIMEOUT)
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    socket.setdefaulttimeout(sto)
    sock.connect((host,port))
    rt_res = [False]
    wt_res = [False]
    import threading
    rt = threading.Thread(target=readloop, args=[sock, in_generator, rt_res])
    wt = threading.Thread(target=writeloop, args=[sock, out, wt_res, rt_res])
    wt.start()
    rt.start()
    rt.join()
    wt.join()
    try:
        sock.close()
    except Exception:
        pass
    if rt_res[0] is True:
        pass
    elif rt_res[0] is False:
        raise Exception('Send thread failed')
    else:
        raise rt_res[0]
    if wt_res[0] is True:
        pass
    elif wt_res[0] is False:
        raise Exception('Receive thread failed')
    else:
        raise wt_res[0]
    return 0

if asyncore is not None:
    class GTNetCatClient(asyncore.dispatcher):
        def __init__(self, host, port, in_iterator, out, client_map):
            asyncore.dispatcher.__init__(self, map=client_map)
            self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
            self.connect( (host, port) )
            self.in_iterator = in_iterator
            self.out = out
            self.window = ''
            self.buffer = ''
            self._writable = True
            self._readable = True
            self.done = False

        def handle_connect(self):
            pass
        
        def handle_close(self):
            self.close()

        def shutdown_rd(self):
            self._readable = False
            try:
                # This can fail because the other end may have closed the
                # socket for us, which is expected.
                self.socket.shutdown(socket.SHUT_RD)
            except socket.error:
                pass
        
        def handle_read(self):
            val = self.recv(BLOCK)
            if val is None or len(val) == 0:
                raise RetryException('Lost connection before request was finished')
            self.out.write(val)
            self.out.flush()
            self.window += val
            if "Not satisfied" in self.window:
                self.shutdown_rd()
                raise Exception('Request not satisfied')
            if "Satisfied" in self.window:
                self.shutdown_rd()
                if self._writable:
                    raise Exception('Datamanager reported request satisfied before full request was sent')
                else:
                    self.done = True
                    self.close()
                    return
            self.window = self.window[-32:]
        
        def writable(self):
            return self._writable
        
        def readable(self):
            return self._readable
        
        def handle_write(self):
            if len(self.buffer) == 0:
                try:
                    self.buffer = self.in_iterator.next()
                except StopIteration:
                    self._writable = False
                    self.socket.shutdown(socket.SHUT_WR)
                    return
            # Windows does horrible things if you try to send a large
            # buffer, so never try more than 4k.
            sent = self.send(self.buffer[:BLOCK])
            self.buffer = self.buffer[sent:]

    def submit_async(host, port, in_generator, out):
        client_map = {}
        client = GTNetCatClient(host, port, in_generator(), out, client_map)
        try:
            asyncore.loop(60*60*24*7, False, client_map)
        finally:
            client.close()
        if not client.done:
            raise Exception('Request not satisfied: Connection terminated prematurely')
        return 0

    submit = submit_async
else:
    submit = submit_sync

def main(argv):
    if len(argv) != 4:
        usage()
        
    host, port, filename = argv[1:]
    try:
        port = int(port)
    except ValueError:
        usage()
    infile = open(filename, 'rb')
    def in_generator():
        while True:
            val = infile.read(BLOCK)
            if val is None or len(val) == 0:
                break
            yield val
    return submit(host, port, in_generator, sys.stdout)

if __name__ == "__main__":
    exit(main(sys.argv))
