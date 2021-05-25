import sys
import struct

def read_from_stdin(bytes):
  if hasattr(sys.stdin, 'buffer'):
    return sys.stdin.buffer.read(bytes)
  else:
    return sys.stdin.read(bytes)

def read():
    (pkt_size,) = struct.unpack('>H', read_from_stdin(2))
    pkt = sys.stdin.read(pkt_size)
    cmd = pkt.split(':')[0]
    if cmd == 'auth':
        u, s, p = pkt.split(':', 3)[1:]
        if u == "wrong":
            write(False)
        else:
            write(True)
    elif cmd == 'isuser':
        u, s = pkt.split(':', 2)[1:]
        if u == "wrong":
            write(False)
        else:
            write(True)
    elif cmd == 'setpass':
        u, s, p = pkt.split(':', 3)[1:]
        write(True)
    elif cmd == 'tryregister':
        u, s, p = pkt.split(':', 3)[1:]
        write(True)
    elif cmd == 'removeuser':
        u, s = pkt.split(':', 2)[1:]
        write(True)
    elif cmd == 'removeuser3':
        u, s, p = pkt.split(':', 3)[1:]
        write(True)
    else:
        write(False)
    read()

def write(result):
    if result:
        sys.stdout.write('\x00\x02\x00\x01')
    else:
        sys.stdout.write('\x00\x02\x00\x00')
    sys.stdout.flush()

if __name__ == "__main__":
    try:
        read()
    except struct.error:
        pass
