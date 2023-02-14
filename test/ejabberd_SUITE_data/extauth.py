"""extauth dummy script for ejabberd testing."""

import sys
import struct

def read_from_stdin(read_bytes):
    """Read buffer from standard input."""
    if hasattr(sys.stdin, 'buffer'):
        return sys.stdin.buffer.read(read_bytes)
    return sys.stdin.read(read_bytes)

def read():
    """Read input and process the command."""
    (pkt_size,) = struct.unpack('>H', read_from_stdin(2))
    pkt = sys.stdin.read(pkt_size)
    cmd = pkt.split(':')[0]
    if cmd == 'auth':
        user, _, _ = pkt.split(':', 3)[1:]
        if user == "wrong":
            write(False)
        else:
            write(True)
    elif cmd == 'isuser':
        user, _ = pkt.split(':', 2)[1:]
        if user == "wrong":
            write(False)
        else:
            write(True)
    elif cmd == 'setpass':
        user, _, _ = pkt.split(':', 3)[1:]
        write(True)
    elif cmd == 'tryregister':
        user, _, _ = pkt.split(':', 3)[1:]
        write(True)
    elif cmd == 'removeuser':
        user, _ = pkt.split(':', 2)[1:]
        write(True)
    elif cmd == 'removeuser3':
        user, _, _ = pkt.split(':', 3)[1:]
        write(True)
    else:
        write(False)
    read()

def write(result):
    """write result to standard output."""
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
