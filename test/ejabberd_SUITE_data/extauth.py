import sys
import struct

def read():
    (pkt_size,) = struct.unpack('>H', sys.stdin.read(2))
    pkt = sys.stdin.read(pkt_size).split(':')
    cmd = pkt[0]
    args_num = len(pkt) - 1
    if cmd == 'auth' and args_num == 3:
        write(True)
    elif cmd == 'isuser' and args_num == 2:
        write(True)
    elif cmd == 'setpass' and args_num == 3:
        write(True)
    elif cmd == 'tryregister' and args_num == 3:
        write(True)
    elif cmd == 'removeuser' and args_num == 2:
        write(True)
    elif cmd == 'removeuser3' and args_num == 3:
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
