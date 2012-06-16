from coas import AssemblerAnnotation
import struct

def mapping(words):
    return ''.join(["%s\t%s\n" % (k, v) for k, v in words.items()])

def binaryOutput(data, endian=">"):
    return ''.join([struct.pack(endian+"H", o) for o in data if not isinstance(o, AssemblerAnnotation)])

def intelHex(data):
    data = ''.join([struct.pack(">H", o) for o in bin if not isinstance(o, AssemblerAnnotation)])

    chunkSize = 32
    def chunk(c):
        for i in range(0,len(c),chunkSize):
            yield c[i:i+chunkSize]

    def format(address, rec_type, data = []):
        data = [len(data), address>>8, address & 0xFF, rec_type] + data
        data += [((sum(data) ^ 0xFF) + 1) & 0xFF]
        return ":" + (''.join(["%2X" % d for d in data])).replace(' ','0')

    for addr, block in enumerate(chunk([ord(c) for c in data])):
        yield format(addr * chunkSize, 0, block)
    yield format(0,1)

def datOutput(data):
    leadIn, limit, start = False, 0, 0
    for p, k in enumerate(data):
        if isinstance(k, AssemblerAnnotation):
            yield "; (%4x) %s" % (start, k.pos[3])
            leadIn = False
            continue 

        if not leadIn:
            yield "\nDAT "
            leadIn, limit, start = True, 0, p
        elif limit >= 8:
            yield "\nDAT "
            leadIn, limit = True, 0

        yield ("0x%4x" % k).replace(" ","0") + " "
        limit += 1

def verilog(data):
    for k in data:
        if isinstance(k, AssemblerAnnotation):
            yield "// %s\n" % k.pos[3]
        else:
            yield ("%4x" % k).replace(" ","0") + " "

